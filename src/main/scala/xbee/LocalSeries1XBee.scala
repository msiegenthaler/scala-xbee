package ch.inventsoft.xbee

import ch.inventsoft.scalabase.communicationport._
import ch.inventsoft.scalabase.oip._
import ch.inventsoft.scalabase.process._
import cps.CpsUtils._
import ch.inventsoft.scalabase.time._
import ch.inventsoft.scalabase.extcol.ListUtil._
import Process._
import Messages._
import XBeeParsing._


/**
 * Locally attached (i.e. via USB) XBee Series 1.
 * This implementation bases on a LocalLowLevelXBee that takes care of the actual communication with
 * the XBee device.
 */
object LocalSeries1XBee extends SpawnableCompanion[LocalXBee with Spawnable] {
  def apply(lowLevel: LocalLowLevelXBee)(as: SpawnStrategy) = start(as) {
    new LocalSeries1XBee(lowLevel)
  }
 
  protected class LocalSeries1XBee protected(lowLevel: LocalLowLevelXBee) extends LocalXBee with StateServer[State] {
    protected type Command = FrameId => Seq[Byte]
    
    override val maxDataPerPacket = 100
    protected[this] val getFrameTimeout = 1 s
    protected[this] val localCommandTimeout = 5 s
    protected[this] val sendTimeout = 2 minutes
    
    override protected[this] def initialState = {
      lowLevel.setIncomingCommandProcessor(Some(process))
      spawnChild(NotMonitored) { 
        //prefetch address
        receiveWithin(5 s)(address.option)
      }
      State(None, Nil, FrameId(), None)
    }

    protected[this] override def messageHandler(state: State) = {
      case cmd @ ReceivedCommand(this.lowLevel, data) =>
        data match {
          case RX64((address, signal, option, data), Nil) =>
            noop
            log.debug("Received packet from {}: {} (signal: {}, options: {})", address, byteListToHex(data), signal, option)
            state.forwardTo.foreach(_ ! XBeeDataPacket(this, address, Some(signal), option.addressBroadcast || option.panBroadcast, data))
            Some(state)
          case RX16((address, signal, option, data), Nil) =>
            noop
            log.debug("Received packet from {}: {} (signal: {}, options: {})", address, byteListToHex(data), signal, option)
            state.forwardTo.foreach(_ ! XBeeDataPacket(this, address, Some(signal), option.addressBroadcast || option.panBroadcast, data))
            Some(state)
          case other =>
            noop
            state.multiplex.foreach { p => 
              p ! cmd
            }
            Some(state)
        }
      case end: ProcessEnd => //one of the subprocesses terminated, remove it
        end match {
          case ProcessExit(_) => () //normal termination
          case ProcessKill(p, proc, reason) => 
            if (proc == this.process) () //we killed it, ignore
            else throw new RuntimeException("Subprocess was killed: "+reason)
          case ProcessCrash(p, reason) => //crash occured, restart ourself
            throw reason
        }
        noop
        Some(state.removeMultiplex(end.process))
    }  
    protected[this] def child[A](body: => A @processCps) = call_? { (state: State, reply: A => Unit) =>
      val p = spawnChild(Monitored) {
        val res = body
        reply(res)
      }
      Some(state.addMultiplex(p))
    }
    
    protected[this] def sendWithoutFrame(command: Command): Unit = {
      lowLevel sendCommand command(NoFrameId)
    }
    protected[this] def sendWithFrame_(command: Command): MessageSelector[FrameId] = call { state =>
      val frame = state.frame
      lowLevel sendCommand command(frame)
      (frame, state.nextFrame)
    }
    protected[this] def sendWithFrame(command: Command): FrameId @processCps = {
      val f = receiveWithin(getFrameTimeout) { sendWithFrame_(command).option }
      f.getOrElse(throw new RuntimeException("Could not get a frame id"))
    }    

    override def address = call_? { (state, reply) =>
      def requestSLAddress(retryCount: Int, sh: XBeeAddress64_High): Option[XBeeAddress64] @processCps = retryCount match {
        case 0 => None
        case retryCount =>
          val frame = sendWithFrame(f => AT.SL_read(f))
          log.trace("Send SL request")
          receiveWithin(localCommandTimeout) {
            case ReceivedCommand(_, AT.SL_response(((`frame`, status), a), Nil)) => if (status == AT.StatusOk) {
              //Got a valid response, now we got SL and SH
              log.trace("Got SL response {}", a)
              Some(sh + a)
            } else requestSLAddress(retryCount - 1, sh)
            case Timeout => requestSLAddress(retryCount - 1, sh)
          }
      }
      def requestAddress(retryCount: Int): Option[XBeeAddress64] @processCps = retryCount match {
        case 0 => None
        case retryCount =>
          val frame = sendWithFrame(f => AT.SH_read(f))
          log.trace("Send SH request")
          receiveWithin(localCommandTimeout) {
            case ReceivedCommand(_, AT.SH_response(((`frame`, status), a), Nil)) => if (status == AT.StatusOk) {
              //Got a valid response, now get SL
              log.trace("Got SH response {}", a)
              requestSLAddress(retryCount, a)
            } else requestAddress(retryCount - 1)        
            case Timeout => requestAddress(retryCount - 1)
          }
      }
      state.cachedAddress match {
        case Some(address) =>
          reply(address)
          Some(state)
        case None =>
          val p = spawnChild(Monitored) {
            val address = requestAddress(3)
            address match {
              case Some(address) =>
                log.debug("The local xbee address (64-bit) is {}", address)
                cacheAddress(address)
                reply(address)
              case None =>
                log error "Could not get the 64-bit address of the local xbee"
                reply(XBeeAddress64(0))
            }
          }
          Some(state.addMultiplex(p))
      }
    }
    protected[this] def cacheAddress(address: XBeeAddress64) = cast { state =>
      log.trace("Caching local xbee address {}", address)
      if (state.cachedAddress.isEmpty) state.withCachedAddress(Some(address))
      else state
    }

    override def alias = child {
      def fetchAlias = {
        val frame = sendWithFrame(f => AT.MY_read(f))
        receiveWithin(localCommandTimeout) {
          case ReceivedCommand(_, AT.MY_response(((`frame`, status), a), Nil)) => Some(a)
          case Timeout => None
        }
      }
      def fetchWithRetry(left: Int): Option[XBeeAddress16] @processCps = left match {
        case 0 => None
        case left =>
          val res = fetchAlias
          if (res.isDefined) {
            noop
            res
          } else fetchWithRetry(left - 1)
      }

      val x = fetchWithRetry(3)
      x match { 
        case Some(a) =>
          val address = if (a == XBeeAddress16Disabled) None else Some(a)
          log.debug("The alias of the local xbee is {}", address)
          address
        case None =>
          log.error("Could not get the alias of the local xbee")
          None
      }
    }
    
    override def alias(alias: Option[XBeeAddress16]) = cast { state =>
      log.debug("Setting alias address to {}", alias)
      val a = alias.getOrElse(XBeeAddress16Disabled)
      sendWithoutFrame(f => AT.MY_set(f, a))
      //TODO use frame id to get confirmation?
      state
    }
    
    override def sendPacket(to: XBeeAddress, data: Seq[Byte]) = {
      val d =  data.take(maxDataPerPacket).toList
      log.debug("Sending untracket packet {} to {}", byteListToHex(d), to)
      to match {
        case to: XBeeAddress64 =>
          sendWithoutFrame(f => TX64(f, to, TransmitOptionNormal, d))
        case to: XBeeAddress16 =>
          sendWithoutFrame(f => TX16(f, to, TransmitOptionNormal, d))
      }
    }

    override def sendTrackedPacket(to: XBeeAddress, data: Seq[Byte]) = child {
      val d =  data.take(maxDataPerPacket).toList
      log.debug("Sending tracked packet {} to {}", byteListToHex(d), to)
      val frame = to match {
        case to: XBeeAddress64 => sendWithFrame(f => TX64(f, to, TransmitOptionNormal, d))
        case to: XBeeAddress16 => sendWithFrame(f => TX16(f, to, TransmitOptionNormal, d))
      }
      val result = receiveWithin(sendTimeout) {
        case ReceivedCommand(_, TX_status((`frame`, status), Nil)) =>
          status
        case Timeout =>
          TransmitStatusNoAckReceived
      }
      log.debug("Sending tracked packet to {} completed with result {}", to, result)
      result
    }
    
    
    override def broadcastPacket(data: Seq[Byte]) = {
      val d =  data.take(maxDataPerPacket).toList
      log.debug("Broadcasting packet {}", byteListToHex(d))
      //TODO xbee actually supports tracking of broadcast sends, make use of that
      sendWithoutFrame(f => TX64(f, XBeeAddress64Broadcast, TransmitOption(true,true), d))
    }
    
    def discover(timeout: Duration = 2500 ms) = child {
      def handle(frame: FrameId, soFar: List[DiscoveredXBeeDevice] = Nil): List[DiscoveredXBeeDevice] @processCps = {
        receiveWithin(timeout) {
          case ReceivedCommand(_, AT.ND_node(((`frame`, status),a16,a64,signal,id), Nil)) =>
            val a16o = if (a16 == XBeeAddress16Disabled) None else Some(a16)
            val item = DiscoveredXBeeDevice(a64, a16o, Some(signal))
            handle(frame, item :: soFar)
          case ReceivedCommand(_, AT.ND_end((`frame`, status),Nil)) =>
            soFar.reverse
        }
      }
      def setTimeout(retry: Int): Boolean @processCps = {
        val frame = sendWithFrame(f => AT.NT(f, timeout))
        receiveWithin(localCommandTimeout) {
          case ReceivedCommand(_, AT.NT_response(((frame, status), timeout), rest)) => 
            if (status == AT.StatusOk) {
              log.trace("Node discovery timeout set to {}", timeout)
              true
            } else if (retry > 0) setTimeout(retry - 1) else false
          case Timeout => if (retry > 0) setTimeout(retry - 1) else false
        }
      }
      
      log.trace("Discovering nodes")
      setTimeout(3)
      val frame = sendWithFrame(f => AT.ND(f))      
      val nodes = handle(frame)
      log.debug("Discovered devices: {}", nodes)
      nodes
    }
        
    def incomingMessageProcessor(processor: Option[Process]) = cast { state =>
      state.withForwardTo(processor)
    }

    override def close = cast_ { state =>
      log.debug("Closing")
      lowLevel.close
      None
    }
  }
  protected case class State(cachedAddress: Option[XBeeAddress64], multiplex: List[Process], frame: FrameId, forwardTo: Option[Process]) {
    def addMultiplex(process: Process) = withMultiplex(process :: multiplex)
    def removeMultiplex(process: Process) = withMultiplex(multiplex.filterNot(_ == process))
    def withMultiplex(multiplex: List[Process]) = State(cachedAddress, multiplex, frame, forwardTo)
    def withCachedAddress(cachedAddress: Option[XBeeAddress64]) = State(cachedAddress, multiplex, frame, forwardTo)
    def nextFrame = State(cachedAddress, multiplex, frame++, forwardTo)
    def withForwardTo(forwardTo: Option[Process]) = State(cachedAddress, multiplex, frame, forwardTo)
  }
}