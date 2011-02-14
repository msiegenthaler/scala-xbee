package ch.inventsoft
package xbee

import scalabase.io._
import scalabase.oip._
import scalabase.process._
import scalabase.time._
import scalabase.extcol.ListUtil._
import Messages._
import XBeeParsing._


/**
 * Locally attached (i.e. via USB) XBee Series 1.
 * This implementation bases on a LocalLowLevelXBee that takes care of the actual communication with
 * the XBee device.
 */
trait LocalSeries1XBee extends LocalXBee with StateServer {
  override val maxDataPerPacket = 100
  protected val getFrameTimeout = 1 s
  protected val localCommandTimeout = 5 s
  protected val sendTimeout = 2 minutes

  protected type State = S1State
  protected case class S1State(cachedAddress: Option[XBeeAddress64], multiplex: List[Process], frame: FrameId, incomingHandler: IncomingHandler, lowLevel: LocalLowLevelXBee, reader: Process) {
    def addMultiplex(process: Process) = copy(multiplex=process :: multiplex)
    def removeMultiplex(process: Process) = copy(multiplex=multiplex.filterNot(_ == process))
    def nextFrame = copy(frame=frame++)
  }
  protected case class LowLevelCommand(command: Command)
  protected type FramedCommand = FrameId => Seq[Byte]
  protected type IncomingHandler = ReceivedXBeeDataPacket => Unit @process

  protected def openLowLevel: LocalLowLevelXBee @process
  override protected def init = {
    val lowLevel = ResourceManager[LocalLowLevelXBee](openLowLevel, _.close).receive.resource
    val reader = spawnChild(Required)(readFromLowLevel(lowLevel))
    spawnChild(NotMonitored) { 
      //prefetch address
      address
    }
    S1State(None, Nil, FrameId(), _ => noop, lowLevel, reader)
  }
  /** Reader fun, that forwards all data read from the lowLevel */
  protected def readFromLowLevel(lowLevel: LocalLowLevelXBee): Unit @process = {
    val read = lowLevel.readWithin(1 s)
    read match {
      case Some(Data(items)) =>
        items.foreach_cps { cmd =>
          log.trace("Received LowLevel command {}", byteListToHex(cmd))
          val c = LowLevelCommand(cmd)
          process ! c
        }
        receiveNoWait {
          case Timeout => readFromLowLevel(lowLevel) //loop
          case Terminate => noop // end
        }
      case Some(EndOfData) =>
        log.trace("Received EndOfData")
        noop
      case None => //timeout
        receiveNoWait {
          case Timeout => readFromLowLevel(lowLevel) //loop
          case Terminate => noop // end
        }
    }
  }
  protected override def handler(state: State) = super.handler(state).orElse_cps {
    case llc @ LowLevelCommand(command) => command match {
      case RX64((address, signal, option, data), Nil) =>
        log.debug("Received packet from {}: {} (signal: {}, options: {})",
                  address, byteListToHex(data), signal, option)
        val bc = option.addressBroadcast || option.panBroadcast
        val packet = ReceivedXBeeDataPacket(address, Some(signal), bc, data)
        state.incomingHandler(packet)
        Some(state)
      case RX16((address, signal, option, data), Nil) =>
        log.debug("Received packet from {}: {} (signal: {}, options: {})",
                  address, byteListToHex(data), signal, option)
        val bc = option.addressBroadcast || option.panBroadcast
        val packet = ReceivedXBeeDataPacket(address, Some(signal), bc, data)
        state.incomingHandler(packet)
        Some(state)
      case other =>
        state.multiplex.foreach_cps(_ ! llc)
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
  protected override def termination(state: State) = {
    log.debug("Closing the XBee")
  }

  protected def child[A](body: => A @process): Selector[A] @process = {
    this ! new ModifyStateMessage with MessageWithSimpleReply[A] {
      override def execute(state: State) = {
        val p = spawnChild(Monitored) {
          reply(body)
        }
        state.addMultiplex(p)
      }
    }
  }
    
  protected def sendWithoutFrame(command: FramedCommand): Unit @process = cast { state =>
    state.lowLevel.writeCast(command(NoFrameId))
    state
  }
  protected def sendWithFrame_(command: FramedCommand): Selector[FrameId] @process = call { state =>
    val frame = state.frame
    state.lowLevel writeCast command(frame)
    (frame, state.nextFrame)
  }
  protected def sendWithFrame(command: FramedCommand): FrameId @process = {
    val f = receiveWithin(getFrameTimeout) { sendWithFrame_(command).option }
    f.getOrElse(throw new RuntimeException("Could not get a frame id"))
  }

  override def setMessageHandler(handler: ReceivedXBeeDataPacket => Unit @process) = cast { state =>
    state.copy(incomingHandler = handler)
  }

  override def address = {
    def requestSLAddress(retryCount: Int, sh: XBeeAddress64_High): Option[XBeeAddress64] @process = {
      retryCount match {
        case 0 => None
        case retryCount =>
          val frame = sendWithFrame(f => AT.SL_read(f))
          log.trace("Send SL request")
          receiveWithin(localCommandTimeout) {
            case LowLevelCommand(AT.SL_response(((`frame`, status), a), Nil)) => if (status == AT.StatusOk) {
              //Got a valid response, now we got SL and SH
              log.trace("Got SL response {}", a)
              Some(sh + a)
            } else requestSLAddress(retryCount - 1, sh)
            case Timeout =>
              log.trace("No response to SL within timout")
              requestSLAddress(retryCount - 1, sh)
            case other =>
println("### "+other); None
          }
      }
    }
    /** request the 64-bit address (sh&sl) */
    def requestAddress(retryCount: Int): Option[XBeeAddress64] @process = {
      retryCount match {
        case 0 => None
        case retryCount =>
          val frame = sendWithFrame(f => AT.SH_read(f))
          log.trace("Send SH request")
          receiveWithin(localCommandTimeout) {
            case LowLevelCommand(AT.SH_response(((`frame`, status), a), Nil)) => if (status == AT.StatusOk) {
              //Got a valid response, now get SL
              log.trace("Got SH response {}", a)
              requestSLAddress(retryCount, a)
            } else requestAddress(retryCount - 1)
            case Timeout =>
              log.trace("No response to SH within timout")
              requestAddress(retryCount - 1)
          }
      }
    }

    this ! new ModifyStateMessage with MessageWithSimpleReply[XBeeAddress64] {
      override def execute(state: State) = {
        state.cachedAddress match {
          case Some(address) =>
            reply(address)
            state
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
            state.addMultiplex(p)
        }
      }
    }
  }

  protected def cacheAddress(address: XBeeAddress64) = cast { state =>
    if (state.cachedAddress.isEmpty) { 
      log.trace("Caching local xbee address {}", address)
      state.copy(cachedAddress = Some(address))
    } else state
  }

  override def alias = child {
    def fetchAlias = {
      val frame = sendWithFrame(f => AT.MY_read(f))
      receiveWithin(localCommandTimeout) {
        case LowLevelCommand(AT.MY_response(((`frame`, status), a), Nil)) => Some(a)
        case Timeout => None
      }
    }
    def fetchWithRetry(left: Int): Option[XBeeAddress16] @process = left match {
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

  override def alias(alias: Option[XBeeAddress16]) = child {
    log.debug("Setting alias address to {}", alias)
    val a = alias.getOrElse(XBeeAddress16Disabled)
    val frameId = sendWithFrame(f => AT.MY_set(f, a))
    //TODO wait for confirmation
    ()
  }

  override def send(to: XBeeAddress, data: Seq[Byte]) = {
    val d =  data.take(maxDataPerPacket).toList
    log.debug("Sending untracket packet {} to {}", byteListToHex(d), to)
    to match {
      case to: XBeeAddress64 =>
        sendWithoutFrame(f => TX64(f, to, TransmitOptionNormal, d))
      case to: XBeeAddress16 =>
        sendWithoutFrame(f => TX16(f, to, TransmitOptionNormal, d))
    }
  }
  override def sendTracked(to: XBeeAddress, data: Seq[Byte]) = child {
    val d =  data.take(maxDataPerPacket).toList
    log.debug("Sending tracked packet {} to {}", byteListToHex(d), to)
    val frame = to match {
      case to: XBeeAddress64 => sendWithFrame(f => TX64(f, to, TransmitOptionNormal, d))
      case to: XBeeAddress16 => sendWithFrame(f => TX16(f, to, TransmitOptionNormal, d))
    }
    val result = receiveWithin(sendTimeout) {
      case LowLevelCommand(TX_status((`frame`, status), Nil)) =>
        status
      case Timeout =>
        TransmitStatusNoAckReceived
    }
    log.debug("Sending tracked packet to {} completed with result {}", to, result)
    result
  }
  override def broadcast(data: Seq[Byte]) = {
    val d =  data.take(maxDataPerPacket).toList
    log.debug("Broadcasting packet {}", byteListToHex(d))
    sendWithoutFrame(f => TX64(f, XBeeAddress64Broadcast, TransmitOption(true,true), d))
  }
  
  override def discover(timeout: Duration = 2500 ms) = child {
    def handle(frame: FrameId, soFar: List[DiscoveredXBeeDevice] = Nil): List[DiscoveredXBeeDevice] @process = {
      receiveWithin(timeout) {
        case LowLevelCommand(AT.ND_node(((`frame`, status),a16,a64,signal,id), Nil)) =>
          val a16o = if (a16 == XBeeAddress16Disabled) None else Some(a16)
          val item = DiscoveredXBeeDevice(a64, a16o, Some(signal))
          handle(frame, item :: soFar)
        case LowLevelCommand(AT.ND_end((`frame`, status),Nil)) =>
          soFar.reverse
      }
    }
    def setTimeout(retries: Int = 3): Boolean @process = {
      val frame = sendWithFrame(f => AT.NT(f, timeout))
      receiveWithin(localCommandTimeout) {
        case LowLevelCommand(AT.NT_response(((frame, status), timeout), rest)) => 
          if (status == AT.StatusOk) {
            log.trace("Node discovery timeout set to {}", timeout)
            true
          } else if (retries > 0) setTimeout(retries - 1) else false
        case Timeout => if (retries > 0) setTimeout(retries - 1) else false
      }
    }
    log.trace("Discovering nodes")
    setTimeout()
    val frame = sendWithFrame(f => AT.ND(f))      
    val nodes = handle(frame)
    log.debug("Discovered devices: {}", nodes)
    nodes
  }

  override def close = stopAndWait
}


object LocalSeries1XBee {
  def apply(lowLevel: => LocalLowLevelXBee @process, as: SpawnStrategy = SpawnAsRequiredChild) = {
    val xbee = new LocalSeries1XBee {
      override def openLowLevel = lowLevel
    }
    Spawner.start(xbee, as)
  }
} 
    
