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
    
    override protected[this] def initialState = {
      State(None, Nil, FrameId(), None)
    }
    override protected[this] def messageHandler(state: State) = {
      case ReceivedCommand(this.lowLevel, data) =>
        state.multiplex.foreach(_ ! data)
        Some(state)
      case e: ProcessEnd => //one of the subprocesses terminated, remove it
        Some(state.removeMultiplex(e.process))
    }
    
    protected[this] def child[A](body: => A @processCps) = call_? { (state: State, reply: A => Unit) =>
      val p = spawnChild(Required) {
        val res = body
        reply(res)
      }
      Some(state.addMultiplex(p))
    }
    protected[this] def retry[A](retriesLeft: Int)(body: () => Option[A] @processCps): Option[A] @processCps = retriesLeft match {
      case 0 => None
      case retriesLeft =>
        val res = body()
        if (res.isDefined) res
        else retry(retriesLeft - 1)(body)
    }
    
    protected[this] def sendWithoutFrame(command: Command): Unit = {
      lowLevel sendCommand command(NoFrameId)
    }
    protected[this] def sendWithFrame_(command: Command): MessageSelector[FrameId] = call { state =>
      val frame = state.frame
      lowLevel sendCommand command(frame)
      (frame, state.nextFrame)
    }
    protected[this] def sendWithFrame(command: Command): Option[FrameId] @processCps = {
      receiveWithin(getFrameTimeout) { sendWithFrame_(command).option }
    }    

    override def address = call_? { (state, reply) =>
      def requestSLAddress(retryCount: Int, sh: XBeeAddress64_High): Option[XBeeAddress64] @processCps = retryCount match {
        case 0 => None
        case retryCount =>
          val frame = sendWithFrame(f => AT.SL_read(f))
          receiveWithin(localCommandTimeout) {
            case AT.SL_response(((`frame`, status), a), _) => if (status == AT.StatusOk) {
              //Got a valid response, now we got SL and SH
              Some(sh + a)
            } else requestSLAddress(retryCount - 1, sh)
            case Timeout => requestSLAddress(retryCount - 1, sh)
          }
      }
      def requestAddress(retryCount: Int): Option[XBeeAddress64] @processCps = retryCount match {
        case 0 => None
        case retryCount =>
          val frame = sendWithFrame(f => AT.SH_read(f))
          receiveWithin(localCommandTimeout) {
            case AT.SH_response(((`frame`, status), a), _) => if (status == AT.StatusOk) {
              //Got a valid response, now get SL
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
          val p = spawnChild(Required) {
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
      if (state.cachedAddress.isEmpty) state.withCachedAddress(Some(address))
      else state
    }

    override def alias = child {
      retry(3) { () =>
        val frame = sendWithFrame(f => AT.MY_read(f))
        receiveWithin(localCommandTimeout) {
          case AT.MY_response(((`frame`, status), a), _) =>
            val address = if (a == XBeeAddress16Disabled) None else Some(a)
            log.debug("The alias of the local xbee is {}", address)
            Some(address)
          case Timeout => 
            None
        }
      }.getOrElse {
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

    // override def sendTrackedPacket(to: XBeeAddress, data: Seq[Byte]) = {
    //   //TODO
    // }
    
    
    override def broadcastPacket(data: Seq[Byte]) = {
      val d =  data.take(maxDataPerPacket).toList
      log.debug("Broadcasting packet {}", byteListToHex(d))
      //TODO xbee actually supports tracking of broadcast sends, make use of that
      sendWithoutFrame(f => TX64(f, XBeeAddress64Broadcast, TransmitOption(true,true), d))
    }
    
    // def discover(timeout: Duration = 2500 ms) = {
    //   //TODO       
    // }
    
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
 
 
 
 /*
class LocalSeries1XBee protected(lowLevel: LocalLowLevelXBee) extends LocalXBee with StateServer[LocalSeries1XBeeState] {
  type State = LocalSeries1XBeeState
  type Command = Seq[Byte]
  type PreparedCommand = FrameId => Seq[Byte]
  implicit def stateToSomeState(state: State) = Some(state)
  
  override val maxDataPerPacket = 100

  override protected[this] def initialState = {
    val server = this
    lowLevel.setIncomingCommandProcessor(Some(process))
    //Query the address to have it cached afterwards and to check if stuff is working
    // (address let's us crash if it fails)  
    spawnChild(Required) { server.address }
    LocalSeries1XBeeState(None, FrameId(), Map(), None)
  }
  protected[this] override def messageHandler(state: State) = {
    case ReceivedCommand(this.lowLevel, data) =>
      Some(processIncomingCommand(state)(data))
  }  
  protected[this] def processIncomingCommand(state: State): PartialFunction[Seq[Byte],State] = {
    case RX64((address, signal, option, data),Nil) =>
      state.sendIncomingTo.foreach(_ ! XBeeDataPacket(this, address, Some(signal), option.addressBroadcast || option.panBroadcast, data))
      state
    case RX16((address, signal, option, data), Nil) =>
      state.sendIncomingTo.foreach(_ ! XBeeDataPacket(this, address, Some(signal), option.addressBroadcast || option.panBroadcast, data))
      state
    case result @ FrameIdCommand((_,frameId), rest) =>
      state.commandResultHandlers.get(frameId).filter(_.isDefinedAt(result)) match {
        case Some(handler) =>
          val (cr, newState) = handler(result)(state)
          cr match {
            case EndOfCommand => newState.removeCommand(frameId)
            case CommandContinues(next) => newState.changeCommand(frameId, next)
          }
        case None => state
      }
    case anything =>
      println(anything.toString)
      state
  }
  
  override def incomingMessageProcessor(processor: Option[Process]) = cast { state =>
    state.withSendIncomingTo(processor)
  }
  
  override def address = call_? { (state, reply) => state.address match {
    case Some(address) =>
      //set by cacheAddress
      reply(address)
      Some(state)
    case None =>
      object HighFailed
      object LowFailed
      val collector = spawnChild(Required) {
        val high = receiveWithin(5 seconds) {
          case high: XBeeAddress64_High => noop; Some(high)
          case HighFailed => noop; None
          case Timeout => noop; None
        }
        val n = high.flatMap_cps { high => 
          val low = receiveWithin(5 seconds) {
            case low: XBeeAddress64_Low => noop; Some(low)
            case LowFailed => noop; None
            case Timeout => noop; None
          }
          low.map(_ + high)
        }
	n match {
          case Some(address) =>
	    noop
            reply(address)
            cacheAddress(address)
          case None =>
            //let it crash, since this process was spawned as required
            // we'll take down our parent as well
            throw new RuntimeException("Could not get 64-bit address")
        }
      }
      executeCommands(state)(
        sendCommandWithFrame(f => AT.SH_read(f)) { frame => {
          case AT.SH_response(((`frame`,status),address), value) =>
            if (status == AT.StatusOk) collector ! address
            else collector ! HighFailed
            UnmodifyingEndOfCommand
        }},
        sendCommandWithFrame(f => AT.SL_read(f)) { frame => {
          case AT.SL_response(((`frame`,status),address), value) =>
            if (status == AT.StatusOk) collector ! address
            else collector ! LowFailed
            UnmodifyingEndOfCommand
        }}
      )
  }}
  protected[this] def cacheAddress(address: XBeeAddress64) = cast { state =>
    log.debug("Address of local xbee is {}", address)
    state.withAddress(address)
  }

  override def alias = call_? { (state, reply) =>
    sendCommandWithFrame(f => AT.MY_read(f)) { frame => {
      case AT.MY_response(((`frame`,status),a),rest) =>
        val address = if (status == AT.StatusOk) {
          if (a == XBeeAddress16Disabled) None
          else Some(a)
        } else None
        reply(address)
        UnmodifyingEndOfCommand
      case DroppedCommand =>
        reply(None)
        UnmodifyingEndOfCommand
    }}(state)
  }

  override def alias(address: Option[XBeeAddress16]) = cast { state =>
    val a = address.getOrElse(XBeeAddress16Disabled)
    log.debug("Setting alias address to {}", address)
    sendCommandNoFrame(AT.MY_set(NoFrameId, a))(state)
  }
  
  override def sendPacket(to: XBeeAddress, data: Seq[Byte]) = cast { state => 
    val d =  data.take(maxDataPerPacket).toList
    log.debug("Sending packet {} to {}", byteListToHex(d), to)
    to match {
    case to: XBeeAddress64 =>
      sendCommandNoFrame(TX64(NoFrameId, to, TransmitOptionNormal,d))(state)
    case to: XBeeAddress16 =>
      sendCommandNoFrame(TX16(NoFrameId, to, TransmitOptionNormal, d))(state)
    }
  }
  override def broadcastPacket(data: Seq[Byte]) = cast { state =>
    val d =  data.take(maxDataPerPacket).toList
    log.debug("Broadcasting packet {}", byteListToHex(d))
    //TODO Xbee actually supports tracking of broadcast sends
    sendCommandNoFrame(TX64(NoFrameId, XBeeAddress64Broadcast, TransmitOption(true,true), d))(state)
  }

  
  override def sendTrackedPacket(to: XBeeAddress, data: Seq[Byte]) = call_? { (state, reply) => 
    val d =  data.take(maxDataPerPacket).toList
    log.debug("Sending tracked packet {} to {}", byteListToHex(d), to)
    to match {
      case to: XBeeAddress64 =>
        sendCommandWithFrame(f => TX64(f, to, TransmitOptionNormal,d)) { frame => {
          case TX_status((`frame`,status),Nil) =>
            log.debug("Sending tracked packet to {} completed with result {}", to, status)
            reply(status)
            UnmodifyingEndOfCommand
        }}(state)
      case to: XBeeAddress16 =>
        sendCommandWithFrame(f => TX16(f, to, TransmitOptionNormal,d)) { frame => {
          case TX_status((`frame`,status),Nil) =>
            log.debug("Sending tracked packet to {} completed with result {}", to, status)
            reply(status)
            UnmodifyingEndOfCommand
        }}(state)
    }
  }
  
  override def discover(timeout: Duration = 2500 ms) = call_? { (state, reply) =>
    def ndHandler(frame: FrameId, soFar: List[DiscoveredXBeeDevice]): CommandResultHandler = {
      case AT.ND_node(((`frame`,status),a16,a64,signal,id),rest) =>
        val a16o = if (a16 == XBeeAddress16Disabled) None else Some(a16)
        val item = DiscoveredXBeeDevice(a64, a16o, Some(signal))
        (s: State) => (CommandContinues(ndHandler(frame, item :: soFar)), s)
      case AT.ND_end((`frame`,status),rest) =>
        log.debug("Discovered devices: {}", soFar)
        reply(soFar.reverse)
        UnmodifyingEndOfCommand
    }
  
    log.trace("Starting device discovery...")
    executeCommands(state)(
      sendCommandWithFrame(f => AT.NT(f, timeout)) { frame => {
        case AT.NT_response((`frame`,status),timeout) => UnmodifyingEndOfCommand
      }},
      sendCommandWithFrame(f => AT.ND(f)) { frame => ndHandler(frame, Nil) }
    )
  }
  
  override def close = cast_ { state =>
    log.debug("Closing")
    lowLevel.close
    None
  }
  
  protected[this] def executeCommands(state: State)(f: (State => Option[State])*): Option[State] = {
    val initial: Option[State] = Some(state)
    f.foldLeft(initial) { (s,fun) => s.flatMap(s => fun(s)) }
  }
    
  protected[this] def sendCommandWithFrame(command: FrameId => Command)(commandResultHandler: FrameId => CommandResultHandler): (State => Option[State]) = {
    def sendIt(state: State): Option[State] = {
      val (frameId, newState) = state.pushCommand(commandResultHandler)
      lowLevel sendCommand command(frameId)
      Some(newState)
    }
    sendIt _
  }
  protected[this] def sendCommandNoFrame(command: Command)(state: State): State = {
    lowLevel sendCommand command
    state
  }
}
object LocalXBeeCommands {
  type State = LocalSeries1XBeeState
  
  sealed trait ResultToCommand  
  object EndOfCommand extends ResultToCommand
  case class CommandContinues(next: CommandResultHandler) extends ResultToCommand

  val UnmodifyingEndOfCommand = (state: State) => (EndOfCommand, state)
  
  val DroppedCommand = Nil
  
  type CommandResultHandler = PartialFunction[Seq[Byte],State => (ResultToCommand,State)]
}

case class LocalSeries1XBeeState(address: Option[XBeeAddress64], frameId: FrameId, commandResultHandlers: Map[FrameId,CommandResultHandler], sendIncomingTo: Option[Process]) {
  def withFrameId(frameId: FrameId) = LocalSeries1XBeeState(address, frameId, commandResultHandlers, sendIncomingTo)
  def withCommandResultHandlers(commands: Map[FrameId,CommandResultHandler]) = LocalSeries1XBeeState(address, frameId, commands, sendIncomingTo)
  def withSendIncomingTo(sendIncomingTo: Option[Process]) = LocalSeries1XBeeState(address, frameId, commandResultHandlers, sendIncomingTo)
  def withAddress(address: XBeeAddress64) = LocalSeries1XBeeState(Some(address), frameId, commandResultHandlers, sendIncomingTo)

  def pushCommand(command: FrameId => CommandResultHandler): (FrameId,LocalSeries1XBeeState) = {
    //drop the previous command with the same frameId
    val newState = commandResultHandlers.get(frameId).map(f => 
      if (f.isDefinedAt(DroppedCommand)) f(DroppedCommand)(this)._2
      else this
    ).getOrElse(this)
    
    //add the new command
    val newHandlers = newState.commandResultHandlers.updated((newState.frameId), command(frameId))
    (frameId, newState.withCommandResultHandlers(newHandlers).withFrameId(frameId++))
  }
  
  def changeCommand(forFrame: FrameId, command: CommandResultHandler) = {
    val newCommands = commandResultHandlers.updated(forFrame, command)
    withCommandResultHandlers(newCommands)
  }
  
  def removeCommand(forFrame: FrameId) = {
    withCommandResultHandlers(commandResultHandlers - forFrame)
  }
}

object LocalSeries1XBee extends SpawnableCompanion[LocalSeries1XBee] {
  def apply(lowLevel: LocalLowLevelXBee)(as: SpawnStrategy) = start(as) {
    new LocalSeries1XBee(lowLevel)
  }
}
*/