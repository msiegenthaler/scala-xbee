package ch.inventsoft.xbee

import ch.inventsoft.scalabase.communicationport._
import ch.inventsoft.scalabase.process._
import Messages._
import ch.inventsoft.scalabase.oip._
import ch.inventsoft.scalabase.log.Log
import XBeeParsing._
import ch.inventsoft.scalabase.extcol.ListUtil._


/**
 * LowLevelLocalXBee implementation using a XBee in API-mode attached to a communication port.
 */
class CommunicationPortLowLevelLocalXBeeInApiMode protected(port: CommunicationPort) extends LocalLowLevelXBee with StateServer[CPXBeeLowLevelState] with Log {
  type State = CPXBeeLowLevelState
  protected[this] override def initialState = {
    port.redirectIncomingTo(Some(process))
    CPXBeeLowLevelState(None, Nil)
  }
  protected[this] override def messageHandler(state: State) = {
    case DataReceived(this.port, data) =>
      log.trace("XBee: Received data fragement {}", byteListToHex(data))
      val buffer = state.buffer ++ data
      val afterBuffer = processBuffer { data =>
        log.debug("XBee: Received packet {}", byteListToHex(data))
        state.sendTo.foreach(_ ! ReceivedCommand(this, data))
      }(buffer)
      Some(state.withBuffer(afterBuffer))
  }
  
  private def processBuffer(handler: (Seq[Byte] => Unit))(buffer: Seq[Byte]): Seq[Byte] = {
    val sr = UnescapedPacket.skipInvalid(buffer)
    if (sr.wasDataSkipped) log.info("XBee: Dropped data {}", byteListToHex(sr.skip))
    sr.data match {
      case UnescapedPacket(data, rest) =>
        handler(data)
        processBuffer(handler)(rest)
      case data => data
    }
  }
  
  override def sendCommand(command: Seq[Byte]) = cast { state =>
    val packet = UnescapedPacket(command)
    // trace
    log.debug("XBee: Sending packet with payload: {}", byteListToHex(command)) 
    log.trace("XBee: Sending data {}", byteListToHex(packet))
    // eot
    port send packet.iterator
    state
  }
  
  override def setIncomingCommandProcessor(process: Option[Process]) = cast { state =>
    state.withSendTo(process)
  }

  override def close = cast_ { state =>
    log.debug("Closing")
    port.close
    None
  }
}
object CommunicationPortLowLevelLocalXBeeInApiMode extends SpawnableCompanion[CommunicationPortLowLevelLocalXBeeInApiMode] {
  def apply(communicationPort: CommunicationPort)(as: SpawnStrategy) = start(as) {
    new CommunicationPortLowLevelLocalXBeeInApiMode(communicationPort)
  }
}

case class CPXBeeLowLevelState(sendTo: Option[Process], buffer: Seq[Byte]) {
  def withSendTo(sendTo: Option[Process]) = CPXBeeLowLevelState(sendTo, buffer)
  def withBuffer(buffer: Seq[Byte]) = CPXBeeLowLevelState(sendTo, buffer)
}
