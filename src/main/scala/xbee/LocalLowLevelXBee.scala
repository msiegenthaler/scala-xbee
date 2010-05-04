package ch.inventsoft.xbee

import ch.inventsoft.scalabase.process._
import Messages._
import LocalLowLevelXBee._


/**
 * Low-level interface onto a local xbee. Allows to send/receive parsed packets (with
 * checksum etc.).
 */
trait LocalLowLevelXBee {
  /**
   * Sends a command to the xbee.
   */
  def sendCommand(command: Command): Unit
  
  /**
   * Sends responses from the xbee to the process (as ReceivedCommand's).
   */
  def setIncomingCommandProcessor(process: Option[Process]): Unit
  
  /**
   * Closes the communication to the xbee, also closes the underlying infrastructure.
   */
  def close: Unit
}

case class ReceivedCommand(from: LocalLowLevelXBee, bytes: Command)

object LocalLowLevelXBee {
  type Command = Seq[Byte]
}

