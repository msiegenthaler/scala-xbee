package ch.inventsoft.xbee

import ch.inventsoft.scalabase.time._
import ch.inventsoft.scalabase.process._
import Messages._
import XBeeParsing._


/**
 * An XBee module directly attached to this machine.
 */
trait LocalXBee {
  /**
   * Device address of the attached xbee 
   */
  def address: MessageSelector[XBeeAddress64]
  /**
   * The alias address of the attached xbee (16-bit addressing)
   */
  def alias: MessageSelector[Option[XBeeAddress16]]
  /**
   * Sets the alias address of the attached xbee (16-bit addressing)
   */
  def alias(alias: Option[XBeeAddress16]): Unit
  
  /**
   * Maximum number of bytes that can be transmitted as one packet.
   */
  val maxDataPerPacket: Int
  
  /**
   * Sends data to a remote XBee.
   * Does not check if the transmission was successful or if the remote xbee exists.
   * The data is truncated if it exceeds maxDataPerPacket. 
   */
  def sendPacket(to: XBeeAddress, data: Seq[Byte]): Unit
  /**
   * Sends data to a remote XBee and tracks whether the transmission was successful. 
   * The data is truncated if it exceeds maxDataPerPacket. 
   */
  def sendTrackedPacket(to: XBeeAddress, data: Seq[Byte]): MessageSelector[TransmitStatus]
  /**
   * Broadcasts data to all XBees on the same PAN. 
   * The data is truncated if it exceeds maxDataPerPacket. 
   */
  def broadcastPacket(data: Seq[Byte]): Unit
  
  /**
   * Discover all xbees on the same PAN. 
   */
  def discover(timeout: Duration = 2500 ms): MessageSelector[List[DiscoveredXBeeDevice]]
  
  /**
   * Sets the process that receives incoming messages from the XBee module.
   * The messages are sent as XBeeDataPacket instances.
   */
  def incomingMessageProcessor(processor: Option[Process]): Unit
  
  /**
   * Close the communication to the xbee. This instance is no longer usable after calling close.
   */
  def close: Unit
}

/**
 * A packet received from another XBee. 
 */
case class XBeeDataPacket(receivedBy: LocalXBee, source: XBeeAddress, signalStrength: Option[SignalStrength], broadcast: Boolean, data: Seq[Byte]) {
  def length = data.size
  override def toString = source.toString + ": "+data.map(_.toInt.toHexString+" ")
}

/**
 * Result of a discovery. Represents a XBee.
 */
case class DiscoveredXBeeDevice(address64: XBeeAddress64, address16: Option[XBeeAddress16], signalStrength: Option[SignalStrength])

/**
 * Strength of a received signal.
 */
case class SignalStrength(dBm: Int) extends Ordered[SignalStrength] {
  def isStrongerThan(other: SignalStrength) = this > other
  override def compare(other: SignalStrength) = dBm compare other.dBm 
  override def toString = dBm.toString+"dBm"
}

/**
 * Status/result of a transmission.
 */
sealed trait TransmitStatus {
  val isSuccess: Boolean
}
object TransmitStatusSuccess extends TransmitStatus{
  override val isSuccess = true
}
object TransmitStatusNoAckReceived extends TransmitStatus{
  override val isSuccess = false
}
object TransmitStatusCCAFailure extends TransmitStatus{
  override val isSuccess = false
}
object TransmitStatusPurged extends TransmitStatus {
  override val isSuccess = false
}
