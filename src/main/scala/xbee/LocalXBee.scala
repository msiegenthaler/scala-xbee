package ch.inventsoft
package xbee

import scalabase.time._
import scalabase.process._
import Messages._
import XBeeParsing._


/**
 * An XBee module directly attached to this machine.
 */
trait LocalXBee {
  /**
   * Device address of the attached xbee 
   */
  def address: Selector[XBeeAddress64] @process
  /**
   * The alias address of the attached xbee (16-bit addressing)
   */
  def alias: Selector[Option[XBeeAddress16]] @process
  /**
   * Sets the alias address of the attached xbee (16-bit addressing)
   */
  def alias(alias: Option[XBeeAddress16]): Selector[Unit] @process
  
  /**
   * Maximum number of bytes that can be transmitted as one packet.
   */
  val maxDataPerPacket: Int
  
  /**
   * Sends data to a remote XBee.
   * Does not check if the transmission was successful or if the remote xbee exists.
   * The data is truncated if it exceeds maxDataPerPacket. 
   */
  def send(to: XBeeAddress, data: Seq[Byte]): Unit @process
  /**
   * Sends data to a remote XBee and tracks whether the transmission was successful. 
   * The data is truncated if it exceeds maxDataPerPacket. 
   */
  def sendTracked(to: XBeeAddress, data: Seq[Byte]): Selector[TransmitStatus] @process
  /**
   * Broadcasts data to all XBees on the same PAN. 
   * The data is truncated if it exceeds maxDataPerPacket. 
   */
  def broadcast(data: Seq[Byte]): Unit @process
  
  /**
   * Discover all xbees on the same PAN. 
   */
  def discover(timeout: Duration = 2500 ms): Selector[List[DiscoveredXBeeDevice]] @process
  
  /**
   * Sets the handler for messages sent to this xbee.
   * The handler is executed in the xbee's process, so it should be safe and short. In
   * most cases it should just send a message to another process.
   */
  def setMessageHandler(handler: ReceivedXBeeDataPacket => Unit @process): Unit @process
  
  /**
   * Close the communication to the xbee. This instance is no longer usable after calling close.
   */
  def close: Completion @process
}

/**
 * A packet sent to the local xbee.
 */
case class ReceivedXBeeDataPacket(sender: XBeeAddress,
                                  signalStrength: Option[SignalStrength],
                                  broadcast: Boolean,
                                  data: Seq[Byte]) {
  def length = data.size
  override def toString = sender.toString + ": "+data.map(_.toInt.toHexString+" ")
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
  /** @return [0,100] */
  def asPercent: Int = {
    if (dBm < -113) 0
    else if (dBm < -108) 5
    else if (dBm < -103) 10
    else if (dBm < -97) 15
    else if (dBm < -92) 20
    else if (dBm < -87) 25
    else if (dBm < -82) 30
    else if (dBm < -77) 35
    else if (dBm < -70) 40
    else if (dBm < -64) 45
    else if (dBm < -58) 50
    else if (dBm < -50) 55
    else if (dBm < -47) 60
    else if (dBm < -43) 65
    else if (dBm < -49) 70
    else if (dBm < -33) 75
    else if (dBm < -27) 80
    else if (dBm < -20) 85
    else if (dBm < -15) 90
    else if (dBm < -10) 95
    else 100
  }
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
