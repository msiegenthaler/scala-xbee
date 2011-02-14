package ch.inventsoft
package xbee

import scalabase.binary.BytesParsing._
import scalabase.time._


/**
 * Byte-Representations of packets used to communicate with XBee-devices.
 */
object XBeeParsing {
  
  object UnescapedPacket extends FragmentClass[Seq[Byte]] {
    protected val preample = <<( fix_byte(0x7e), short ) drop1
      
    protected def calculateChecksum(of: Seq[Byte]): Byte = {
      (0xFF - (of.foldLeft(0)((a, b) => a + b) & 0xFF)).toByte
    }

    override val definition = <<( list_short_count(preample, byte), byte )>>>(
      (payload: Seq[Byte]) => {
        val checksum = calculateChecksum(payload)
        (payload, checksum)
      },
      t => {
        val (data, checksum) = t
        val expectedChecksum = calculateChecksum(data)
        if (checksum == expectedChecksum) Some(data)
        else None
      }
    )
    
    def skipInvalid(data: Seq[Byte]) = {
      def skipToNext0x7e(data: Seq[Byte]) = {
        data.span(_ != 0x7e)
      }
      def isLengthInvalid(data: Seq[Byte]) = {
        data.length > 1 && data(1) != 0x00
      }
      def isPacketCompleteButInvalid(data: Seq[Byte]) = data match {
        case preample((length), rest) =>
          if (rest.length > length) { //rest is data + checksum
            UnescapedPacket.unapply(data).isEmpty // checksum is wrong
          } else false
        case _ => false
      }

      def skip(data: Seq[Byte], skippedSoFar: Seq[Byte]): SkipResult = {
        val (skipped, d) = skipToNext0x7e(data)
        if (isLengthInvalid(d) || isPacketCompleteButInvalid(d)) {
          //invalid length or invalid checksum
          // - drop the first byte (not empty, else isLengthPossibilyValid would not return false)
          //   then go find the the next 0x7e
          val (first, d2) = d.splitAt(1) 
          val s = skippedSoFar ++ skipped ++ first
          skip(d2, s)
        } else {
          //possibly valid
          SkipResult(d, skippedSoFar ++ skipped)
        }
      }
      skip(data, Nil)
    }
  }
  
  case class SkipResult(data: Seq[Byte], skip: Seq[Byte]) {
    def wasDataSkipped = !skip.isEmpty
  }
  
  
  val frameId = byte.map(
        (frame: FrameId) => frame.value,
        t => Some(FrameId(t))
      )
  val address16 = short.map(
        (address: XBeeAddress16) => address.value,
        t => Some(XBeeAddress16(t))
      )
  val address64 = long.map(
        (address: XBeeAddress64) => address.value,
        t => Some(XBeeAddress64(t))
      )
  val address64_high = integer.map(
        (a: XBeeAddress64_High) => a.value,
        i => Some(XBeeAddress64_High(i))
      )
  val address64_low = integer.map(
        (a: XBeeAddress64_Low) => a.value,
        i => Some(XBeeAddress64_Low(i))
      )
      
  val signal_strength = byte.map(
        (s: SignalStrength) => (s.dBm * -1).toByte,
        v => Some(SignalStrength((v & 0xFF) * -1))
      )

  val FrameIdCommand = <<( byte, frameId )    
      
  object AT {
    /**
     * Response status to an AT command.
     */
    sealed trait Status {
      val isSuccess: Boolean
    }
    object StatusOk extends Status {
      override val isSuccess = true
    }
    object StatusError extends Status {
      override val isSuccess = false
    }
    object StatusInvalidCommand extends Status {
      override val isSuccess = false
    }
    object StatusInvalidParameter extends Status {
      override val isSuccess = false
    }
    object Status {
      private[AT] def apply(value: Byte) = value match {
        case 0 => StatusOk
        case 1 => StatusError
        case 2 => StatusInvalidCommand
        case 3 => StatusInvalidParameter
        case _ => throw new IllegalArgumentException
      }
      private[AT] def unapply(status: Status): Option[Byte] = status match {
        case StatusOk => Some(0)
        case StatusError => Some(1)
        case StatusInvalidCommand => Some(2)
        case StatusInvalidParameter => Some(3)
        case _ => None
      }
    }
    
    private def AtCommandPreample(c1: Char, c2: Char) = {
      <<( fix_byte(0x08), frameId, fix_byte(c1.toByte), fix_byte(c2.toByte) ).drop1.drop2.drop2
    }
    private val response_status = byte.map[Status](
          (status: Status) => status match {
            case Status(value) => value
            case _ => 0xFF.toByte
          },
          t => {
            try {
              Some(Status(t))
            } catch {
              case e: IllegalArgumentException => None
            }
          }
        )
    private def AtResponsePreample(c1: Char, c2: Char) = {
      <<( fix_byte(0x88.toByte), frameId, fix_byte(c1.toByte), fix_byte(c2.toByte), response_status ).drop1.drop2.drop2
    }

    /**
     * Set the 16 bit address of the XBee
     */
    val MY_set = <<( AtCommandPreample('M','Y'), address16 )>>
    val MY_read = AtCommandPreample('M','Y')
    val MY_response = <<( AtResponsePreample('M', 'Y'), address16 )>>
    
    /**
     * Reads the higher 32 bit of the devices 64-bit address.
     */
    val SH_read = AtCommandPreample('S', 'H')
    val SH_response = <<( AtResponsePreample('S','H'), address64_high )>>

    /**
     * Reads the lower 32 bit of the devices 64-bit address.
     */
    val SL_read = AtCommandPreample('S', 'L')
    val SL_response = <<( AtResponsePreample('S','L'), address64_low )>>
    
    /**
     * Set the high 32 bit of the destination address. Always use together with DL.
     */
    val DH_set = <<( AtCommandPreample('D','H'), address64_high )>>
    val DH_read = AtCommandPreample('D','H')
    val DH_response = <<( AtResponsePreample('D', 'H'), address64_high )>>

    /**
     * Set the lower 32 bit of the destination address. Always use together with DH.
     */
    val DL_set = <<( AtCommandPreample('D','L'), address64_low )>>
    val DL_read = AtCommandPreample('D','L')
    val DL_response = <<( AtResponsePreample('D','L'), address64_low )>>
    
    /**
     * Node discover.
     */
    val ND = AtCommandPreample('N','D')
    val ND_node = <<( AtResponsePreample('N','D'), address16, address64, signal_strength, null_terminated_ascii_string )>>
    val ND_end = <<( AtResponsePreample('N','D') )>>
    
    /**
     * Node discover timeout.
     */
    private val time = byte.map[Duration](
        (v: Duration) => {
          val units = v.amountAs(Milliseconds) / 100
          units.toByte
        },
        t => Some(Duration((t & 0xFF) * 100, Milliseconds))
      )
    val NT = <<( AtCommandPreample('N','T'), time )>>
    val NT_response = <<( AtResponsePreample('N','T'), time )>>
    
    //TODO more AT commands
  }
  
  case class TransmitOption(val ackDisabled: Boolean, val sendWithBroadcastPanId: Boolean) {
    def withAckDisabled(v: Boolean) = TransmitOption(v, sendWithBroadcastPanId)
    def withBroadcastPanId(v: Boolean) = TransmitOption(ackDisabled, v)
  }
  object TransmitOptionNormal extends TransmitOption(false, false)
  
  object TransmitStatus {
    private[XBeeParsing] def apply(value: Byte): TransmitStatus = value match {
      case 0 => TransmitStatusSuccess
      case 1 => TransmitStatusNoAckReceived
      case 2 => TransmitStatusCCAFailure
      case 3 => TransmitStatusPurged
    }
    private[XBeeParsing] def unapply(status: TransmitStatus): Option[Byte] = status match {
      case TransmitStatusSuccess => Some(0)
      case TransmitStatusNoAckReceived => Some(1)
      case TransmitStatusCCAFailure => Some(2)
      case TransmitStatusPurged => Some(3)
    }
  }
  
  private val transmitOption = bit_byte.map(
        (o: TransmitOption) => {
          (false, false, false, false, false, o.sendWithBroadcastPanId, false, o.ackDisabled)
        },
        t => {
          Some(TransmitOption(t._8, t._6))
        }
      )
  private val transmitStatus = byte.map(
        (s: TransmitStatus) => s match {
          case TransmitStatus(v) => v
          case _ => 0xFF.toByte
        },
        t => Some(TransmitStatus(t))
      )
  
  /**
   * Transmit data to a remote device using a 64-bit address.
   */
  val TX64 = <<( fix_byte(0x00), frameId, address64, transmitOption, list_to_end(byte)) drop1
  /**
   * Transmit data to a remote device using a 16-bit address.
   */
  val TX16 = <<( fix_byte(0x01), frameId, address16, transmitOption, list_to_end(byte)) drop1
  /**
   * Transmit status (result). Answer to a TX64 or TX16, is sent when the TX is completed.
   */
  val TX_status = <<( fix_byte(0x89.toByte), frameId, transmitStatus) drop1
  
  
  case class ReceiveOption(addressBroadcast: Boolean, panBroadcast: Boolean)
  
  private val receiveOption = bit_byte.map(
        (o: ReceiveOption) => {
          (false, false, false, false, false, o.panBroadcast, o.addressBroadcast, false)
        },
        t => {
          Some(ReceiveOption(t._7, t._6))
        }
      )
  
  /**
   * Received data from a remote device with a 64-bit address.
   */
  val RX64 = <<( fix_byte(0x80.toByte), address64, signal_strength, receiveOption, list_to_end(byte)) drop1
  /**
   * Received data from a remote device with a 16-bit address.
   */
  val RX16 = <<( fix_byte(0x81.toByte), address16, signal_strength, receiveOption, list_to_end(byte)) drop1

  
  
  
  class NullTerminatedBytes extends ListFragment[Byte] {
    override val element = byte
    override protected def shouldAbort(bytes: Seq[Byte], soFar: List[Byte]) =
      bytes.isEmpty || soFar.headOption == 0 
  }
  def null_terminated_string(encoding: String) = new NullTerminatedBytes().map(
    (string: String) => {
      val bytes = string.getBytes(encoding).toList 
      bytes ::: List(0.toByte)
    },
    bytes => {
      Some(new String(bytes.takeWhile(_ != 0).toArray, encoding))
    })
  val null_terminated_ascii_string = null_terminated_string("ISO-8859-1")
}
