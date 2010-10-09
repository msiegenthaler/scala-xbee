package ch.inventsoft
package serialcommunication

import java.io.{InputStream,OutputStream}
import scalabase.io._
import scalabase.time._
import scalabase.process._
import scalabase.oip._


/**
 * Description of a serial port
 */
trait SerialPortDescription {
  def name: String
  def open(baudRate: Int)(as: SpawnStrategy): CommunicationPort[Byte,Byte] @process
  override def toString = "SerialPort "+name
  override def equals(other: Any) = other match {
    case port: SerialPortDescription => port.name == name 
    case _ => false
  }
}

object SerialPort {
  /**
   * List all available serial ports.
   */
  def list: List[SerialPortDescription] = {
    def collectEnum[A](e: java.util.Enumeration[A], soFar: List[A]): List[A] = {
      if (e.hasMoreElements) collectEnum(e, e.nextElement :: soFar)
      else soFar.reverse
    }
    val enum = gnu.io.CommPortIdentifier.getPortIdentifiers.asInstanceOf[java.util.Enumeration[gnu.io.CommPortIdentifier]]
    collectEnum(enum, Nil).map(id => new SerialPortDescriptionImpl(id))
  }

  /**
   * Serial port with the given name.
   */
  def forName(name: String) =
    list.filter(_.name == name).headOption  
   
  private class SerialPortDescriptionImpl(identifier: gnu.io.CommPortIdentifier) extends SerialPortDescription {
    override val name = identifier.getName
    val timeout = 5 s
    override def open(baudRate: Int)(as: SpawnStrategy): CommunicationPort[Byte,Byte] @process = {
      val comPort = CommunicationPort[Byte,Byte,SerialPortState](
        open = {
          val p = identifier.open("Scala", timeout.amount(Milliseconds).toInt).asInstanceOf[gnu.io.SerialPort]

          import gnu.io.SerialPort._
          p.setFlowControlMode(FLOWCONTROL_NONE)
          p.setSerialPortParams(baudRate, DATABITS_8, STOPBITS_1, PARITY_NONE)

          val src = InputStreamSource(p.getInputStream)
          val snk = OutputStreamSink(p.getOutputStream)
          SerialPortState(p, src, snk)
        },
        close = { (sps: SerialPortState) =>
          val s1 = sps.source.close
          val s2 = sps.sink.close
          s1.await; s2.await
          sps.port.close
        },
        as = as
      )
      comPort.receive
    }
  }

  private case class SerialPortState(port: gnu.io.SerialPort, source: Source[Byte], sink: Sink[Byte])
}

