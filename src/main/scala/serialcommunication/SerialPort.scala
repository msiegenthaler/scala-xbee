package ch.inventsoft.serialcommunication

import ch.inventsoft.scalabase.communicationport._
import ch.inventsoft.scalabase.time._
import ch.inventsoft.scalabase.process._
import ch.inventsoft.scalabase.oip._
import java.io.{InputStream,OutputStream}


/**
 * Description of a serial port
 */
trait SerialPortDescription {
  def name: String
  def open(baudRate: Int)(as: SpawnStrategy): CommunicationPort @processCps
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
   
  private class SerialPortDescriptionImpl(identifier: gnu.io.CommPortIdentifier) extends SerialPortDescription with SpawnableCompanion[SerialPortImpl] {
    override val name = identifier.getName
    override def open(baudRate: Int)(as: SpawnStrategy): CommunicationPort @processCps = start(as) {
      new SerialPortImpl(identifier, baudRate)
    }
  }
  
  private class SerialPortImpl(identifier: gnu.io.CommPortIdentifier, baudRate: Int) extends IOStreamPort[Process] {
    protected[this] override def openStreams: (InputStream, OutputStream, Process) @processCps = {
      val timeout = 5 s
      val port = identifier.open("Scala", timeout.amount(Milliseconds).toInt).asInstanceOf[gnu.io.SerialPort]
      val kid = spawnChild(Required) {
        ch.inventsoft.scalabase.process.receive {
          case Terminate => port.close
        }
      }
      port.setFlowControlMode(gnu.io.SerialPort.FLOWCONTROL_NONE)
      port.setSerialPortParams(baudRate, gnu.io.SerialPort.DATABITS_8, gnu.io.SerialPort.STOPBITS_1, gnu.io.SerialPort.PARITY_NONE)
      (port.getInputStream, port.getOutputStream, kid)
    }
    protected[this] override def closeAdditionalResource(kid: Process) = 
      kid ! Terminate
  }
}

