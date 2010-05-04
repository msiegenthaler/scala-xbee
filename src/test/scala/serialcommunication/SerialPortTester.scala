package ch.inventsoft.serialcommunication

import ch.inventsoft.scalabase.time._
import ch.inventsoft.scalabase.process._


object SerialPortTester extends Application {

  val ports = SerialPort.list
  ports.foreach( port => println(port))
  
}
