package ch.inventsoft.serialcommunication

import ch.inventsoft.scalabase.oip._
import ch.inventsoft.scalabase.time._
import ch.inventsoft.scalabase.process._
import ch.inventsoft.scalabase.process.cps.CpsUtils._


object SerialPortTester extends Application { spawn {
  val ports = SerialPort.list
  ports.foreach( port => println(port))
  
  ports.foreach_cps { pd => 
    val p = pd.open(19200)(SpawnAsRequiredChild)
    println("Opened "+pd+", closing it...")
    p.close
  }
}}