package ch.inventsoft.xbee

import ch.inventsoft.scalabase.time._
import ch.inventsoft.scalabase.oip._
import ch.inventsoft.scalabase.process._
import ch.inventsoft.xbee._
import ch.inventsoft.serialcommunication._
import ch.inventsoft.scalabase.process.cps.CpsUtils._


object Series1Example extends Application {
val w = new scala.concurrent.SyncVar[Unit]
spawn {
  println("Serial Ports (usbserial)")
  SerialPort.list.foreach(e => println("  "+e))
  // val portDesc = SerialPort.forName("/dev/cu.usbserial-FTELSJ9T").get
  val portDesc = SerialPort.forName("/dev/cu.usbserial-A6008hNp").get
  println("Connecting to "+portDesc)
  val serialPort = portDesc.open(19200)(SpawnAsRequiredChild)
  println("Connected to "+portDesc)
  val lowlevel =  LocalLowLevelXBeeInApiModePort(serialPort).receive
  println("Connected lowlevel XBee")
  val xbee = LocalSeries1XBee(lowlevel)

  //Print the addresses of all other devices
  val peers = receive { xbee.discover() }
  println("Found peer xbees")
  peers.foreach(e => println("  "+e))

  //Print my id
  val my = receive { xbee.address }
  println("I am "+my)
  
  //Send 09080706 to all devices (one at a time)
  peers.foreach_cps { p =>
    xbee.sendTracked(p.address64, 0x09 :: 0x08 :: 0x07 :: 0x06 :: Nil map(_.toByte))
    receiveWithin(500 ms) { case Timeout => () }
  }

  //Broadcast 01020304 to all xbees
  val data = 0x01 :: 0x02 :: 0x03 :: 0x04 :: Nil map(_.toByte)
  xbee.broadcast(data);

  //Receive a message
  val rcv = self
  xbee.setMessageHandler(rcv ! _)

  receiveWithin(20 seconds) {
    case packet: ReceivedXBeeDataPacket =>
      println("Received: "+packet)
    case Timeout => 
      println("nothing received");
  }

  //Shutdown
  println("done")
  xbee.close
  lowlevel.close
  serialPort.close
  w.set(())
}
w.get(60000)
}
