package ch.inventsoft
package xbee

import org.scalatest._
import matchers._
import java.io._
import scalabase.process._
import Messages._
import scalabase.oip._
import scalabase.time._
import scalabase.io._
import XBeeParsing._


class LocalLowLevelXBeeInApiModeSpec extends ProcessSpec with ShouldMatchers {

  trait Device {
    def readAsDevice(byteCount: Int, delay: Duration = (200 ms)): List[Byte] @process
    def sendAsDevice(data: List[Byte], delay: Duration = (200 ms)): Unit @process
    def close: Unit @process
  }
  private class TestPortContainer extends Device {
    val deviceInput = new PipedInputStream
    val deviceOutput = new PipedOutputStream
    val portInput = new PipedInputStream(deviceOutput)
    val portOutput = new PipedOutputStream(deviceInput)
    
    override def readAsDevice(byteCount: Int, delay: Duration = (200 ms)) = {
      sleep(delay)
      val buffer = new Array[Byte](byteCount)
      def readIt(pos: Int): List[Byte] = {
        if (pos == byteCount) buffer.toList
        else deviceInput.read(buffer, pos, byteCount-pos) match {
          case -1 => buffer.take(pos).toList
          case 0 => readIt(pos)
          case count => readIt(pos+count)
        }
      }
      readIt(0)
    }
    override def sendAsDevice(data: List[Byte], delay: Duration = (200 ms)) = {
      deviceOutput.write(data.toArray)
      deviceOutput.flush
      sleep(delay)
    }
    
    var _port: CommunicationPort[Byte,Byte] = null
    def port = {
      if (_port == null) throw new IllegalStateException
      else _port
    }
    def start: Unit @process = {
      _port = {
        case class SourceSink(source: Source[Byte], sink: Sink[Byte])
        CommunicationPort[Byte,Byte,SourceSink](
          open = {
            val source = InputStreamSource(portInput)
            val sink = OutputStreamSink(portOutput)
            SourceSink(source, sink)
          },
          close = { (ss: SourceSink) =>
            val s1 = ss.sink.close
            val s2 = ss.source.close
            s1.await
            s2.await
          },
          as = SpawnAsRequiredChild
        ).receiveWithin(1 s)
      }
    }
    
    def close = {
      port.close
      deviceInput.close
      deviceOutput.close
    }
  }

/*
  class TestCommunicationPort private() extends CommunicationPort with StateServer[PortState] with Device {
    protected[this] override def initialState = PortState(Nil, Nil, None)
    
    def send(data: Iterator[Byte]) = cast { state =>
      state.withInput(state.inputBuffer ++ data.toList)
    }
    override def receive = call { state =>
      (state.outputBuffer, state.withOutput(Nil))
    }
    override def redirectIncomingTo(process: Option[Process]) = cast { state =>
      val s1 = state.withForwardTo(process)
      if (process.isDefined && !state.outputBuffer.isEmpty) {
        process.get ! DataReceived(this, state.outputBuffer)
        state.withOutput(Nil)
      } else s1
    }
    
    override def close = call_ { state =>
      ((), None)
    }

    override def internalDeviceSends(data: Seq[Byte]) = cast { state =>
      val newState = state.forwardTo match {
	case Some(forwardTo) =>
	  forwardTo ! DataReceived(this, data)
	  state
	case None =>
	  state.withOutput(state.outputBuffer ++ data)
      }
      newState
    }
    override def internalDeviceReceive = call { state =>
      (state.inputBuffer, state.withInput(Nil))
    }
  }
  object TestCommunicationPort extends SpawnableCompanion[TestCommunicationPort] {
    def apply(as: SpawnStrategy) = start(as)(new TestCommunicationPort)
  }
  case class PortState(inputBuffer: Seq[Byte], outputBuffer: Seq[Byte], forwardTo: Option[Process]) {
    def withInput(inputBuffer: Seq[Byte]) = PortState(inputBuffer, outputBuffer, forwardTo)
    def withOutput(outputBuffer: Seq[Byte]) = PortState(inputBuffer, outputBuffer, forwardTo)
    def withForwardTo(forwardTo: Option[Process]) = PortState(inputBuffer, outputBuffer, forwardTo)
  }
  */
  def initialize: (Device, LocalLowLevelXBee) @process = {
    val container = new TestPortContainer
    container.start
    val port = container.port
    val xbee = LocalLowLevelXBeeInApiModePort(port, SpawnAsRequiredChild).receiveWithin(1 s)
    (container, xbee)
  }
  def checkNoPending(xbee: LocalLowLevelXBee): Unit @process = {
    val res = xbee.readWithin(200 ms)
    if (res.nonEmpty) fail("Failed checkNoPending: "+res.get)
    ()
  }

  describe("low level xbee") {
    describe("sending") {
      it_("should forward the sent data") {
        val (device, xbee) = initialize
        xbee.write(AT.MY_read(FrameId(0x52))).await
        val r = device.readAsDevice(8)
	r should be(0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x52 :: 0x4D :: 0x59 :: 0xFF :: Nil map(_.toByte))
	xbee.close.await
	device.close
      }
    }
    describe("receiving") {
      it_("should receive a complete command") {
        val (device, xbee) = initialize
        device.sendAsDevice(0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x52 :: 0x4D :: 0x59 :: 0xFF :: Nil map(_.toByte))

        val data = xbee.readWithin(1 s).get
        data should be(mkData(0x08, 0x52, 0x4D, 0x59))

        checkNoPending(xbee)
        xbee.close.await
	device.close
      }
      it_("should receive a command the is sent in single bytes") {
        val (device, xbee) = initialize
        val data = 0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x52 :: 0x4D :: 0x59 :: 0xFF :: Nil map(_.toByte)
        data.foreach_cps { b =>
          checkNoPending(xbee)
          device.sendAsDevice(b :: Nil)
        }
        val rec = xbee.readWithin(1 s).get
        rec should be(mkData(0x08, 0x52, 0x4D, 0x59))

        checkNoPending(xbee)
	xbee.close.await
	device.close
      }
      it_("should receive two complete command that are sent together") {
        val (device, xbee) = initialize
        val c1 = 0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x52 :: 0x4D :: 0x59 :: 0xFF :: Nil map(_.toByte)
        val c2 = 0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x01 :: 0x44 :: 0x48 :: 0x6A :: Nil map(_.toByte)
        device.sendAsDevice(c1 ::: c2)
        
        val r1 = xbee.readWithin(1 s)
        r1 should be(Some(Data(mkd(0x08, 0x52, 0x4D, 0x59) :: mkd(0x08, 0x01, 0x44, 0x48) :: Nil)))

        checkNoPending(xbee)
	device.close
	xbee.close.await
      }
      it_("should receive two complete command that sent 1.5 to .5 ") {
        val (device, xbee) = initialize
        val c1 = 0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x52 :: 0x4D :: 0x59 :: 0xFF :: Nil map(_.toByte)
        val c2 = 0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x01 :: 0x44 :: 0x48 :: 0x6A :: Nil map(_.toByte)
        device.sendAsDevice(c1 ::: c2.take(3))
        
        val r1 = xbee.readWithin(2 s)
        r1 should be(Some(mkData(0x08, 0x52, 0x4D, 0x59)))
        checkNoPending(xbee)
        
        device.sendAsDevice(c2.drop(3))
        val r2 = xbee.readWithin(2 s)
        r2 should be(Some(mkData(0x08, 0x01, 0x44, 0x48)))

        checkNoPending(xbee)
	device.close
	xbee.close.await
      }
      it_("should receive a command with junk before it") {
        val (device, xbee) = initialize
        val junk = 0x12 :: 0x13 :: Nil map(_.toByte)
        val c1 = 0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x52 :: 0x4D :: 0x59 :: 0xFF :: Nil map(_.toByte)
        device.sendAsDevice(junk ::: c1)

        val r1 = xbee.readWithin(1 s)
        r1 should be(Some(mkData(0x08, 0x52, 0x4D, 0x59)))

        checkNoPending(xbee)
	device.close
	xbee.close.await
      }
      it_("should receive two complete command that are sent together with junk between") {
        val (device, xbee) = initialize
        val junk = 0x01 :: 0x12 :: 0x13 :: Nil map(_.toByte)
        val c1 = 0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x52 :: 0x4D :: 0x59 :: 0xFF :: Nil map(_.toByte)
        val c2 = 0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x01 :: 0x44 :: 0x48 :: 0x6A :: Nil map(_.toByte)
        device.sendAsDevice(c1 ::: junk ::: c2)
        
        val r1 = xbee.readWithin(1 s)
        r1 should be(Some(Data(mkd(0x08, 0x52, 0x4D, 0x59) :: mkd(0x08, 0x01, 0x44, 0x48) :: Nil)))

        checkNoPending(xbee)
	device.close
	xbee.close.await
      }
    }
  }

  def mkData(d: Int*) = {
    val data = d.map(_.toByte).toList
    Data(data :: Nil)
  }
  def mkd(d: Int*) = {
    d.map(_.toByte).toList
  }
}
