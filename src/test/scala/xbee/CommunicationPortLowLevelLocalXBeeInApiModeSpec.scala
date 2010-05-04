package ch.inventsoft.xbee

import org.scalatest._
import matchers._
import java.io._
import ch.inventsoft.scalabase.process._
import cps.CpsUtils._
import ch.inventsoft.scalabase.oip._
import ch.inventsoft.scalabase.time._
import ch.inventsoft.scalabase.communicationport._
import Messages._
import XBeeParsing._


class CommunicationPortLowLevelLocalXBeeInApiModeSpec extends ProcessSpec with ShouldMatchers {
  
  trait Device { 
    protected[this] def internalDeviceSends(data: Seq[Byte]): Unit
    protected[this] def internalDeviceReceive: MessageSelector[Seq[Byte]]
    
    def deviceReceive: Seq[Byte] @processCps = {
      sleep(200 ms)
      receiveWithin(1 s) { internalDeviceReceive }
    }
    def deviceSends(data: Seq[Byte]): Unit @processCps = {
      internalDeviceSends(data)
      sleep(200 ms)
    }
    def close: MessageSelector[Unit]
  }

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
  
  def initialize: (Device, LocalLowLevelXBee) @processCps = {
    val port = TestCommunicationPort(SpawnAsRequiredChild)
    val xbee = CommunicationPortLowLevelLocalXBeeInApiMode(port)(SpawnAsRequiredChild)
    xbee.setIncomingCommandProcessor(Some(self))
    (port, xbee)
  }
  
  def checkNoPending = {
    receiveWithin(200 ms) {
      case Timeout => //ok
      case other => fail("Failed checkNoPending: "+other)
    }
  }
  
  describe("low level xbee") {
    describe("sending") {
      it_("should forward the sent data") {
        val (device, xbee) = initialize
        xbee.sendCommand(AT.MY_read(FrameId(0x52)))
        val r = device.deviceReceive
	r should be(0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x52 :: 0x4D :: 0x59 :: 0xFF :: Nil map(_.toByte))
	device.close
	xbee.close
      }
    }
    describe("receiving") {
      it_("should receive a complete command") {
        val (device, xbee) = initialize
        device.deviceSends(0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x52 :: 0x4D :: 0x59 :: 0xFF :: Nil map(_.toByte))
        
        receiveWithin(1 s) {
          case ReceivedCommand(`xbee`, data) =>
            data should be(0x08 :: 0x52 :: 0x4D :: 0x59 :: Nil map(_.toByte))
          case other => fail("Failed: "+other)
        }
        checkNoPending
	device.close
	xbee.close
      }
      it_("should receive a command the is sent in single bytes") {
        val (device, xbee) = initialize
        val data = 0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x52 :: 0x4D :: 0x59 :: 0xFF :: Nil map(_.toByte)
        data.foreach_cps { b =>
          checkNoPending
          device.deviceSends(b :: Nil)
        }
        
        receiveWithin(1 s) {
          case ReceivedCommand(`xbee`, data) =>
            data should be(0x08 :: 0x52 :: 0x4D :: 0x59 :: Nil map(_.toByte))
          case other => fail("Failed: "+other)
        }
        checkNoPending
	device.close
	xbee.close
      }
      it_("should receive two complete command that are sent together") {
        val (device, xbee) = initialize
        val c1 = 0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x52 :: 0x4D :: 0x59 :: 0xFF :: Nil map(_.toByte)
        val c2 = 0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x01 :: 0x44 :: 0x48 :: 0x6A :: Nil map(_.toByte)
        device.deviceSends(c1 ::: c2)
        
        receiveWithin(1 s) {
          case ReceivedCommand(`xbee`, data) =>
            data should be(0x08 :: 0x52 :: 0x4D :: 0x59 :: Nil map(_.toByte))
          case other => fail("Failed: "+other)
        }
        receiveWithin(1 s) {
          case ReceivedCommand(`xbee`, data) =>
            data should be(0x08 :: 0x01 :: 0x44 :: 0x48 :: Nil map(_.toByte))
          case other => fail("Failed "+other)
        }
        checkNoPending
	device.close
	xbee.close
      }
      it_("should receive two complete command that sent 1.5 to .5 ") {
        val (device, xbee) = initialize
        val c1 = 0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x52 :: 0x4D :: 0x59 :: 0xFF :: Nil map(_.toByte)
        val c2 = 0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x01 :: 0x44 :: 0x48 :: 0x6A :: Nil map(_.toByte)
        device.deviceSends(c1 ::: c2.take(3))
        
        receiveWithin(2 s) {
          case ReceivedCommand(`xbee`, data) =>
            data should be(0x08 :: 0x52 :: 0x4D :: 0x59 :: Nil map(_.toByte))
          case other => fail("Failed: "+other)
        }
        checkNoPending
        
        device.deviceSends(c2.drop(0))
        
        receiveWithin(2 s) { 
          case ReceivedCommand(`xbee`, data) =>
            data should be(0x08 :: 0x01 :: 0x44 :: 0x48 :: Nil map(_.toByte))
          case other => fail("Failed "+other)
        }
        checkNoPending
	device.close
	xbee.close
      }
      it_("should receive a command with junk before it") {
        val (device, xbee) = initialize
        val junk = 0x12 :: 0x13 :: Nil map(_.toByte)
        val c1 = 0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x52 :: 0x4D :: 0x59 :: 0xFF :: Nil map(_.toByte)
        device.deviceSends(junk ::: c1)

        receiveWithin(1 s) {
          case ReceivedCommand(`xbee`, data) =>
            data should be(0x08 :: 0x52 :: 0x4D :: 0x59 :: Nil map(_.toByte))
          case other => fail("Failed: "+other)
        }
        checkNoPending
	device.close
	xbee.close
      }
      it_("should receive two complete command that are sent together with junk between") {
        val (device, xbee) = initialize
        val junk = 0x01 :: 0x12 :: 0x13 :: Nil map(_.toByte)
        val c1 = 0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x52 :: 0x4D :: 0x59 :: 0xFF :: Nil map(_.toByte)
        val c2 = 0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x01 :: 0x44 :: 0x48 :: 0x6A :: Nil map(_.toByte)
        device.deviceSends(c1 ::: junk ::: c2)
        
        receiveWithin(1 s) {
          case ReceivedCommand(`xbee`, data) =>
            data should be(0x08 :: 0x52 :: 0x4D :: 0x59 :: Nil map(_.toByte))
          case other => fail("Failed: "+other)
        }
        receiveWithin(1 s) {
          case ReceivedCommand(`xbee`, data) =>
            data should be(0x08 :: 0x01 :: 0x44 :: 0x48 :: Nil map(_.toByte))
          case other => fail("Failed "+other)
        }
        checkNoPending
	device.close
	xbee.close
      }
    }
  }
}
