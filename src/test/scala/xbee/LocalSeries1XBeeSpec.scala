package ch.inventsoft.xbee

import org.scalatest._
import matchers._
import java.io._
import ch.inventsoft.scalabase.process._
import ch.inventsoft.scalabase.oip._
import ch.inventsoft.scalabase.time._
import ch.inventsoft.scalabase.log._
import ch.inventsoft.scalabase.extcol.ListUtil._
import ch.inventsoft.scalabase.communicationport._
import Messages._
import XBeeParsing._
import scala.concurrent._
import cps.CpsUtils._


class LocalSeries1XBeeSpec extends ProcessSpec with ShouldMatchers {
  
  type Handler = PartialFunction[Seq[Byte],Seq[Byte]]
  private trait Device {
    def commandsInBuffer: List[Seq[Byte]] @processCps
    def sendResponse(response: Seq[Byte]): Unit
    def addHandler(handler: Handler): Unit @processCps
    def handlers: List[Handler] @processCps
  }
  private object TestLowLevelXBee extends SpawnableCompanion[TestLowLevelXBee] {
    def apply() = start(SpawnAsRequiredChild)(new TestLowLevelXBee)
  }
  private class TestLowLevelXBee extends LocalLowLevelXBee with Device with StateServer[TestState] with Log {
    protected[this] override def initialState = TestState(Nil, None, Nil)

    override def sendCommand(command: Seq[Byte]) = cast { state =>
      log.info("Low-Level XBee: Got data: {}", byteListToHex(command))
      def applyHandlers(command: Seq[Byte], handlers: List[Handler], nonMatchingHandlers: List[Handler]): (List[Handler], Option[Seq[Byte]]) = handlers match {
        case handler :: r =>
          if (handler.isDefinedAt(command)) {
            val response = handler(command)
            (nonMatchingHandlers.reverse ::: r, Some(response)) 
          } else {
            applyHandlers(command, r, handler :: nonMatchingHandlers)
          }
        case Nil => (nonMatchingHandlers.reverse, None)
      }
      val (newHandlers, result) = applyHandlers(command, state.handlers.reverse, Nil)
      result match {
        case Some(data) =>
          sendResponse(data)
          state.withHandlers(newHandlers.reverse)
        case None =>
          state.withHandlers(newHandlers.reverse).withIn(command :: state.commandsIn)
      }
    }
    override def setIncomingCommandProcessor(process: Option[Process]) = cast { state =>
      state.withProcessor(process)
    }
    override def close = cast_ { state =>
      None
    }
    
    protected[this] def commandsInBuffer_ = call { state => 
      (state.commandsIn.reverse, state.withIn(Nil)) 
    }
    override def commandsInBuffer = {
      sleep(200 ms)
      receiveWithin(1 s) { this.commandsInBuffer_ }
    }
    override def sendResponse(response: Seq[Byte]) = cast { state =>
      sleep(50 ms)
      log.info("Low-Level XBee: Sending data to processor {}: {}", state.processor.map(_.toString).getOrElse("None"), byteListToHex(response))
      state.processor.foreach(_ ! ReceivedCommand(this, response))
      state
    }
    protected[this] def handlers_ = get(_.handlers)
    override def handlers = receiveWithin(1 s) { this.handlers_ }
    protected[this] def addHandler_(handler: Handler) = cast { state =>
      def applyHandlerToCommands(commands: List[Seq[Byte]], nonMatchingCommands: List[Seq[Byte]]): (Option[Seq[Byte]],List[Seq[Byte]]) = commands match {
        case command :: r => 
          if (handler.isDefinedAt(command)) {
            val data = handler(command)
            (Some(data), nonMatchingCommands.reverse ::: r)
          } else applyHandlerToCommands(r, command :: nonMatchingCommands)
        case Nil => (None, nonMatchingCommands.reverse)
      }
    
      val (result, newCommands) = applyHandlerToCommands(state.commandsIn.reverse, Nil)
      result match {
        case Some(data) =>
          sendResponse(data)
          state.withIn(newCommands.reverse)
        case None =>
          state.withIn(newCommands.reverse).withHandlers(handler :: state.handlers)
      }
    }
    override def addHandler(handler: Handler) = {
      addHandler_(handler)
      sleep(200 ms)
    }
  }
  private case class TestState(commandsIn: List[Seq[Byte]], processor: Option[Process], handlers: List[Handler]) {
    def withIn(commandsIn: List[Seq[Byte]]) = TestState(commandsIn, processor, handlers)
    def withProcessor(processor: Option[Process]) = TestState(commandsIn, processor, handlers)
    def withHandlers(handlers: List[Handler]) = TestState(commandsIn, processor, handlers)
  }
  

  val shouldAddress = XBeeAddress64(0x0102030405060708L)
  private def initialize: (Device,LocalXBee) @processCps = {
    val lowLevel = TestLowLevelXBee()
    val xbee = LocalSeries1XBee(lowLevel)(SpawnAsRequiredChild)

    lowLevel.addHandler {
      case AT.SH_read((frameId),Nil) =>
        AT.SH_response((frameId, AT.StatusOk), shouldAddress.highPart)
    }
    lowLevel.addHandler {
      case AT.SL_read((frameId),Nil) =>
        AT.SL_response((frameId, AT.StatusOk), shouldAddress.lowPart)
    }
    sleep(600 ms)
    (lowLevel, xbee)
  }
  def stop(device: Device, local: LocalXBee) = {
    sleep(300 ms)
    assertEquals(device.commandsInBuffer, Nil)
    assertEquals(device.handlers, Nil)
    local.close
  }
  
  describe("LocalSeries1XBee") {
    /*
    it_("should have a 64 bit address") {
      val (device, xbee) = initialize
      // is cached
      assertEquals(receiveWithin(5 s)(xbee.address.option), Some(shouldAddress))
      stop(device,xbee)
    }
    it_("should have the 64 bit address cached") {
      val (device, xbee) = initialize
      
      assertEquals(receiveWithin(1 s)(xbee.address.option), Some(shouldAddress))
      assertEquals(device.commandsInBuffer, Nil)
      assertEquals(device.handlers, Nil)

      // now the requests should be cached
      sleep(200 ms)
      assertEquals(receiveWithin(1 s)(xbee.address.option), Some(shouldAddress))
      assertEquals(receiveWithin(1 s)(xbee.address.option), Some(shouldAddress))
      assertEquals(receiveWithin(1 s)(xbee.address.option), Some(shouldAddress))
      assertEquals(receiveWithin(1 s)(xbee.address.option), Some(shouldAddress))
      stop(device,xbee)
    }
    it_("should be able to read the 16-bit address (disabled)") {
      val (device, xbee) = initialize
      
      device.addHandler {
        case AT.MY_read((frameId),Nil) =>
          AT.MY_response((frameId, AT.StatusOk), XBeeAddress16Disabled)
      }
      assertEquals(receiveWithin(1 s)(xbee.alias), None)
      stop(device,xbee)
    }
    it_("should be able to read the 16-bit address (enabled)") {
      val (device, xbee) = initialize
      val address = XBeeAddress16(1234)
      
      device.addHandler {
        case AT.MY_read((frameId),Nil) =>
          AT.MY_response((frameId, AT.StatusOk), address)
      }
      assertEquals(receiveWithin(1 s)(xbee.alias), Some(address))
      stop(device,xbee)
    }
    it_("should be able to set the 16-bit address to nothing") {
      val (device, xbee) = initialize
      
      val a = new SyncVar[XBeeAddress16]
      device.addHandler {
        case AT.MY_set(((frameId,address)),Nil) =>
          a.set(address)
          AT.MY_response((frameId, AT.StatusOk), address)
      }
      xbee.alias(None)
      a.get(1000) should be(Some(XBeeAddress16Disabled))
      stop(device,xbee)
    }
    it_("should be able to set the 16-bit address to something") {
      val (device, xbee) = initialize
      val shouldAddress = XBeeAddress16(1234)

      val a = new SyncVar[XBeeAddress16]
      device.addHandler {
        case AT.MY_set(((frameId,address)),Nil) =>
          a.set(address)
          AT.MY_response((frameId, AT.StatusOk), address)
      }
      xbee.alias(Some(shouldAddress))
      a.get(1000) should be(Some(shouldAddress))
      stop(device,xbee)
    }
    it_("should be possible to send untracked packets to other xbees (64 bit)") {
      val (device, xbee) = initialize
      val destinationAddress = XBeeAddress64(0x0102030405060708L)
      val data = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil map(_.toByte)
      
      xbee.sendPacket(destinationAddress, data)
      val cs = device.commandsInBuffer 
      cs match {
        case TX64((frameId, dest, options, d),r) :: Nil =>
          dest should be(destinationAddress)
          options should be(TransmitOptionNormal)
          d should be(data)
          frameId should be(NoFrameId)
          r should be(Nil)
        case other => fail("Fail "+other)
      }
      stop(device,xbee)
    }
    it_("should be possible to send untracked packets to other xbees (16 bit)") {
      val (device, xbee) = initialize
      val destinationAddress = XBeeAddress16(443)
      val data = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil map(_.toByte)
      
      xbee.sendPacket(destinationAddress, data)
      val cs = device.commandsInBuffer
      cs match {
        case TX16((frameId, dest, options, d),r) :: Nil =>
          dest should be(destinationAddress)
          options should be(TransmitOptionNormal)
          d should be(data)
          frameId should be(NoFrameId)
          r should be(Nil)
        case other => fail("Fail "+other)
      }
      stop(device,xbee)
    }

    it_("should be possible to send tracked packets to other xbees with success (64 bit)") {
      val (device, xbee) = initialize
      val destinationAddress = XBeeAddress64(0x0102030405060708L)
      val data = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil map(_.toByte)

      device.addHandler {
        case TX64((frameId, dest, options, d),r) =>
          TX_status(frameId, TransmitStatusSuccess)
      }

      val r = receiveWithin(1 s) {
        xbee.sendTrackedPacket(destinationAddress, data)
      }
      r should be(TransmitStatusSuccess)
      stop(device,xbee)
    }
    it_("should be possible to send tracked packets to other xbees with no-ack (64 bit)") {
      val (device, xbee) = initialize
      val destinationAddress = XBeeAddress64(0x0102030405060708L)
      val data = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil map(_.toByte)

      device.addHandler {
        case TX64((frameId, dest, options, d),r) =>
          TX_status(frameId, TransmitStatusNoAckReceived)
      }

      val r= receiveWithin(1 s) {
        xbee.sendTrackedPacket(destinationAddress, data)
      }
      r should be(TransmitStatusNoAckReceived)
      stop(device,xbee)
    }
    it_("should be possible to send tracked packets to other xbees with success (16 bit)") {
      val (device, xbee) = initialize
      val destinationAddress = XBeeAddress16(443)
      val data = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil map(_.toByte)

      device.addHandler {
        case TX16((frameId, dest, options, d),r) =>
          TX_status(frameId, TransmitStatusSuccess)
      }

      val r = receiveWithin(1 s) {
        xbee.sendTrackedPacket(destinationAddress, data)
      }
      r should be(TransmitStatusSuccess)
      stop(device,xbee)
    }
    
    it_("should be possible to send tracked packets to other xbees with no-ack (16 bit)") {
      val (device, xbee) = initialize
      val destinationAddress = XBeeAddress16(443)
      val data = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil map(_.toByte)

      device.addHandler {
        case TX16((frameId, dest, options, d),r) =>
          TX_status(frameId, TransmitStatusNoAckReceived)
      }

      val r = receiveWithin(1 s) {
        xbee.sendTrackedPacket(destinationAddress, data)
      }
      r should be(TransmitStatusNoAckReceived)
      stop(device,xbee)
    }
    
    it_("should be possible to receive packets from other xbees (64-bit)") {
      val (device, xbee) = initialize
      val sourceAddress = XBeeAddress64(0x0102030405060708L)
      val data = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil map(_.toByte)
      
      xbee.incomingMessageProcessor(Some(self))
      sleep(200 ms)
      
      device.sendResponse(RX64(sourceAddress, SignalStrength(-12), ReceiveOption(false, false), data))
      
      receiveWithin(1 s) {
        case XBeeDataPacket(receiver, source, signal, broadcast, d) =>
          receiver should be(xbee)
          source should be(sourceAddress)
          signal should be(Some(SignalStrength(-12)))
          broadcast should be(false)
          d should be(data)
        case other => fail("Fail "+other)  
      }
      stop(device,xbee)
    }
    it_("should be possible to receive packets broadcastet by other xbees (64-bit)") {
      val (device, xbee) = initialize
      val sourceAddress = XBeeAddress64(0x0102030405060708L)
      val data = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil map(_.toByte)
      
      xbee.incomingMessageProcessor(Some(self))
      sleep(200 ms)
      
      device.sendResponse(RX64(sourceAddress, SignalStrength(-32), ReceiveOption(true, false), data))
      
      receiveWithin(1 s) {
        case XBeeDataPacket(receiver, source, signal, broadcast, d) =>
          receiver should be(xbee)
          source should be(sourceAddress)
          signal should be(Some(SignalStrength(-32)))
          broadcast should be(true)
          d should be(data)
        case other => fail("Fail "+other)  
      }
      stop(device,xbee)
    }
    it_("should be possible to receive packets from other xbees (16-bit)") {
      val (device, xbee) = initialize
      val sourceAddress = XBeeAddress16(443)
      val data = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil map(_.toByte)
      
      xbee.incomingMessageProcessor(Some(self))
      sleep(200 ms)
      
      device.sendResponse(RX16(sourceAddress, SignalStrength(-200), ReceiveOption(false, false), data))
      
      receiveWithin(1 s) {
        case XBeeDataPacket(receiver, source, signal, broadcast, d) =>
          receiver should be(xbee)
          source should be(sourceAddress)
          signal should be(Some(SignalStrength(-200)))
          broadcast should be(false)
          d should be(data)
        case other => fail("Fail "+other)  
      }
      stop(device,xbee)
    }
    it_("should be possible to receive packets broadcasted by other xbees (16-bit)") {
      val (device, xbee) = initialize
      val sourceAddress = XBeeAddress16(443)
      val data = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil map(_.toByte)
      
      xbee.incomingMessageProcessor(Some(self))
      sleep(200 ms)
      
      device.sendResponse(RX16(sourceAddress, SignalStrength(-40), ReceiveOption(false, true), data))
      
      receiveWithin(1 s) {
        case XBeeDataPacket(receiver, source, signal, broadcast, d) =>
          receiver should be(xbee)
          source should be(sourceAddress)
          signal should be(Some(SignalStrength(-40)))
          broadcast should be(true)
          d should be(data)
        case other => fail("Fail "+other)  
      }
      stop(device,xbee)
    }
    */
    
    it_("should be possible to discover nodes") {
      val (device,xbee) = initialize
      val node1 = DiscoveredXBeeDevice(XBeeAddress64(0x0102030405060708L),None,Some(SignalStrength(-10)))
      val node2 = DiscoveredXBeeDevice(XBeeAddress64(0x0102030405060709L),Some(XBeeAddress16(0x1215)),Some(SignalStrength(-11)))
      val node3 = DiscoveredXBeeDevice(XBeeAddress64(0x0102030405060701L),Some(XBeeAddress16(0x1311)),Some(SignalStrength(-12)))
      val node4 = DiscoveredXBeeDevice(XBeeAddress64(0x0102030405060703L),Some(XBeeAddress16(0x0023)),Some(SignalStrength(-40)))
      val nodes = node1 :: node2 :: node3 :: node4 :: Nil
      
      def nToC(frame: FrameId, node: DiscoveredXBeeDevice) = {
        AT.ND_node((frame,AT.StatusOk), node.address16.getOrElse(XBeeAddress16Disabled), node.address64, node.signalStrength.get, "Ha")
      }

      val discover = xbee.discover(2 s)
      
      val cs = device.commandsInBuffer
      cs match {
        case AT.NT((frameNt,timeout),rest) :: AT.ND((frame),Nil) :: Nil =>
          device.sendResponse(AT.NT_response((frameNt,AT.StatusOk),timeout))
          nodes.foreach_cps { node =>
            sleep(timeout / (nodes.size+1))
            device.sendResponse(nToC(frame, node))
          }
          device.sendResponse(AT.ND_end((frame,AT.StatusOk)))
        case other => fail("Fail "+other)
      }
      
      val r = receiveWithin(5 s) { discover }
      r should be(node1 :: node2 :: node3 :: node4 :: Nil)
      stop(device,xbee)
    }
    it_("should be possible to discover nodes (no nodes)") {
      val (device,xbee) = initialize
      val nodes = Nil
      
      def nToC(frame: FrameId, node: DiscoveredXBeeDevice) = {
        AT.ND_node((frame,AT.StatusOk), node.address16.getOrElse(XBeeAddress16Disabled), node.address64, node.signalStrength.get, "Ha")
      }

      val discover = xbee.discover(2 s)
      
      val cs = device.commandsInBuffer 
      cs match {
        case AT.NT((frameNt,timeout),rest) :: AT.ND((frame),Nil) :: Nil =>
          device.sendResponse(AT.NT_response((frameNt,AT.StatusOk),timeout))
          sleep(timeout - (200 ms))
          device.sendResponse(AT.ND_end((frame,AT.StatusOk)))
        case other => fail("Fail "+other)
      }
      
      assertEquals(receiveWithin(5 s) { discover }, Nil)
      stop(device,xbee)
    }
  }

}
