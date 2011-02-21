package ch.inventsoft
package xbee

import scala.collection.immutable.Queue
import org.scalatest._
import matchers._
import java.io._
import scalabase.process._
import scalabase.oip._
import scalabase.time._
import scalabase.log._
import scalabase.extcol.ListUtil._
import scalabase.io._
import Messages._
import XBeeParsing._
import scala.concurrent._


class LocalSeries1XBeeSpec extends ProcessSpec with ShouldMatchers with Log {
  
  type Handler = PartialFunction[Command,Command]
  private trait Device {
    def commandsInBuffer: List[Command] @process
    def sendResponse(response: Command): Unit @process
    def addHandler(handler: Handler): Unit @process
    def handlers: List[Handler] @process
  }
  private object TestLowLevelXBee {
    def apply() = Spawner.start(new TestLowLevelXBee, SpawnAsRequiredChild)
  }
  private class TestLowLevelXBee extends LocalLowLevelXBee with Device with StateServer with Log {
    case class State(commands: List[Command],
                      readBuffer: List[Command],
                      deviceHandlers: List[Handler],
                      xbeeHandler: Option[Command => Unit @process])

    protected override def init = {
      State(Nil, Nil, Nil, None)
    }

    private def applyHandlers(command: Command, handlers: List[Handler], nonMatching: List[Handler]):
        (List[Handler], Option[Command]) = handlers match {
      case handler :: r =>
        if (handler.isDefinedAt(command)) {
          log.trace("ah: applying handler for {}", byteListToHex(command))
          val response = handler(command)
          (nonMatching.reverse ::: r, Some(response)) 
        } else {
          applyHandlers(command, r, handler :: nonMatching)
        }
      case Nil =>
        log.trace("ah: enqueuing {} (none of {} handlers matched)", byteListToHex(command), nonMatching.size)
        (nonMatching.reverse, None)
    }
    private def applyHandlers(cmds: List[Command], handlers: List[Handler], unmatched: List[Command] = Nil):
        (List[Handler], List[Command]) @process = cmds match {
      case command :: rest =>
        val (nh, r) = applyHandlers(command, handlers, Nil)
        val um = if (r.isDefined) {
          sendResponse(r.get)
          unmatched
        } else {
          command :: unmatched
        }
        applyHandlers(rest, nh, um)
      case Nil =>
        (handlers, unmatched.reverse)
    }
    override def write(cmds: Seq[Command]) = call { state =>
      log.info("write: {}", cmds.map(byteListToHex(_)).mkString(", "))
      sleep(200 ms)
      val (nh, unmatched) = applyHandlers(cmds.toList, state.deviceHandlers)
      ((), state.copy(commands = state.commands ++ unmatched, deviceHandlers = nh))
    }
    override def writeCast(cmds: Seq[Command]) = cast { state =>
      log.info("writeCast: {}", cmds.map(byteListToHex(_)).mkString(", "))
      sleep(200 ms)
      val (nh, unmatched) = applyHandlers(cmds.toList, state.deviceHandlers)
      state.copy(commands = state.commands ++ unmatched, deviceHandlers = nh)
    }
    override def read(max: Int) = {
      log.trace("read: enqueuing")
      this ! new ModifyStateMessage with MessageWithSimpleReply[Read[Command]] {
        override def execute(state: State) = state.readBuffer match {
          case Nil =>
            val handler = (cmd: Command) => {
              log.trace("read(): Received command {}", byteListToHex(cmd))
              reply(Data(cmd :: Nil))
            }
            state.copy(xbeeHandler = Some(handler))
          case cmds =>
            log.trace("read(): Returning {} commands", cmds.length)
            reply(Data(cmds))
            state.copy(readBuffer=Nil)
        }
      }
    }.receive
    override def readWithin(timeout: Duration, max: Int) = {
      log.trace("read(timeout): enqueuing")
      this ! new ModifyStateMessage with MessageWithSimpleReply[Option[Read[Command]]] {
        override def execute(state: State) = state.readBuffer match {
          case Nil =>
            val handler = (cmd: Command) => {
              log.trace("read(timeout): Received command {}", byteListToHex(cmd))
              reply(Some(Data(cmd :: Nil)))
            }
            spawnChild(Required) {
              sleep(timeout)
              cancelHandler(handler) {
                log.trace("read(timeout): Canceled handler")
                reply(None)
              }
            }
            state.copy(xbeeHandler = Some(handler))
          case cmds =>
            log.trace("read(timeout): Returning {} commands", cmds.length)
            reply(Some(Data(cmds)))
            state.copy(readBuffer=Nil)
        }
      }
    }.receive
    protected def cancelHandler(handler: Command => Unit @process)(exec: => Unit @process) = cast { state =>
      if (state.xbeeHandler.isDefined && state.xbeeHandler.get.eq(handler)) {
        exec
        state.copy(xbeeHandler = None)
      } else state
    }

    override def sendResponse(response: Command) = cast { state =>
      sleep(100 ms)
      log.info("Low-Level XBee: Sending data {}", byteListToHex(response))
      state.xbeeHandler match {
        case Some(handler) =>
          handler(response)
          state.copy(xbeeHandler=None)
        case None =>
          noop
          state.copy(readBuffer=state.readBuffer ::: response :: Nil)
      }
    }

    override def commandsInBuffer = {
      sleep(100 ms)
      call { state =>
        (state.commands.toList, state.copy(commands=Nil))
      }.receiveWithin(2 s)
    }
    override def addHandler(handler: Handler) = cast { state =>
      val (h, nc) = applyHandlers(state.commands.toList, handler :: Nil)
      if (h.isEmpty) log.trace("addHandler: direct application")
      else log.trace("addHandler: enqueued handler")
      state.copy(commands=nc, deviceHandlers = state.deviceHandlers ::: h)
    }
    override def handlers = get(_.deviceHandlers).receiveWithin(1 s)

    override def close = stopAndWait
  }
  

  val shouldAddress = XBeeAddress64(0x0102030405060708L)
  private def initialize: (Device,LocalXBee) @process = {
    val lowLevel = TestLowLevelXBee()
    val xbee = LocalSeries1XBee(lowLevel)

    lowLevel.addHandler {
      case AT.SH_read((frameId),Nil) =>
        log.trace("SH-handler: response")
        AT.SH_response((frameId, AT.StatusOk), shouldAddress.highPart)
    }
    lowLevel.addHandler {
      case AT.SL_read((frameId),Nil) =>
        log.trace("SL-handler: response")
        AT.SL_response((frameId, AT.StatusOk), shouldAddress.lowPart)
    }
    sleep(800 ms)
    noPending(lowLevel)
    (lowLevel, xbee)
  }
  def stop(device: Device, local: LocalXBee) = {
    sleep(300 ms)
    noPending(device)
    local.close.await
  }
  def noPending(device: Device) = {
    val bc = device.commandsInBuffer
    if (bc.nonEmpty)
      fail("Device commands left "+bc.map(byteListToHex(_)))
    val bh = device.handlers
    if (bh.nonEmpty)
      fail("Device handlers left "+bh)
  }
  describe("LocalSeries1XBee") {
    it_("should have a 64 bit address") {
      val (device, xbee) = initialize
      // is cached
      val address = xbee.address.receiveOption(5 s)
      address should be(Some(shouldAddress))
      stop(device,xbee)
    }
    it_("should have the 64 bit address cached") {
      val (device, xbee) = initialize
      
      assertEquals(receiveWithin(1 s)(xbee.address.option), Some(shouldAddress))
      noPending(device)

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
      
      xbee.send(destinationAddress, data)
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
      
      xbee.send(destinationAddress, data)
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
        xbee.sendTracked(destinationAddress, data)
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
        xbee.sendTracked(destinationAddress, data)
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
        xbee.sendTracked(destinationAddress, data)
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
        xbee.sendTracked(destinationAddress, data)
      }
      r should be(TransmitStatusNoAckReceived)
      stop(device,xbee)
    }

    it_("should be possible to receive packets from other xbees (64-bit)") {
      val (device, xbee) = initialize
      val sourceAddress = XBeeAddress64(0x0102030405060708L)
      val data = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil map(_.toByte)
      
      val rec = self
      xbee.setMessageHandler(rec ! _)
      sleep(200 ms)
      
      device.sendResponse(RX64(sourceAddress, SignalStrength(-12), ReceiveOption(false, false), data))
      
      receiveWithin(1 s) {
        case ReceivedXBeeDataPacket(source, signal, broadcast, d) =>
          source should be(sourceAddress)
          signal should be(Some(SignalStrength(-12)))
          broadcast should be(false)
          d should be(data)
        case other => fail("Fail "+other)  
      }
      stop(device,xbee)
    }
    it_("should be possible to receive a packet from another xbee (real-world)") {
      val (device, xbee) = initialize
      
      val rec = self
      xbee.setMessageHandler(rec ! _)
      sleep(200 ms)
      
      val inData = 0x80 :: 0x00 :: 0x13 :: 0xA2 :: 0x00 :: 0x40 :: 0x3A :: 0xD0 :: 0xA6 :: 0x30 :: 0x00 :: 0x04 :: 0x00 :: 0x1A :: 0x00 :: 0x07 :: 0x00 :: 0x00 :: Nil map(_.toByte)
      device.sendResponse(inData)
      
      receiveWithin(1 s) {
        case ReceivedXBeeDataPacket(source, signal, broadcast, d) =>
          source should be(XBeeAddress64(0x0013A200403AD0A6L))
          signal should be(Some(SignalStrength(-48)))
          broadcast should be(false)
          d should be(0x04 :: 0x00 :: 0x1A :: 0x00 :: 0x07 :: 0x00 :: 0x00 :: Nil map(_.toByte))
        case other => fail("Fail "+other)  
      }
      stop(device,xbee)
    }
    it_("should be possible to receive packets broadcastet by other xbees (64-bit)") {
      val (device, xbee) = initialize
      val sourceAddress = XBeeAddress64(0x0102030405060708L)
      val data = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil map(_.toByte)
      
      val rec = self
      xbee.setMessageHandler(rec ! _)
      sleep(200 ms)
      
      device.sendResponse(RX64(sourceAddress, SignalStrength(-32), ReceiveOption(true, false), data))
      
      receiveWithin(1 s) {
        case ReceivedXBeeDataPacket(source, signal, broadcast, d) =>
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
      
      val rec = self
      xbee.setMessageHandler(rec ! _)
      sleep(200 ms)
      
      device.sendResponse(RX16(sourceAddress, SignalStrength(-200), ReceiveOption(false, false), data))
      
      receiveWithin(1 s) {
        case ReceivedXBeeDataPacket(source, signal, broadcast, d) =>
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
      
      val rec = self
      xbee.setMessageHandler(rec ! _)
      sleep(200 ms)
      
      device.sendResponse(RX16(sourceAddress, SignalStrength(-40), ReceiveOption(false, true), data))
      
      receiveWithin(1 s) {
        case ReceivedXBeeDataPacket(source, signal, broadcast, d) =>
          source should be(sourceAddress)
          signal should be(Some(SignalStrength(-40)))
          broadcast should be(true)
          d should be(data)
        case other => fail("Fail "+other)  
      }
      stop(device,xbee)
    }

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
      sleep(100 ms)

      val cs = device.commandsInBuffer
      cs match {
        case AT.NT((frameNt,timeout),rest) :: Nil =>
          device.sendResponse(AT.NT_response(frameNt,AT.StatusOk))
        case other => fail("Fail nt "+other)
      }
      sleep(200 ms)
      
      val cs2 = device.commandsInBuffer
      cs2 match {
        case AT.ND((frame),Nil) :: Nil =>
          nodes.foreach_cps { node =>
            sleep((1 s) / (nodes.size+1))
            device.sendResponse(nToC(frame, node))
          }
          device.sendResponse(AT.ND_end((frame,AT.StatusOk)))
        case other => fail("Fail nd "+other)
      }
      
      val r = receiveWithin(5 s) { discover }
      r should be(node1 :: node2 :: node3 :: node4 :: Nil)
      stop(device,xbee)
    }
    it_("should be possible to discover nodes (no nodes)") {
      val (device,xbee) = initialize
      val nodes = Nil

      noPending(device)
      val discover = xbee.discover(2 s)
      sleep(100 ms)

      val cs = device.commandsInBuffer 
      cs match {
        case AT.NT((frameNt,timeout),Nil) :: Nil =>
          device.sendResponse(AT.NT_response(frameNt,AT.StatusOk))
        case other => fail("Fail nt "+other.map(byteListToHex(_)))
      }
      sleep(500 ms)
      val cs2 = device.commandsInBuffer
      cs2 match {
        case AT.ND((frame),Nil) :: Nil =>
          device.sendResponse(AT.ND_end((frame,AT.StatusOk)))
        case other => fail("Fail nd "+other.map(byteListToHex(_)))
      }
      
      assertEquals(discover.receiveWithin(5 s), Nil)
      stop(device,xbee)
    }
  }
}
