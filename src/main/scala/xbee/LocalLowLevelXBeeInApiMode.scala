package ch.inventsoft
package xbee

import scalabase.io._
import scalabase.process._
import Messages._
import scalabase.oip._
import scalabase.log._
import scalabase.extcol.ListUtil._
import XBeeParsing._


/**
 * LowLevelLocalXBee implementation using a XBee in API-mode attached to a communication port.
 */
object LocalLowLevelXBeeInApiModePort {
  def apply(port: => CommunicationPort[Byte,Byte] @process, as: SpawnStrategy = SpawnAsRequiredChild):
      Selector[LocalLowLevelXBee] @process = {
    val x = CommunicationPort[Command,Command,PortSourceSink](
      open = {
        val source = LowLevelXBeeInApiModeSource(port)
        val sink = LowLevelXBeeInApiModeSink(port)
        PortSourceSink(source, sink)
      },
      close = pss => noop,
      as = as)
    x    
  }
  private case class PortSourceSink(source: Source[Command], sink: Sink[Command])
}

/** Reveived commands from a source representing an xbee in api mode */
trait LowLevelXBeeInApiModeSource extends TransformingSource[Byte,Command,Seq[Byte]] with Log {
  protected override def createAccumulator = Nil
  protected override def process(buffer: Seq[Byte], add: Seq[Byte]) = {
    val (found,rest) = lookForPackets(buffer ++ add)
    (found,rest)
  }
  protected def lookForPackets(in: Seq[Byte], soFar: List[Command] = Nil): (Seq[Command],Seq[Byte]) = {
    val sr = UnescapedPacket.skipInvalid(in)
    if (sr.wasDataSkipped) log.info("XBee: Dropped data {}", byteListToHex(sr.skip))
    sr.data match {
      case UnescapedPacket(command, rest) =>
        lookForPackets(rest, command :: soFar)
      case rest =>
        (soFar.reverse, rest)
    }
  }
}
object LowLevelXBeeInApiModeSource {
  def apply(source: => Source[Byte] @process, as: SpawnStrategy = SpawnAsRequiredChild):
      Source[Command] @process = {
    val lls = new LowLevelXBeeInApiModeSource {
      override def openSource = {
        val rm = ResourceManager[Source[Byte]](
          resource = source,
          close = _.close
        ).receive
        rm.resource
      }
    }
    Spawner.start(lls, as)
  }
}

/** Sends commands to the sink representing an xbee in api mode */
trait LowLevelXBeeInApiModeSink extends TransformingSink[Command,Byte,Unit] with Log {
  protected override def createAccumulator = ()
  protected override def process(a: Unit, cmds: Seq[Command]) = {
    val init: Seq[Byte] = Nil
    val data = cmds.foldLeft(init) { (soFar,command) =>
      val packet = UnescapedPacket(command)
      log.debug("XBee: Sending packet with payload: {}", byteListToHex(command)) 
      log.trace("XBee: Sending data {}", byteListToHex(packet))
      soFar ++ packet
    }
    (data, ())
  }
}
object LowLevelXBeeInApiModeSink {
  def apply(sink: => Sink[Byte] @process, as: SpawnStrategy = SpawnAsRequiredChild):
      Sink[Command] @process = {
    val lls = new LowLevelXBeeInApiModeSink {
      override def openSink = {
        val rm = ResourceManager[Sink[Byte]](
          resource = sink,
          close = _.close
        ).receive
        rm.resource
      }
    }
    Spawner.start(lls, as)
  }
}
