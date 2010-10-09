package ch.inventsoft

import scalabase.process._
import scalabase.io._


package object xbee {

  /**
   * Low-level interface onto a local xbee.
   * Allows to send/receive parsed packets (with checksum etc.).
   */
  type LocalLowLevelXBee = CommunicationPort[Command,Command]

  /** A low-level xbee command */
  type Command = Seq[Byte]
}

