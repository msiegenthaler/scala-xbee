package ch.inventsoft.xbee


/**
 * Id of an XBee 'packet' used for message receival tracking (receipt).
 * The values 1-255 generate a receipt with the same number, the value 0
 * signal that no receipt is required.
 */
sealed trait FrameId {
  val value: Byte
  def next: FrameId
  final def ++ = next
  def hasReceipt: Boolean
  override def equals(o: Any) = o match {
    case FrameId(id) => value == id
    case _ => false
  }
  override def hashCode = value.hashCode
  override def toString = "FrameId("+value+")"
}

object NoFrameId extends FrameId {
  override val value: Byte = 0
  override val next = this
  override val hasReceipt = false
}

object FrameId {
  def apply(): FrameId= new FrameIdImpl(1)
  def apply(id: Byte): FrameId = {
    if (id == 0) NoFrameId
    else new FrameIdImpl(id)
  }
  def unapply(frameId: FrameId) = Some(frameId.value)

  private class FrameIdImpl(val value: Byte) extends FrameId {
    def next = {
      val next = (value + 1).toByte
      if (next == 0) new FrameIdImpl(1) else new FrameIdImpl(next)
    }
    override val hasReceipt = true 
  }
}
