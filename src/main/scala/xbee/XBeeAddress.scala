package ch.inventsoft
package xbee


/**
 * Address of an XBee device.
 */
sealed trait XBeeAddress {
  def isAddressable: Boolean
}


/**
 * Alias address of an XBee device (16-bit)
 */
sealed trait XBeeAddress16 extends XBeeAddress {
  val value: Short
  override def toString = {
    val s = value.toHexString.toUpperCase
    val hex = if (s.length>4) s.drop(s.length-4)
      else "0" * (4-s.length) + s
    "XBeeAddress16(0x"+hex+")"
  }
  override def equals(other: Any) = other match {
    case a: XBeeAddress16 => a.value == value
    case _ => false
  }
  override def hashCode = value.hashCode
}
/**
 * 16-bit addressing is disabled on the device.
 */
object XBeeAddress16Disabled extends XBeeAddress16 {
  override val value: Short = -1
  override val isAddressable = false
}
object XBeeAddress16 {
  def apply(value: Short): XBeeAddress16 = {
    if (value == XBeeAddress16Disabled.value) XBeeAddress16Disabled
    else new XBeeAddress16Impl(value)
  }
  def unapply(address: XBeeAddress16) = Some(address.value) 
  
  private class XBeeAddress16Impl(override val value: Short) extends XBeeAddress16 {
    override val isAddressable = true
  }
}


/**
 * Unique (physical) address of an XBee device.
 */
case class XBeeAddress64(value: Long) extends XBeeAddress {
  override val isAddressable = true
  val isBroadcast = value == 0xFFFF //see XBeeAddress64Broadcast
  def lowPart = XBeeAddress64_Low((value & 0xFFFFFFFF).toInt)
  def highPart = XBeeAddress64_High(((value >> 32) & 0xFFFFFFFF).toInt)
  def split = (highPart, lowPart)
  override def toString = {
    val s = value.toHexString
    "XBeeAddress64(0x"+("0" * (16-s.length) + s).toUpperCase+")"
  }
}
object XBeeAddress64Broadcast extends XBeeAddress64(0xFFFFL)  // see XBee-Addressing

/**
 * Lower 32 bit of a 64 bit address
 */
case class XBeeAddress64_Low(value: Int) {
  def +(high: XBeeAddress64_High) = {
    val v = (high.value.toLong << 32) | value
    XBeeAddress64(v)
  }
}
/**
 * Higher 32 bit of a 64 bit address
 */
case class XBeeAddress64_High(value: Int) {
  def +(low: XBeeAddress64_Low) = low + this
}
