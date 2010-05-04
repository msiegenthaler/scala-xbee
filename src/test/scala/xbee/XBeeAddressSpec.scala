package ch.inventsoft.xbee

import org.scalatest._
import matchers._


class XBeeAddressSpec extends Spec with ShouldMatchers {

  describe("XBeeAddress64") {
    it("should have an equals method based on the value") {
      XBeeAddress64(12345L) should be(XBeeAddress64(12345L))
      XBeeAddress64(12345L).hashCode should be(XBeeAddress64(12345L).hashCode)
      
      XBeeAddress64(12345L) should not be(XBeeAddress64(12346L))
    }
    it("should have a nice toString method") {
      XBeeAddress64(0).toString should be("XBeeAddress64(0x0000000000000000)")
      XBeeAddress64(1).toString should be("XBeeAddress64(0x0000000000000001)")
      XBeeAddress64(-1).toString should be("XBeeAddress64(0xFFFFFFFFFFFFFFFF)")
      XBeeAddress64(Long.MaxValue).toString should be("XBeeAddress64(0x7FFFFFFFFFFFFFFF)")
    }
    it("should be addressable") {
      XBeeAddress64(0).isAddressable should be(true)
      XBeeAddress64(1).isAddressable should be(true)
      XBeeAddress64(-1).isAddressable should be(true)
      XBeeAddress64(Long.MaxValue).isAddressable should be(true)
    }
    it("should be splittable into a low and a high part") {
      val address = XBeeAddress64(0x0102030405060708L)
      val (high,low) = address.split
      high should be(XBeeAddress64_High(0x01020304))
      low should  be(XBeeAddress64_Low(0x05060708))
    }
    it("should be composable from a low and a high part") {
      val low = XBeeAddress64_Low(0x15161718)
      val high = XBeeAddress64_High(0x11121314)
      low + high should be(XBeeAddress64(0x1112131415161718L))
    }
  }
  
  describe("XBeeAddress16") {
    it("should have an equals method based on the value") {
      XBeeAddress16(123) should be(XBeeAddress16(123))
      XBeeAddress16(-1) should be(XBeeAddress16(-1))
      XBeeAddress16(0) should be(XBeeAddress16(0))
      
      XBeeAddress16(0) should not be(XBeeAddress16(1))
      XBeeAddress16(1) should not be(XBeeAddress16(0))
      XBeeAddress16(12) should not be(XBeeAddress16(13))
    }
    it("should have a nice toString method") {
      XBeeAddress16(0).toString should be("XBeeAddress16(0x0000)")
      XBeeAddress16(1).toString should be("XBeeAddress16(0x0001)")
      XBeeAddress16(-1).toString should be("XBeeAddress16(0xFFFF)")
      XBeeAddress16(Short.MaxValue).toString should be("XBeeAddress16(0x7FFF)")
    }
    it("should be addressable except if it is != -1") {
      XBeeAddress16(0).isAddressable should be(true)
      XBeeAddress16(1).isAddressable should be(true)
      XBeeAddress16(Short.MaxValue).isAddressable should be(true)
      XBeeAddress16(Short.MinValue).isAddressable should be(true)
    }
    it("should not be addressable if it's -1") {
      XBeeAddress16(-1).isAddressable should be(false)
    }
    it("should have a 'disabled' value") {
      XBeeAddress16Disabled should be(XBeeAddress16(-1))
      XBeeAddress16Disabled.hashCode should be(XBeeAddress16(-1).hashCode)
      XBeeAddress16Disabled.isAddressable should be(false)
      XBeeAddress16Disabled.toString should be("XBeeAddress16(0xFFFF)")
    }
  }
}
