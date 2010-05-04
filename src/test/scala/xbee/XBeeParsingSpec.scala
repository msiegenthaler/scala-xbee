package ch.inventsoft.xbee

import org.scalatest._
import matchers._
import XBeeParsing._

class XBeeParsingSpec extends Spec with ShouldMatchers {

  describe("UnescapedPacket") {
    it("should be possible to parse a XBee packet") {
      val data = 0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x52 :: 0x4D :: 0x59 :: 0xFF :: Nil map(_.toByte)
      val payload = 0x08 :: 0x52 :: 0x4D :: 0x59 :: Nil map (_.toByte)

      data match { 
        case UnescapedPacket(p, rest) =>
          p should be(payload)
          rest should be(Nil)
        case other => fail
      }
    }
    it("should be possible to parse a XBee packet with a useless suffix") {
      val data = 0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x52 :: 0x4D :: 0x59 :: 0xFF :: 0x21 :: 0x24 :: Nil map(_.toByte)
      val payload = 0x08 :: 0x52 :: 0x4D :: 0x59 :: Nil map (_.toByte)

      data match { 
        case UnescapedPacket(p, rest) =>
          p should be(payload)
          rest should be(0x21 :: 0x24 :: Nil)
        case other => fail
      }
    }
    it("should not be possible to parse a XBee packet with invalid checksum") {
      val data = 0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x52 :: 0x4D :: 0x59 :: 0x04 :: Nil map(_.toByte)
      val payload = 0x08 :: 0x52 :: 0x4D :: 0x59 :: Nil map (_.toByte)

      data match { 
        case UnescapedPacket(p, rest) => fail
        case other => //ok
      }
    }
  }
  describe("UnescapedPacket skip") {  
    it("should not skip a valid complete packet") {
      val data = 0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x52 :: 0x4D :: 0x59 :: 0xFF :: Nil map(_.toByte)
      UnescapedPacket.skipInvalid(data) should be(SkipResult(data, Nil)) 
    }
    it("should not skip a valid incomplete packet") {
      val data = 0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x52 :: 0x4D :: 0x59 :: 0xFF :: Nil map(_.toByte)
      (1 to data.length).foreach{ i =>
        val d = data.take(i)
        UnescapedPacket.skipInvalid(d) should be(SkipResult(d, Nil))
      }
    }
    it("should skip an invalid one-byte prefix (with a complete packet)") {
      val junk = 0x12 :: Nil map(_.toByte)
      val data = 0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x52 :: 0x4D :: 0x59 :: 0xFF :: Nil map(_.toByte)
      UnescapedPacket.skipInvalid(junk ::: data) should be(SkipResult(data, junk)) 
    }
    it("should skip an invalid two-byte prefix (with and a complete packet)") {
      val junk = 0x12 :: 0x32 :: Nil map(_.toByte)
      val data = 0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x52 :: 0x4D :: 0x59 :: 0xFF :: Nil map(_.toByte)
      UnescapedPacket.skipInvalid(junk ::: data) should be(SkipResult(data, junk)) 
    }
    it("should skip an invalid one-byte prefix (with a incomplete packet)") {
      val junk = 0x12 :: Nil map(_.toByte)
      val data = 0x7e :: 0x00 :: 0x04 :: Nil map(_.toByte)
      UnescapedPacket.skipInvalid(junk ::: data) should be(SkipResult(data, junk)) 
    }
    it("should skip an invalid two-byte prefix (with a incomplete packet)") {
      val junk = 0x12 :: 0x01 :: Nil map(_.toByte)
      val data = 0x7e :: 0x00 :: 0x04 :: Nil map(_.toByte)
      UnescapedPacket.skipInvalid(junk ::: data) should be(SkipResult(data, junk)) 
    }
    it("should skip an invalid packet (invalid checksum)") {
      val data = 0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x52 :: 0x4D :: 0x59 :: 0xFE :: Nil map(_.toByte)
      UnescapedPacket.skipInvalid(data) should be(SkipResult(Nil, data)) 
    }
    it("should skip an invalid packet with junk prefix (invalid checksum)") {
      val data = 0x12 :: 0x13 :: 0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x52 :: 0x4D :: 0x59 :: 0xFE :: Nil map(_.toByte)
      UnescapedPacket.skipInvalid(data) should be(SkipResult(Nil, data)) 
    }
    it("should skip an invalid packet (invalid length)") {
      val data = 0x7e :: 0x12 :: 0x04 :: 0x08 :: 0x52 :: 0x4D :: 0x59 :: 0xFF :: Nil map(_.toByte)
      UnescapedPacket.skipInvalid(data) should be(SkipResult(Nil, data)) 
    }
    it("should skip an invalid packet (invalid length 2)") {
      val data = 0x7e :: 0x01 :: 0x04 :: 0x08 :: 0x52 :: 0x4D :: 0x59 :: 0xFF :: Nil map(_.toByte)
      UnescapedPacket.skipInvalid(data) should be(SkipResult(Nil, data)) 
    }
    it("should skip two invalid packets") {
      val data = 0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x52 :: 0x4D :: 0x59 :: 0xFE :: Nil map(_.toByte)
      UnescapedPacket.skipInvalid(data ::: data) should be(SkipResult(Nil, data ::: data)) 
    }
    it("should skip two invalid packets with a following valid packet") {
      val invalid = 0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x52 :: 0x4D :: 0x59 :: 0xFE :: Nil map(_.toByte)
      val valid = 0x7e :: 0x00 :: 0x04 :: 0x08 :: 0x52 :: 0x4D :: 0x59 :: 0xFF :: Nil map(_.toByte)
      UnescapedPacket.skipInvalid(invalid ::: invalid ::: valid) should be(SkipResult(valid, invalid ::: invalid)) 
    }
  }
  describe("ATMY") {
    it("should support sending a MY read command") {
      val expect = 0x08 :: 0x52 :: 0x4D :: 0x59 :: Nil
      AT.MY_read(FrameId(0x52)) should be(expect.map(_.toByte))
    }
    it("should support parsing a MY read command") {
      0x08 :: 0x52 :: 0x4D :: 0x59 :: Nil map(_.toByte) match {
        case AT.MY_read((frame), rest) =>
          frame should be(FrameId(0x52))
          rest should be(Nil)
        case _ => fail
      }
    }
    it("should support sending a MY write command") {
      val expect = 0x08 :: 0x52 :: 0x4D :: 0x59 :: 0x12 :: 0x23 :: Nil
      AT.MY_set(FrameId(0x52),XBeeAddress16(0x1223)) should be(expect.map(_.toByte))
    }
    it("should support parsing a MY write command") {
      0x08 :: 0x52 :: 0x4D :: 0x59 :: 0x12 :: 0x23 :: Nil map(_.toByte) match {
        case AT.MY_set((frame, address),rest) =>
          frame should be(FrameId(0x52))
          address should be(XBeeAddress16(0x1223))
          rest should be(Nil)
        case _ => fail
      }
    }
    it("should support sending an MY response (ok)") {
      val expect = 0x88 :: 0x52 :: 0x4d :: 0x59 :: 0x00 :: 0x23 :: 0x12 :: Nil map(_.toByte)
      AT.MY_response((FrameId(0x52),AT.StatusOk),XBeeAddress16(0x2312)) should be(expect)
    }
    it("should support sending an MY response (error)") {
      val expect = 0x88 :: 0x51 :: 0x4d :: 0x59 :: 0x01 :: 0x00 :: 0x00 :: Nil map(_.toByte)
      AT.MY_response((FrameId(0x51),AT.StatusError),XBeeAddress16(0)) should be(expect)
    }
    it("should support parsing an MY response (ok)") {
      0x88 :: 0x52 :: 0x4d :: 0x59 :: 0x00 :: 0x23 :: 0x12 :: Nil map(_.toByte) match {
        case AT.MY_response(((frame,status),address),rest) =>
          frame should be(FrameId(0x52))
          status should be(AT.StatusOk)
          address should be(XBeeAddress16(0x2312))
      }
    }
  }
  describe("ATDH") {
    it("should support sending a DH read command") {
      val expect = 0x08 :: 0x01 :: 0x44 :: 0x48 :: Nil map(_.toByte)
      AT.DH_read(FrameId(0x01)) should be(expect)
    }
    it("should support parsing a DH read command") {
      0x08 :: 0x01 :: 0x44 :: 0x48 :: Nil map(_.toByte) match {
        case AT.DH_read(frame, Nil) =>
          frame should be(FrameId(0x01))
        case _ => fail
      }
    }
    it("should support sending a DH write command") {
      val expect = 0x08 :: 0x01 :: 0x44 :: 0x48 :: 0x01 :: 0x02 :: 0x03 :: 0x04 :: Nil map(_.toByte)
      val address = XBeeAddress64(0x0102030405060708L).highPart
      AT.DH_set(FrameId(0x01),address) should be(expect)
    }
    it("should support parsing a DH write command") {
      0x08 :: 0x01 :: 0x44 :: 0x48 :: 0x01 :: 0x02 :: 0x03 :: 0x04 :: Nil map(_.toByte) match {
        case AT.DH_set((frame, address), Nil) =>
          frame should be(FrameId(0x01))
          address should be(XBeeAddress64_High(0x01020304))
        case _ => fail
      }
    }
    it("should support sending a DH response (ok)") {
      val expect = 0x88 :: 0x01 :: 0x44 :: 0x48 :: 0x00 :: 0x01 :: 0x02 :: 0x03 :: 0x04 :: Nil map(_.toByte)
      val address = XBeeAddress64(0x0102030405060708L).highPart
      AT.DH_response(((FrameId(0x01),AT.StatusOk),address)) should be(expect)
    }
    it("should support parsing a DH response") {
      0x88 :: 0x01 :: 0x44 :: 0x48 :: 0x00 :: 0x01 :: 0x02 :: 0x03 :: 0x04 :: Nil map(_.toByte) match {
        case AT.DH_response(((frame,status), address), Nil) =>
          frame should be(FrameId(0x01))
          status should be(AT.StatusOk)
          address should be(XBeeAddress64_High(0x01020304))
        case _ => fail
      }
    }
  }
  describe("ATDL") {
    it("should support sending a DL read command") {
      val expect = 0x08 :: 0x02 :: 0x44 :: 0x4C :: Nil map(_.toByte)
      AT.DL_read(FrameId(0x02)) should be(expect)
    }
    it("should support parsing a DL read command") {
      0x08 :: 0x02 :: 0x44 :: 0x4C :: Nil map(_.toByte) match {
        case AT.DL_read(frame, Nil) =>
          frame should be(FrameId(0x02))
        case _ => fail
      }
    }
    it("should support sending a DL write command") {
      val expect = 0x08 :: 0x01 :: 0x44 :: 0x4C :: 0x05 :: 0x06 :: 0x07 :: 0x08 :: Nil map(_.toByte)
      val address = XBeeAddress64(0x0102030405060708L).lowPart
      AT.DL_set(FrameId(0x01),address) should be(expect)
    }
    it("should support parsing a DL write command") {
      0x08 :: 0x01 :: 0x44 :: 0x4C :: 0x05 :: 0x06 :: 0x07 :: 0x08 :: Nil map(_.toByte) match {
        case AT.DL_set((frame, address), Nil) =>
          frame should be(FrameId(0x01))
          address should be(XBeeAddress64_Low(0x05060708))
        case _ => fail
      }
    }
    it("should support sending a DL response (ok)") {
      val expect = 0x88 :: 0x01 :: 0x44 :: 0x4C :: 0x00 :: 0x05 :: 0x06 :: 0x07 :: 0x08 :: Nil map(_.toByte)
      val address = XBeeAddress64(0x0102030405060708L).lowPart
      AT.DL_response(((FrameId(0x01),AT.StatusOk),address)) should be(expect)
    }
    it("should support parsing a DL response") {
      0x88 :: 0x01 :: 0x44 :: 0x4C :: 0x00 :: 0x05 :: 0x06 :: 0x07 :: 0x08 :: Nil map(_.toByte) match {
        case AT.DL_response(((frame,status), address), Nil) =>
          frame should be(FrameId(0x01))
          status should be(AT.StatusOk)
          address should be(XBeeAddress64_Low(0x05060708))
        case _ => fail
      }
    }
  }
  describe("TX") {
    it("should support sending data to 64 bit addresses") {
      val data = 0x12 :: 0x32 :: 0x11 :: 0x19 :: 0x60 :: 0xFF :: 0xFE :: Nil map(_.toByte)
      val expect = 0x00 :: 0x01 :: 0x01 :: 0x02 :: 0x03 :: 0x04 :: 0x05 :: 0x06 :: 0x07 :: 0x08 :: 0x00 :: Nil map(_.toByte)
      TX64(FrameId(0x01), XBeeAddress64(0x0102030405060708L), TransmitOptionNormal, data) should be(expect ::: data)
    }
    it("should support sending data to 16 bit addresses") {
      val data = 0x12 :: 0x32 :: 0x11 :: 0x19 :: 0x60 :: 0xFF :: 0xFE :: Nil map(_.toByte)
      val expect = 0x01 :: 0x01 :: 0x12 :: 0x13 :: 0x00 :: Nil map(_.toByte)
      TX16(FrameId(0x01), XBeeAddress16(0x1213), TransmitOptionNormal, data) should be(expect ::: data)
    }
    it("should support receiving ok responses") {
      0x89 :: 0x53 :: 0x00 :: Nil map(_.toByte) match {
        case TX_status((frame,status),rest) =>
          frame should be(FrameId(0x53))
          status should be(TransmitStatusSuccess)
        case _ => fail
      }
    }
    it("should support receiving nack responses") {
      0x89 :: 0x01 :: 0x01 :: Nil map(_.toByte) match {
        case TX_status((frame,status),rest) =>
          frame should be(FrameId(0x01))
          status should be(TransmitStatusNoAckReceived)
        case _ => fail
      }
    }
  }
  describe("RX") {
    it("should be able to serialize a 64-bit RX message") {
      val payload = 0x12 :: 0x32 :: 0x11 :: 0x19 :: 0x60 :: 0xFF :: 0xFE :: Nil map(_.toByte)
      val expect = (0x80 :: 0x01 :: 0x02 :: 0x03 :: 0x04 :: 0x05 :: 0x06 :: 0x07 :: 0x08 :: 0x28 :: 0x00 :: Nil).map(_.toByte) ::: payload
      RX64(XBeeAddress64(0x0102030405060708L),SignalStrength(-40),ReceiveOption(false,false),payload) should be(expect)
    }
    it("should be able to receive messages from 64-bit addresses") {
      val payload = 0x12 :: 0x32 :: 0x11 :: 0x19 :: 0x60 :: 0xFF :: 0xFE :: Nil map(_.toByte)
      val data = (0x80 :: 0x01 :: 0x02 :: 0x03 :: 0x04 :: 0x05 :: 0x06 :: 0x07 :: 0x08 :: 0x28 :: 0x00 :: Nil).map(_.toByte) ::: payload
      data match {
        case RX64((source,signal,option,data),Nil) =>
          source should be(XBeeAddress64(0x0102030405060708L))
          signal should be(SignalStrength(-40))
          option.addressBroadcast should be(false)
          option.panBroadcast should be(false)
          data should be(payload)
        case _ => fail
      }
    }
    it("should be able to receive broadcast messages from 64-bit addresses") {
      val payload = 0x12 :: 0x32 :: 0x11 :: 0x19 :: 0x60 :: 0xFF :: 0xFE :: Nil map(_.toByte)
      val data = (0x80 :: 0x01 :: 0x02 :: 0x03 :: 0x04 :: 0x05 :: 0x06 :: 0x07 :: 0x08 :: 0x28 :: 0x02 :: Nil).map(_.toByte) ::: payload
      data match {
        case RX64((source,signal,option,data),Nil) =>
          source should be(XBeeAddress64(0x0102030405060708L))
          signal should be(SignalStrength(-40))
          option.addressBroadcast should be(true)
          option.panBroadcast should be(false)
          data should be(payload)
        case _ => fail
      }
    }
    it("should be able to receive pan broadcast messages from 64-bit addresses") {
      val payload = 0x12 :: 0x32 :: 0x11 :: 0x19 :: 0x60 :: 0xFF :: 0xFE :: Nil map(_.toByte)
      val data = (0x80 :: 0x01 :: 0x02 :: 0x03 :: 0x04 :: 0x05 :: 0x06 :: 0x07 :: 0x08 :: 0x28 :: 0x04 :: Nil).map(_.toByte) ::: payload
      data match {
        case RX64((source,signal,option,data),Nil) =>
          source should be(XBeeAddress64(0x0102030405060708L))
          signal should be(SignalStrength(-40))
          option.addressBroadcast should be(false)
          option.panBroadcast should be(true)
          data should be(payload)
        case _ => fail
      }
    }
    
    it("should be able to serialize a 16-bit RX message") {
      val payload = 0x12 :: 0x32 :: 0x11 :: 0x19 :: 0x60 :: 0xFF :: 0xFE :: Nil map(_.toByte)
      val expect = (0x81 :: 0x12 :: 0x13 :: 0x29 :: 0x00 :: Nil).map(_.toByte) ::: payload
      RX16(XBeeAddress16(0x1213),SignalStrength(-41),ReceiveOption(false,false),payload) should be(expect)
    }
    it("should be able to receive messages from 16-bit addresses") {
      val payload = 0x12 :: 0x32 :: 0x11 :: 0x19 :: 0x60 :: 0xFF :: 0xFE :: Nil map(_.toByte)
      val data = (0x81 :: 0x12 :: 0x13 :: 0x29 :: 0x00 :: Nil).map(_.toByte) ::: payload
      data match {
        case RX16((source,signal,option,data),Nil) =>
          source should be(XBeeAddress16(0x1213))
          signal should be(SignalStrength(-41))
          option.addressBroadcast should be(false)
          option.panBroadcast should be(false)
          data should be(payload)
        case _ => fail
      }
    }
    it("should be able to receive broadcast messages from 16-bit addresses") {
      val payload = 0x12 :: 0x32 :: 0x11 :: 0x19 :: 0x60 :: 0xFF :: 0xFE :: Nil map(_.toByte)
      val data = (0x81 :: 0x12 :: 0x13 :: 0x29 :: 0x02 :: Nil).map(_.toByte) ::: payload
      data match {
        case RX16((source,signal,option,data),Nil) =>
          source should be(XBeeAddress16(0x1213))
          signal should be(SignalStrength(-41))
          option.addressBroadcast should be(true)
          option.panBroadcast should be(false)
          data should be(payload)
        case _ => fail
      }
    }
    it("should be able to receive pan broadcast messages from 16-bit addresses") {
      val payload = 0x12 :: 0x32 :: 0x11 :: 0x19 :: 0x60 :: 0xFF :: 0xFE :: Nil map(_.toByte)
      val data = (0x81 :: 0x12 :: 0x13 :: 0x29 :: 0x04 :: Nil).map(_.toByte) ::: payload
      data match {
        case RX16((source,signal,option,data),Nil) =>
          source should be(XBeeAddress16(0x1213))
          signal should be(SignalStrength(-41))
          option.addressBroadcast should be(false)
          option.panBroadcast should be(true)
          data should be(payload)
        case _ => fail
      }
    }
    it("should not match a SH-response") {
      val data = List(-120, 1, 83, 72, 0, 1, 2, 3, 4) map(_.toByte)
      RX64.unapply(data) should be(None)
    }
  }
  
  describe("ATND") {
    it("should support sending a ND command") {
      val expect = 0x08 :: 0x01 :: 'N'.toInt :: 'D'.toInt :: Nil map(_.toByte)
      val command = AT.ND(FrameId(0x01))
      command should be(expect);
    }
    it("should support parsing a ND command") {
      val data = 0x08 :: 0x01 :: 'N'.toInt :: 'D'.toInt :: Nil map(_.toByte)
      data match { 
        case AT.ND((frame), rest) =>
          frame should be(FrameId(0x01))
        case other => fail
      }
    }
    
    it("should serialize a null terminated string") {
      val expect = 0x4D :: 0x61 :: 0x72 :: 0x69 :: 0x6F :: 0 :: Nil map(_.toByte)
      null_terminated_ascii_string("Mario") should be(expect)
    }
    it("should parse a null terminated string (terminated)") {
      val data = 0x4D :: 0x61 :: 0x72 :: 0x69 :: 0x6F :: 0 :: Nil map(_.toByte)
      data match {
        case null_terminated_ascii_string((text),rest) =>
          text should be("Mario")
          rest should be(Nil)
        case other => fail
      }
    }
    it("should parse a null terminated string (unterminated)") {
      val data = 0x4D :: 0x61 :: 0x72 :: 0x69 :: 0x6F :: Nil map(_.toByte)
      data match {
        case null_terminated_ascii_string((text),rest) =>
          text should be("Mario")
          rest should be(Nil)
        case other => fail
      }
    }
    
    it("should be able to parse a ND node response") {
      val data = 0x88 :: 0x03 :: 'N'.toInt :: 'D'.toInt :: 0x00 ::
        0x12 :: 0x13 :: //MY
        0x01 :: 0x02 :: 0x03 :: 0x04 :: 0x05 :: 0x06 :: 0x07 :: 0x08 :: // serial
        40 :: //signal
        'H'.toInt :: 'A'.toInt :: 'M'.toInt :: 0x00 :: // node id 
        Nil map(_.toByte)

      data match {
        case AT.ND_node(((frame, status), a1, a2, signal, id), rest) =>
          frame should be(FrameId(0x03))
          status should be(AT.StatusOk)
          a1 should be(XBeeAddress16(0x1213))
          a2 should be(XBeeAddress64(0x0102030405060708L))
          signal should be(SignalStrength(-40))
          id should be("HAM")
          rest should be(Nil)
        case other => fail
      }
    }
    it("should be able to parse a ND end response") {
      val data = 0x88 :: 0x03 :: 'N'.toInt :: 'D'.toInt :: 0x00 :: Nil map(_.toByte)
      data match {
        case AT.ND_end((frame, status),rest) =>
          frame should be(FrameId(0x03))
          status should be(AT.StatusOk)
          rest should be(Nil)
        case other => fail
      }
    }
  }
}
