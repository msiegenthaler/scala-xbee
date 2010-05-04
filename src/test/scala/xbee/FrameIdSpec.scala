package ch.inventsoft.xbee

import org.scalatest._
import matchers._

class FrameIdSpec extends Spec with ShouldMatchers {
  describe("FrameId") {
    it("should be possible to be create one with a default value of 1") {
      val id = FrameId()
      id.value should be(1)
    }
    it("should be possible to create one with each value between 1 and 255") {
      (1 to 255).foreach(i => {
        val n = i.toByte
        val id = FrameId(n)
        id.value should be(n)
      })
    }
    it("should be possible to create one with a value of 0 as NoFrameId") {
      FrameId(0) should be(NoFrameId)
      FrameId(0).next should be(NoFrameId)
    }
    it("should be equal to another instance with the same value") {
      FrameId(12) should be(FrameId(12))
      FrameId(12).hashCode should be(FrameId(12).hashCode)
      FrameId(-127) should be(FrameId(-127))
      FrameId(-127).hashCode should be(FrameId(-127).hashCode)
    }
    it("should not be equal to another instance with another value") {
      FrameId(12) should not be(FrameId(13))
      FrameId(13) should not be(FrameId(12))
      FrameId(-10) should not be(FrameId(10))
    }
    it("should have a next") {
      val o = FrameId()
      o should not be(o.next)
    }
    it("should support 255 different values when using next") {
      val ids = (1 to 255).map { i => 
        (1 to i).foldLeft(FrameId())((id,_) => id.next)
      }
      ids.foreach( id =>
        ids.filter(_ == id).size should be(1)
      )
    }
    it("should be cyclic after 255 values") {
      val o = FrameId()
      val l = (1 to 255).foldLeft(o)((id,_) => id.next)
      o should be(l)
    }
    it("should work forever") {
      val x = (1 to 100000).foldLeft(FrameId())((id,_) => id.next)
      x should be(FrameId(41))
    }
    it("should not have a receipt") {
      FrameId(1).hasReceipt should be(true)
      FrameId(10).hasReceipt should be(true)
      FrameId().hasReceipt should be(true)
      FrameId().next.hasReceipt should be(true)
    }
    it("should not have a receipt when it's 0") {
      FrameId(0).hasReceipt should be(false)
      FrameId(0).next.hasReceipt should be(false)
    }
    it("should be unapplyable") {
      FrameId(12) match {
        case FrameId(value) => value should be(12)
        case _ => fail
      }
    }
  }
  describe("NoFrameId") {
    it("should have a value of 0") {
      NoFrameId.value should be(0)
    }
    it("should be equal to itself") {
      NoFrameId should be(NoFrameId)
      NoFrameId.hashCode should be(NoFrameId.hashCode)
    }
    it("should not have a receipt") {
      NoFrameId.hasReceipt should be(false)
    }
    it("should return itself as next") {
      NoFrameId.next should be(NoFrameId)
      NoFrameId.next.next should be(NoFrameId)
    }
  }
}
