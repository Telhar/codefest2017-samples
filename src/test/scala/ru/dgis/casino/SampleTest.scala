package ru.dgis.casino

import org.scalatest.{FreeSpecLike, Matchers}
import ru.dgis.casino.Message.ClickType
import ru.dgis.casino.MessageMutator._

class SampleTest extends FreeSpecLike with Matchers {
  val context = Id.oneOf(123L, 456L) ++ Type(ClickType.OnButton) ++ MObject.none

  implicit def clickGen = MessageGen.clickGen
  implicit def impressionGen = MessageGen.impressionGen

  "Test" - {
    "can generate click" in {
      val c = context.click.get
      List(123L, 456L) should contain (c.id)
      c.`type` shouldBe ClickType.OnButton
      c.`object` shouldBe empty
    }
    "can generate impression" in {
      val i = context.impression.get
      List(123L, 456L) should contain (i.id)
    }
  }
}
