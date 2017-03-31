package ru.dgis.casino

import org.scalatest.{FreeSpecLike, Matchers}
import ru.dgis.casino.Message.ClickType
import ru.dgis.casino.MessageMutator._

class SampleTest extends FreeSpecLike with Matchers {
  val context = Id(123L) ++ Type(ClickType.OnButton) ++ MObject.none

  implicit def clickGen = MessageGen.clickGen
  implicit def impressionGen = MessageGen.impressionGen

  "Test" - {
    "can generate click" in {
      val c = context.click.get
      c.id shouldBe 123L
      c.`type` shouldBe ClickType.OnButton
      c.`object` shouldBe empty
    }
    "can generate impression" in {
      val i = context.impression.get
      i.id shouldBe 123L
    }
  }
}
