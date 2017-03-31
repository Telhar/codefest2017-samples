package ru.dgis.casino

import java.time.LocalDateTime

import org.scalacheck.Gen

object MessageMutator {
  import MessageGen._
  import Message._

  object Id extends SimpleContext[Long] {
    override val clickLens = ClickLens((a, v) => a.copy(id = v), _.id)
    override val impressionLens = ImpressionLens((a, v) => a.copy(id = v), _.id)
    val gen = idGen
  }

  object Ts extends SimpleContext[LocalDateTime] {
    override val clickLens = ClickLens((a, v) => a.copy(ts = v), _.ts)
    override val impressionLens = ImpressionLens((a, v) => a.copy(ts = v), _.ts)
    val gen = tsGen
  }

  object Type extends SimpleContext[ClickType] {
    override val clickLens = ClickLens((a, v) => a.copy(`type` = v), _.`type`)
    val gen = clickTypeGen
  }

  object MObject extends OptionContext[Object] {
    def getNonEmptyObject = gen.retryUntil(_.nonEmpty).sample.get.get
    override val clickLens = ClickLens((a, v) => a.copy(`object` = v), _.`object`)
    override val impressionLens = ImpressionLens(
      (a, v) => a.copy(`object` = v.getOrElse(getNonEmptyObject)),
      a => Some(a.`object`)
    )
    val gen: Gen[Option[Object]] = Gen.option(objectGen)
  }

  // ObjectType, ObjectId
}
