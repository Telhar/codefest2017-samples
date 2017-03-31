package ru.dgis.casino

import java.time.LocalDateTime

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import ru.dgis.casino.Message.ClickType.{OnButton, OnLink}

object MessageGen {
  import ru.dgis.casino.Message._

  val idGen = arbLong.arbitrary
  val tsGen = Gen.const(LocalDateTime.now)
  val clickTypeGen: Gen[ClickType] = Gen.oneOf(OnLink, OnButton)
  val objectTypeGen: Gen[Object.Type] = Gen.oneOf(Object.Type.Link, Object.Type.Button)
  val objectGen: Gen[Object] = for {
    id <- idGen
    _type <- objectTypeGen
  } yield Object(id, _type)

  val clickGen: Gen[Click] = for {
    id <- idGen
    ts <- tsGen
    _type <- clickTypeGen
    _object <- Gen.option(objectGen)
  } yield Click(id, ts, _type, _object)

  val impressionGen: Gen[Impression] = for {
    id <- idGen
    ts <- tsGen
    _object <- objectGen
  } yield Impression(id, ts, _object)

  val messageGen = Gen.oneOf(clickGen, impressionGen)
}
