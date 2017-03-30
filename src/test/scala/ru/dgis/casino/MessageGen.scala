package ru.dgis.casino

import java.time.LocalDateTime

import org.scalacheck.Gen
import org.scalacheck.Arbitrary._
import ru.dgis.casino.Message.ClickType.{OnButton, OnLink}

object MessageGen {
  import ru.dgis.casino.Message._

  val clickTypeGen: Gen[ClickType] = Gen.oneOf(OnLink, OnButton)

  val clickGen: Gen[Click] = for {
    id <- arbLong
    ts <- LocalDateTime.now
    _type <- clickTypeGen
  } yield Click(id, ts, _type)

  val objectGen: Gen[Object] = for {
    id <- arbLong
    _type <- Gen.oneOf(Object.Type.Link, Object.Type.Button)
  } yield Object(id, _type)

  val impressionGen: Gen[Impression] = for {
    id <- arbLong
    ts <- LocalDateTime.now
    _object <- objectGen
  } yield Impression(id, ts, _object)

  val messageGen = Gen.oneOf(clickGen, impressionGen)
}
