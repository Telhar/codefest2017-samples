package ru.dgis.casino

import org.scalacheck.Gen

import scala.language.implicitConversions

trait MutatorsUtils {
  implicit def valueToOption[T](v: T): Option[T] =
    Some(v)

  implicit def genValueToGenOption[T](g: Gen[T]): Gen[Option[T]] =
    g.map(Some.apply)

  implicit def seqOfValuesToSeqOfOption[T](s: Seq[T]): Seq[Option[T]] =
    s.map(Some.apply)

  implicit def listOfValuesToListOfOption[T](l: List[T]): List[Option[T]] =
    l.map(Some.apply)
}
