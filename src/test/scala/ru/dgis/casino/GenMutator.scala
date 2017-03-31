package ru.dgis.casino

import scala.language.implicitConversions
import org.scalacheck.Gen
import GenUtils._

object GenUtils {
  implicit def sample[T](gen: Gen[T]): T = gen.sample.get
}

/** General trait to compose generator mutations */
trait GenMutator[T] extends (Gen[T] => Gen[T]) {
  def gen(implicit g: Gen[T]): Gen[T] = apply(g)
  def get(implicit g: Gen[T]): T = this.gen.sample.get

  def ++(another: GenMutator[T]): GenMutator[T] =
    this.apply _ andThen another.apply
}

object GenMutator {
  implicit def fromGenerator[T](m: Gen[T] => Gen[T]): GenMutator[T] =
    new GenMutator[T] {
      override def apply(v1: Gen[T]) = m(v1)
    }

  def mutator[T](m: T => T): GenMutator[T] =
    new GenMutator[T] {
      override def apply(v1: Gen[T]) = v1.map(m)
    }

  def empty[T] = fromGenerator[T](x => x)
}