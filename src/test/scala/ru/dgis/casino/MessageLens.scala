package ru.dgis.casino

import org.scalacheck.Gen
import ru.dgis.casino.Message._
import ru.dgis.casino.GenMutator._

import scala.language.postfixOps
import scalaz.Lens

trait MessageLens[T] {
  type ClickLens[A] = Lens[Click, A]
  type ImpressionLens[A] = Lens[Impression, A]

  object ClickLens {
    def apply(set: (Click, T) => Click, get: (Click) => T) =
      Some(Lens.lensu[Click, T] (set, get))
  }

  object ImpressionLens {
    def apply(set: (Impression, T) => Impression, get: (Impression) => T) =
      Some(Lens.lensu[Impression, T] (set, get))
  }
}

trait LensContext[T] extends MessageLens[T] {
  val clickLens: Option[ClickLens[T]] = None
  val impressionLens: Option[ImpressionLens[T]] = None
  def gen: Gen[T]

  protected def lensCheck[M](l: Option[Lens[M, T]], o: M, check: (T) => Boolean) = l match {
    case None => true
    case Some(lens) => check(lens.get(o))
  }
  private def lensSet[M](l: Option[Lens[M, T]], o: M, v: T) = l match {
    case None => o
    case Some(lens) => lens.set(o, v)
  }

  def apply(v: T) = MessageContext(
    click = mutator[Click](o => lensSet(clickLens, o, v)),
    impression = mutator[Impression](o => lensSet(impressionLens, o, v))
  )

  def apply(): MessageContext = this.apply(gen.sample.get)
}

trait WithNoneAndDefined[T] {
  self: LensContext[Option[T]] =>

  def none: MessageContext = this.apply(None)
  def defined: MessageContext = MessageContext(
    click = (gen: Gen[Click]) => gen.retryUntil(o => lensCheck(clickLens, o, _.isDefined)),
    impression = (gen: Gen[Impression]) => gen.retryUntil(o => lensCheck(impressionLens, o, _.isDefined))
  )
}

trait WithConditions[T] {
  self: LensContext[T] =>

  def apply(g: Gen[T]): MessageContext = self.apply(g.sample.get)
  def neq(list: T*) = self.apply(self.gen.retryUntil(v => !list.contains(v)))
  def oneOf(list: T*) = self.apply(Gen.oneOf(list))
  def uniqueStream() = {
    def loop(values: Set[T]): Stream[MessageContext] = {
      val value = self.gen.retryUntil(!values.contains(_)).sample.get
      self.apply(value) #:: loop(values + value)
    }
    loop(Set.empty[T])
  }
  def uniqueList(size: Int) = uniqueStream take size toList
  def uniquePair() = uniqueStream take 2 toList match { case List(c1, c2) => (c1, c2) }
}

trait SimpleContext[T] extends LensContext[T] with WithConditions[T]

trait OptionContext[T] extends LensContext[Option[T]] with WithConditions[Option[T]] with WithNoneAndDefined[T]
