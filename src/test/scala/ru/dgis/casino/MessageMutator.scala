package ru.dgis.casino

import java.time.LocalDateTime

import monocle.Lens
import monocle.macros.GenLens
import org.scalacheck.Gen

object MessageMutator {
  import MessageGen._
  import Message._

  object Id extends SimpleContext[Long] {
    override val clickLens = clickFieldLens(_.id, v => o => o.copy(id = v))
    override val impressionLens = impressionFieldLens(_.id, v => o => o.copy(id = v))
    val gen = idGen
  }

  object Ts extends SimpleContext[LocalDateTime] {
    override val clickLens = clickFieldLens(_.ts, v => o => o.copy(ts = v))
    override val impressionLens = impressionFieldLens(_.ts, v => o => o.copy(ts = v))
    val gen = tsGen
  }

  object Type extends SimpleContext[ClickType] {
    override val clickLens = clickFieldLens(_.`type`, v => o => o.copy(`type` = v))
    val gen = clickTypeGen
  }

  object MObject extends OptionContext[Object] {
    private def _gen = gen.retryUntil(_.nonEmpty).sample.get.get
    private val optionObjectLens = Lens[Object, Option[Object]](Some.apply)(o => _ => o.getOrElse(_gen))

    override val clickLens = clickFieldLens(_.`object`, v => o => o.copy(`object` = v))
    override val impressionLens = Some(GenLens[Impression](_.`object`) composeLens optionObjectLens)
    val gen = Gen.option(objectGen)
  }

  object ObjectType extends OptionContext[Object.Type] {
    private val clickObjectLens = GenLens[Click](_.`object`)
    private val optionObjectTypeLens = Lens[Option[Object], Option[Object.Type]](_.map(_.`type`))(oot => oo => oot.flatMap(ot => oo.map(o => o.copy(`type` = ot))))
    private val objectTypeLens = GenLens[Object](_.`type`)
    private val objectTypeOptionLens = Lens[Object.Type, Option[Object.Type]](Some.apply)(o => oot => o.getOrElse(oot))

    override val clickLens = Some(clickObjectLens composeLens  optionObjectTypeLens)
    override val impressionLens = Some(GenLens[Impression](_.`object`) composeLens objectTypeLens composeLens objectTypeOptionLens)
    val gen = Gen.option(objectTypeGen)
  }

  object ObjectId extends OptionContext[Long] {
    private val clickObjectLens = GenLens[Click](_.`object`)
    private val optionObjectIdLens = Lens[Option[Object], Option[Long]](_.map(_.id))(oot => oo => oot.flatMap(ot => oo.map(o => o.copy(id = ot))))
    private val objectIdLens = GenLens[Object](_.id)
    private val objectIdOptionLens = Lens[Long, Option[Long]](Some.apply)(o => oot => o.getOrElse(oot))

    override val clickLens = Some(clickObjectLens composeLens optionObjectIdLens)
    override val impressionLens = Some(GenLens[Impression](_.`object`) composeLens objectIdLens composeLens objectIdOptionLens)
    val gen = Gen.option(idGen)
  }
}
