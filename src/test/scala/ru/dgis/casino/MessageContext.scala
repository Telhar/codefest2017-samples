package ru.dgis.casino

import ru.dgis.casino.Message._

import scala.language.implicitConversions

/**
  * Message Generators context
  * Wires together all mutations for messages generators
  */
case class MessageContext (
                        click: GenMutator[Click] = GenMutator.empty,
                        impression: GenMutator[Impression] = GenMutator.empty
                      ) {
  def ++(context: MessageContext) = copy(
    click ++ context.click,
    impression ++ context.impression
  )
}

object MessageContext {

  implicit def fromClick(m: GenMutator[Click]): MessageContext = MessageContext(click = m)
  implicit def fromImpression(m: GenMutator[Impression]): MessageContext = MessageContext(impression = m)

  val empty = MessageContext()
}
