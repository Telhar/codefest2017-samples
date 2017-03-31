package ru.dgis.casino

import java.time.LocalDateTime

trait Message

object Message {
  abstract class ClickType(name: String)
  object ClickType {
    case object OnLink extends ClickType("on_link")
    case object OnButton extends ClickType("on_button")
  }

  case class Object(id: Long, `type`: Object.Type)
  object Object {
    sealed trait Type
    object Type {
      case object Link extends Type
      case object Button extends Type
    }
  }

  case class Click(id: Long, ts: LocalDateTime, `type`: ClickType, `object`: Option[Object]) extends Message
  case class Impression(id: Long, ts: LocalDateTime, `object`: Object) extends Message
}
