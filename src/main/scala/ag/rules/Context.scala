package ag.rules

import language.experimental.namedTuples

import scala.caps.Capability

case class Context[A](state: State, rule: Rule[?, A], old: Option[A])
    extends Capability {}

object Context {
  def old[A](using ctx: Context[A]): Option[A] = ctx.old
  def rule[A](using ctx: Context[A]): Rule[?, A] = ctx.rule
}
