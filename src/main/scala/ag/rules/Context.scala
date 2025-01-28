package ag.rules

case class Context[A](state: State, rule: Rule[?, A], old: Option[A]) {}

object Context {
  def old[A](using ctx: Context[A]): Option[A] = ctx.old
  def rule[A](using ctx: Context[A]): Rule[?, A] = ctx.rule
}
