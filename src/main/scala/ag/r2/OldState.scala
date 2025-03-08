package ag.r2

enum OldState[+A] {
  case Current(old_value: Saved[A])
  case Expired(old_value: Saved[A])
  case Missing

  def old_value_opt: Option[Saved[A]] = this match {
    case Current(v) => Some(v)
    case Expired(v) => Some(v)
    case Missing    => None
  }
}
