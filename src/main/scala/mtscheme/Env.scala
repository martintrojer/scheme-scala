package mtscheme

case class Env(val env: EnvT) {

  def addEntry(entry: (String, ExprT)): Env = env match {
    case h::t   => Env((h + entry) :: t)
    case List() => throw new IllegalArgumentException
  }

  def expand(): Env = Env(EnvMapT() :: env)

  def lookUp(s: String): Option[ExprT] = {
    def lu(env: EnvT): Option[ExprT] = env match {
      case List() => None
      case h :: t => (h get s) match {
        case Some(e) => Some(e)
        case None    => lu(t)
      }
    }
    lu(env)
  }

  override def equals(that: Any) = that match {
    case Env(thatEnv) => env == thatEnv
    case _ => false
  }
}

object Env {
  def apply() = new Env(EnvT())
}