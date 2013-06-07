package mtscheme

case class Env(val env: EnvT) {

  def addEntry(entry: (String, ExprT)): Env = env match {
    case h::t   => Env((h + entry) :: t)
    case List() => throw new IllegalArgumentException
  }

  def expand(): Env = Env(EnvMapT() :: env)

  def lookUp(s: String): Option[ExprT] = 
    env find (_ contains s) map (_(s))

  override def equals(that: Any) = that match {
    case Env(thatEnv) => env == thatEnv
    case _ => false
  }
}

object Env {
  def apply() = new Env(EnvT())
}