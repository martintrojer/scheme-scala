package mtscheme

case class Environment(val env: Env) {

  def addEntry(entry: (String, Expression)): Environment = env match {
    case h::t   => Environment((h + entry) :: t)
    case List() => throw new IllegalArgumentException
  }

  def expand(): Environment = Environment(EnvMap() :: env)

  def lookUp(s: String): Option[Expression] = {
    def lu(env: Env): Option[Expression] = env match {
      case List() => None
      case h :: t => (h get s) match {
        case Some(e) => Some(e)
        case None    => lu(t)
      }
    }
    lu(env)
  }

  override def equals(that: Any) = that match {
    case Environment(thatEnv) => env == thatEnv
    case _ => false
  }
}

object Environment {
  def apply() = new Environment(Env())
}