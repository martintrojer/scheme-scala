package mtscheme

// ----------------------------------------
// Case Types

sealed trait ValueType
case class Num(v: Double) extends ValueType
case class Bool(v: Boolean) extends ValueType
case class Name(v: String) extends ValueType

sealed trait ListType
case class LValue(v: ValueType) extends ListType
case class LList(v: ListType) extends ListType

sealed trait Expression
case class NullExpr() extends Expression
case class Combination(v: List[Expression]) extends Expression
case class EList(v: List[ListType]) extends Expression
case class Function(args: List[ListType], body: List[Expression]) extends Expression
//case class Procedure(...) extends Expression
case class Symbol(v: String) extends Expression
case class Value(v: ValueType) extends Expression

// ----------------------------------------
class Interpreter

object Interpreter {

  // ----------------------------------------
  // Environment stuff

  // It could be argued that this should sit in an Env class, but I found
  // it very unnatural to wrap the Env type below in another class which
  // really ISN'T an environment. I.e. an env is a List[Map[String,Expression]],
  // not another class with a member with that signature!

  type Env = List[EnvMap]
  type EnvMap = Map[String,Expression]
  def EnvMap(xs: (String, Expression)*) = Map(xs:_*)

  def setEnv(entry: (String, Expression), env: Env): Env = env match {
    case h::t   => (h + entry) :: t
    case List() => throw new IllegalArgumentException
  }

  def expandEnv(env: Env): Env = EnvMap() :: env

  def lookUp(s: String, env: Env): Option[Expression] = env match {
    case List() => None
    case h :: t => (h get s) match {
      case Some(e) => Some(e)
      case None    => lookUp(s, t)
    }
  }

}
