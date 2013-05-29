package mtscheme

// ----------------------------------------
// Case Types

sealed trait ValueType
case class Num(v: BigDecimal) extends ValueType
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
case class Procedure(f: ((Environment, List[Expression]) => (Environment, Expression))) extends Expression
case class Symbol(v: String) extends Expression
case class Value(v: ValueType) extends Expression

// ----------------------------------------

object Interpreter {

  def eval(env: Environment, expr: Expression): (Environment, Expression) = expr match {
    case NullExpr()           => throw new IllegalStateException("invalid interpreter state")
    case Combination(List())  => throw new IllegalStateException("invalid combination")
    case Combination(h :: t)  =>
      eval(env, h) match {
        case (nenv, Procedure(f))           => apply(f, t, env)
        case (nenv, Function(args, body))   => {
          if (args.length != t.length) throw new IllegalArgumentException("invalid number or arguments")
          val newEnv = (args zip t).foldLeft(nenv.expand())((acc, av) => bindArg(acc, av._1, av._2))
          evalAll(newEnv, body)
        }
        case (nenv, expr)                   => (nenv, expr)
      }
    case Procedure(f)         => (env, Procedure(f))
    case Function(args, body) => throw new IllegalArgumentException("invalid function call")
    case v @ Value(_)         => (env, v)
    case l @ List(_)          => (env, l)
    case Symbol(s)            =>
      env.lookUp(s) match {
        case Some(e)    => (env, e)
        case None       => throw new IllegalArgumentException("unbound symbol '" + s +"'")
    }
  }

  private def apply(f: ((Environment, List[Expression]) => (Environment, Expression)),
                    args: List[Expression], env: Environment) =
    f(env, args)

  // bind argument in a new environment
  private def bindArg(env: Environment, arg: ListType, expr: Expression) = arg match {
    case LValue(Name(n)) => env.addEntry(n -> eval(env, expr)._2)
    case _               => throw new IllegalArgumentException
  }

  // Eval a combination (a list of expressions), return the value of the last one
  private def evalAll(env: Environment, comb: List[Expression]): (Environment, Expression) = comb match {
    case List() => (env, NullExpr())
    case h :: t => {
      val (nenv, res) = eval(env, h)
      t.length match {
        case 0 => (env, res)
        case 1 => eval(nenv, t.head)
        case _ => evalAll(nenv, t)
      }
    }
  }
}
