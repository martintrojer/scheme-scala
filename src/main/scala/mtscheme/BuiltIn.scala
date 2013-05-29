package mtscheme

import mtscheme.Interpreter._

object BuiltIn {

  def aritFun(op: ((BigDecimal, BigDecimal) => BigDecimal))
             (env: Environment, comb: List[Expression]) = {
    val error = new IllegalArgumentException("arithmetic error")
    comb.map(e => eval(env, e)._2) match {
      case Value(Num(first)) :: t =>
        val res = t.foldLeft(first)((acc, e) =>
          e match { case Value(Num(v)) => op(acc,v); case _ => throw error })
        (env,Value(Num(res)))
      case _                      => throw error
    }
  }

  def combFun(op: ((BigDecimal, BigDecimal) => Boolean))
             (env: Environment, comb: List[Expression]) = {
    val error = new IllegalArgumentException("comparison error")
    comb.map(e => eval(env, e)._2) match {
      case Value(Num(first)) :: t =>
        val res = t.foldLeft((true, first))((acc, e) => e match {
          case Value(Num(v)) => (acc._1 && op(acc._2, v), v)
          case _ => throw error })
        (env,Value(Bool(res._1)))
      case _                      => throw error
    }
  }

  // build a list of LValues from a Combination of Symbols
  def buildList(comb: List[Expression]): List[ListType] = comb match {
    case List()         => List()
    case Symbol(n) :: t => LValue(Name(n)) :: buildList(t)
    case Value(v) :: t  => LValue(v) :: buildList(t)
    case _              => throw new IllegalArgumentException("define args")
  }

  // -------------------------------------------

  def _not(env: Environment, comb: List[Expression]) = comb match {
    case expr :: Nil  => eval(env, expr) match {
      case (_, Value(Bool(v)))  => (env, Value(Bool(!v)))
      case _                    => throw new IllegalArgumentException("not")
    }
    case _            => throw new IllegalArgumentException("not")
  }

  def _if(env: Environment, comb: List[Expression]) = {
    val error = new IllegalArgumentException("if")
    val (condExpr, posExpr, negExpr) = comb match {
      case condExpr :: posExpr :: negExpr :: Nil  =>
        (condExpr, posExpr, Some(negExpr))
      case condExpr :: posExpr :: Nil             =>
        (condExpr, posExpr, None)
      case _                                      => throw error
    }
    (eval(env, condExpr))._2 match {
        case Value(Bool(c)) =>
          if (c)
            eval(env, posExpr)
          else negExpr match {
            case Some(e)    => eval(env, e)
            case None       => (env, NullExpr())
          }
        case _              => throw error
     }
  }

  def _cond(env: Environment, comb: List[Expression]) = {
    val error = new IllegalArgumentException("cond")

    def doExpr(comb: List[Expression]) = comb match {
      case Symbol("else") :: posExpr :: Nil =>
        Some(eval(env, posExpr)._2)
      case condExpr :: posExpr :: Nil       => eval(env, condExpr)._2 match {
        case Value(Bool(true))  => Some(eval(env, posExpr)._2)
        case Value(Bool(false)) => None
        case _                  => throw error
      }
      case _                                => throw error
    }

    def runExprs(comb: List[Expression]): Expression = comb match {
      case Combination(c) :: rest   => doExpr(c) match {
        case Some(e)    => e
        case None       => runExprs(rest)
      }
      case _                        => NullExpr()
    }

    (env, runExprs(comb))
  }

  def _define(env: Environment, comb: List[Expression]) = {
    val error = new IllegalArgumentException("define")
    def getStr(expr: Expression) = expr match {
      case Symbol(n)      => n
      case Value(Name(n)) => n
      case _              => throw error
    }
    comb match {
      // variable definition (lambda 'values' fall into this category)
      case Symbol(n) :: expr :: Nil   => {
        val (nenv, res) = eval(env, expr)
        (env.addEntry(n -> res), NullExpr())
      }
      // function definition
      case Combination(ns) :: body    => {
        val fname = getStr(ns.head)
        val args = buildList(ns.tail)
        (env.addEntry(fname -> Function(args, body)), NullExpr())
      }
      case _                          => throw error
    }
  }

  // -------------------------------------------

  val globalEnv = Environment(Env(EnvMap(
                      ("+" -> Procedure(aritFun(_+_) _)),
                      ("-" -> Procedure(aritFun(_-_) _)),
                      ("*" -> Procedure(aritFun(_*_) _)),
                      ("/" -> Procedure(aritFun(_/_) _)),

                      ("=" -> Procedure(combFun(_==_) _)),
                      (">" -> Procedure(combFun(_>_) _)),
                      ("<" -> Procedure(combFun(_<_) _)),
                      (">=" -> Procedure(combFun(_>=_) _)),
                      ("<=" -> Procedure(combFun(_<=_) _)),

                      ("true" -> Value(Bool(true))),
                      ("false" -> Value(Bool(false))),

                      ("not" -> Procedure(_not)),
                      ("if" -> Procedure(_if)),
                      ("cond" -> Procedure(_cond)),
                      ("define" -> Procedure(_define))
    )))
}
