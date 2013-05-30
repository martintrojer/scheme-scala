package mtscheme

import mtscheme.Interpreter._

object BuiltIn {

  def aritFun(op: ((BigDecimal, BigDecimal) => BigDecimal))
             (env: Env, comb: List[ExprT]) = {
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
             (env: Env, comb: List[ExprT]) = {
    val error = new IllegalArgumentException("comparison error")
    comb.map(e => eval(env, e)._2) match {
      case Value(Num(first)) :: t =>
        val res = t.foldLeft((true, first))((acc, e) => e match {
          case Value(Num(v))  => (acc._1 && op(acc._2, v), v)
          case _              => throw error })
        (env,Value(Bool(res._1)))
      case _                      => throw error
    }
  }

  // build a list of LValues from a Comb of Symbols
  def buildList(comb: List[ExprT]): List[ValueT] = comb match {
    case List()         => List()
    case Symbol(n) :: t => Name(n) :: buildList(t)
    case Value(v) :: t  => v :: buildList(t)
    case _              => throw new IllegalArgumentException("define args")
  }

  // -------------------------------------------

  def _not(env: Env, comb: List[ExprT]) = comb match {
    case expr :: Nil  => eval(env, expr) match {
      case (_, Value(Bool(v)))  => (env, Value(Bool(!v)))
      case _                    => throw new IllegalArgumentException("not")
    }
    case _            => throw new IllegalArgumentException("not")
  }

  def _if(env: Env, comb: List[ExprT]) = {
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

  def _cond(env: Env, comb: List[ExprT]) = {
    val error = new IllegalArgumentException("cond")

    def doExpr(comb: List[ExprT]) = comb match {
      case Symbol("else") :: posExpr :: Nil =>
        Some(eval(env, posExpr)._2)
      case condExpr :: posExpr :: Nil       => eval(env, condExpr)._2 match {
        case Value(Bool(true))  => Some(eval(env, posExpr)._2)
        case Value(Bool(false)) => None
        case _                  => throw error
      }
      case _                                => throw error
    }

    def runExprs(comb: List[ExprT]): ExprT = comb match {
      case Comb(c) :: rest  => doExpr(c) match {
        case Some(e)    => e
        case None       => runExprs(rest)
      }
      case _                => NullExpr()
    }

    (env, runExprs(comb))
  }

  def _define(env: Env, comb: List[ExprT]) = {
    val error = new IllegalArgumentException("define")
    def getStr(expr: ExprT) = expr match {
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
      case Comb(ns) :: body           => {
        val fname = getStr(ns.head)
        val args = buildList(ns.tail)
        (env.addEntry(fname -> Func(args, body)), NullExpr())
      }
      case _                          => throw error
    }
  }

  // -------------------------------------------

  val globalEnv = Env(EnvT(EnvMapT(
                      ("+" ->       Proc(aritFun(_+_) _)),
                      ("-" ->       Proc(aritFun(_-_) _)),
                      ("*" ->       Proc(aritFun(_*_) _)),
                      ("/" ->       Proc(aritFun(_/_) _)),

                      ("=" ->       Proc(combFun(_==_) _)),
                      (">" ->       Proc(combFun(_>_) _)),
                      ("<" ->       Proc(combFun(_<_) _)),
                      (">=" ->      Proc(combFun(_>=_) _)),
                      ("<=" ->      Proc(combFun(_<=_) _)),

                      ("not" ->     Proc(_not)),
                      ("if" ->      Proc(_if)),
                      ("cond" ->    Proc(_cond)),
                      ("define" ->  Proc(_define)),

                      ("true" ->    Value(Bool(true))),
                      ("false" ->   Value(Bool(false)))

  )))
}
