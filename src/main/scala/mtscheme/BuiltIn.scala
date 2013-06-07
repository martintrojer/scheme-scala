package mtscheme

import mtscheme.Interpreter._

object BuiltIn {

  def aritFun(op: ((BigDecimal, BigDecimal) => BigDecimal))
             (env: Env, comb: List[ExprT]) = {
    def error = throw new IllegalArgumentException("arithmetic error")
    comb.map(e => eval(env, e)._2) match {
      case Value(Num(first)) :: t =>
        val res = t.foldLeft(first)((acc, e) =>
          e match { case Value(Num(v)) => op(acc,v); case _ => error })
        (env,Value(Num(res)))
      case _                      => error
    }
  }

  def combFun(op: ((BigDecimal, BigDecimal) => Boolean))
             (env: Env, comb: List[ExprT]) = {
    def error = throw new IllegalArgumentException("comparison error")
    comb.map(e => eval(env, e)._2) match {
      case Value(Num(first)) :: t =>
        val res = t.foldLeft((true, first))((acc, e) => e match {
          case Value(Num(v))  => (acc._1 && op(acc._2, v), v)
          case _              => error })
        (env,Value(Bool(res._1)))
      case _                      => error
    }
  }

  // build a list of LValues from a Comb of Symbols
  def buildList(comb: List[ExprT]): List[ValueT] = comb match {
    case List()         => List()
    case Symbol(n) :: t => Name(n) :: buildList(t)
    case Value(v) :: t  => v :: buildList(t)
    case _              => throw new IllegalArgumentException("define args")
  }

  def listToString(ls: List[ExprT]) = {
    def ltos(ls: List[ExprT]): String = ls match {
      case List()   => ""
      case Value(Num(v)) :: t   => v.toString + ", " + ltos(t)
      case Value(Bool(v)) :: t  => v.toString + ", " + ltos(t)
      case Value(Name(v)) :: t  => v.toString + ", " + ltos(t)
      case EList(l) :: t        => "(" + ltos(l) + "), " + ltos(t)
      case _ :: t               => ltos(t)
    }
    "(" + ltos(ls) + ")"
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
    def error = throw new IllegalArgumentException("if")
    val (condExpr, posExpr, negExpr) = comb match {
      case condExpr :: posExpr :: negExpr :: Nil  =>
        (condExpr, posExpr, Some(negExpr))
      case condExpr :: posExpr :: Nil             =>
        (condExpr, posExpr, None)
      case _                                      => error
    }
    (eval(env, condExpr))._2 match {
        case Value(Bool(c)) =>
          if (c)
            eval(env, posExpr)
          else negExpr match {
            case Some(e)    => eval(env, e)
            case None       => (env, NullExpr())
          }
        case _              => error
     }
  }

  def _cond(env: Env, comb: List[ExprT]) = {
    def error = throw new IllegalArgumentException("cond")

    def doExpr(comb: List[ExprT]) = comb match {
      case Symbol("else") :: posExpr :: Nil =>
        Some(eval(env, posExpr)._2)
      case condExpr :: posExpr :: Nil       => eval(env, condExpr)._2 match {
        case Value(Bool(true))  => Some(eval(env, posExpr)._2)
        case Value(Bool(false)) => None
        case _                  => error
      }
      case _                                => error
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
    def error = throw new IllegalArgumentException("define")
    def getStr(expr: ExprT) = expr match {
      case Symbol(n)      => n
      case Value(Name(n)) => n
      case _              => error
    }
    comb match {
      // variable definition (lambda 'values' fall into this category)
      case Symbol(n) :: expr :: Nil   => {
        val (nenv, res) = eval(env, expr)
        (nenv.addEntry(n -> res), NullExpr())
      }
      // function definition
      case Comb(ns) :: body           => {
        val fname = getStr(ns.head)
        val args = buildList(ns.tail)
        (env.addEntry(fname -> Func(args, body)), NullExpr())
      }
      case _                          => error
    }
  }

  def _cons(env: Env, comb: List[ExprT]) = comb match {
    case e1 :: e2 :: Nil  =>
      (eval(env, e1)._2, eval(env, e2)._2) match {
        case (expr1, NullExpr())   =>
          (env, EList(List(expr1)))
        case (expr1, EList(l2))   =>
          (env, EList(expr1 :: l2))
        case (expr1, expr2) =>
          (env, EList(List(expr1, expr2)))
      }
    case _                => throw new IllegalArgumentException("cons")
  }

  def _list(env: Env, comb: List[ExprT]) = {
    def doExpr(comb: List[ExprT]): List[ExprT] = comb match {
      case List()   => Nil
      case h :: t   => eval(env, h)._2 :: doExpr(t)
    }
    (env, EList(doExpr(comb)))
  }

  def _append(env: Env, comb: List[ExprT]) = {
    def doExpr(comb: List[ExprT]): List[ExprT] = comb match {
      case List()   => Nil
      case h :: t   => eval(env, h)._2 match {
        case EList(l) => l ::: doExpr(t)
        case expr     => expr :: doExpr(t)
      }
    }
    (env, EList(doExpr(comb)))
  }

  def _car(env: Env, comb: List[ExprT]) = comb match {
    case h :: Nil   => eval(env, h)._2 match {
      case EList(l)   => (env, l.head)
      case _          => throw new IllegalArgumentException("car")
    }
    case _          => throw new IllegalArgumentException("car")
  }

  def _cdr(env: Env, comb: List[ExprT]) = comb match {
    case h :: Nil   => eval(env, h)._2 match {
      case EList(List())  => (env, EList(List()))
      case EList(l)       => (env, EList(l.tail))
      case _          => throw new IllegalArgumentException("car")
    }
    case _          => throw new IllegalArgumentException("car")
  }

  def _null(env: Env, comb: List[ExprT]) = comb match {
    case h :: Nil   => eval(env, h)._2 match {
      case EList(List())    => (env, Value(Bool(true)))
      case _                => (env, Value(Bool(false)))
    }
    case _          => throw new IllegalArgumentException("null?")
  }

  def _let(env: Env, comb: List[ExprT]) = {
    def error = throw new IllegalArgumentException("let")
    def doBind(acc: Env, binds: List[ExprT]): Env = binds match {
      case List()         =>
        acc
      case Comb(c) :: t   => c match {
        case Symbol(v) :: expr :: Nil =>
          doBind(acc.addEntry(v -> eval(env, expr)._2), t)
        case _                        => error
      }
      case _              => error
    }
    comb match {
      case Comb(binds) :: body :: Nil =>
        val newEnv = doBind(env.expand(), binds)
        eval(newEnv, body)
      case _                          => error
    }
  }

  def _begin(env: Env, comb: List[ExprT]) = evalAll(env, comb)

  def _lambda(env: Env, comb: List[ExprT]) = comb match {
    case Comb(args) :: body => (env, Func(buildList(args), body))
    case _                  => throw new IllegalArgumentException("lambda")
  }

  def _display(env: Env, comb: List[ExprT]) = {
    comb match {
      case expr :: Nil  => (eval(env, expr)._2) match {
        case Value(Num(v))    => println(v)
        case Value(Name(v))   => println(v)
        case Value(Bool(v))   => println(v)
        case EList(l)         => println(listToString(l))
        case _                => throw new IllegalArgumentException("display")
      }
      case _            => throw new IllegalArgumentException("display")
    }
    (env, NullExpr())
  }

  def _newline(env: Env, comb: List[ExprT]) = {
    comb match {
      case List()   => println()
      case _        => throw new IllegalArgumentException("newline")
    }
    (env, NullExpr())
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
                      ("cons" ->    Proc(_cons)),
                      ("list" ->    Proc(_list)),
                      ("append" ->  Proc(_append)),
                      ("car" ->     Proc(_car)),
                      ("cdr" ->     Proc(_cdr)),
                      ("null?" ->   Proc(_null)),
                      ("let" ->     Proc(_let)),
                      ("begin" ->   Proc(_begin)),
                      ("lambda" ->  Proc(_lambda)),
                      ("display" -> Proc(_display)),
                      ("newline" -> Proc(_newline)),

                      ("true" ->    Value(Bool(true))),
                      ("false" ->   Value(Bool(false)))

  )))
}
