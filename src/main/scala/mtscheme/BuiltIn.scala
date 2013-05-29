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


  val globalEnv = Environment(Env(EnvMap(
                      ("+" -> Procedure(aritFun(_+_) _)),
                      ("-" -> Procedure(aritFun(_-_) _)),
                      ("*" -> Procedure(aritFun(_*_) _)),
                      ("/" -> Procedure(aritFun(_/_) _)),

                      ("=" -> Procedure(combFun(_==_) _)),
                      (">" -> Procedure(combFun(_>_) _)),
                      ("<" -> Procedure(combFun(_<_) _)),
                      (">=" -> Procedure(combFun(_>=_) _)),
                      ("<=" -> Procedure(combFun(_<=_) _))
  )))
}
