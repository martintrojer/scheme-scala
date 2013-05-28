package mtscheme

import mtscheme.Interpreter._

object BuiltIn {

  def aritFun(op: ((Double, Double) => Double))
             (env: Environment, comb: List[Expression]) = {
    val terms = comb.map(e => eval(env, e)._2)
    val first = terms.head match { case Value(Num(v)) => v; case _ => 0}
    val res = terms.tail.foldLeft(first)((acc, e) => e match {
      case Value(Num(v))  => op(acc, v)
      case _              => throw new IllegalArgumentException("arithmetic error")
    })
    (env, Value(Num(res)))
  }

  val globalEnv = Environment(Env(EnvMap(
                      ("+" -> Procedure(aritFun(_+_) _)),
                      ("-" -> Procedure(aritFun(_-_) _)),
                      ("*" -> Procedure(aritFun(_*_) _)),
                      ("/" -> Procedure(aritFun(_/_) _)))))
}
