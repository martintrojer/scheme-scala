package mtscheme

object BuiltIn {

  def aritFun(op: ((Double, Double) => Double))(env: Environment, comb: List[Expression]) = {

    val error = new IllegalArgumentException("arithmetic error")

    def afn() = comb match {
      case List()     => throw error
      case e :: t     => (Interpreter.eval(env, e))._2 match {
        case Value(Num(v))  => {
          t.foldLeft(v)((acc, e) => (Interpreter.eval(env, e))._2 match {
            case Value(Num(a))  => op(acc,a)
            case _              => throw error
          })}
        case _              => throw error
        }
      }
    (env, Value(Num(afn())))
  }

  val globalEnv = Environment(Env(
                      EnvMap("+" -> Procedure(aritFun(_+_) _)),
                      EnvMap("-" -> Procedure(aritFun(_-_) _))))
}
