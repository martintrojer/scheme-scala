package mtscheme

import mtscheme.BuiltIn._
import mtscheme.Interpreter.eval
import mtscheme.Parser.parse

object repl extends App {

  def commandLoop(env: Env, res: ExprT): Unit = {
    res match {
      case Value(Num(v))    => println(v)
      case Value(Name(v))   => println(v)
      case Value(Bool(v))   => println(v)
      case EList(l)         => println(listToString(l))
      case _                => println("null")
    }
    try {
      print("> ")
      val line = readLine()
      (commandLoop _).tupled(eval(env, parse(line).head))
    } catch {
      case e: Exception => commandLoop(env, Value(Name("Error; " + e.getMessage)))
    }
  }

  val greet = "(display \"mtscheme v0.1 \")"
  val (env, res) = eval(globalEnv, parse(greet).head)
  commandLoop(env, res)
}
