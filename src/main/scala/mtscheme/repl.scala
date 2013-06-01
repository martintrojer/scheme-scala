package mtscheme

import mtscheme.BuiltIn._
import mtscheme.Interpreter.eval
import mtscheme.Parser.parse

object repl extends App {

  def commandLoop(env: Env, res: ExprT): Unit = {
    res match {
      case Value(Num(v))    => println(v + "\n> ")
      case Value(Name(v))   => println(v + "\n> ")
      case Value(Bool(v))   => println(v + "\n> ")
      case EList(l)         => println(listToString(l) + "\n> ")
      case _                => println("null \n> ")
    }
    try {
      val line = readLine()
      (commandLoop _).tupled(eval(env, parse(line).head))
    } catch {
      case e: Exception => commandLoop(env, Value(Name(e.getMessage)))
    }
  }

  val greet = "(display \"mtscheme v0.1 \")"
  val (env, res) = eval(globalEnv, parse(greet).head)
  commandLoop(env, res)
}
