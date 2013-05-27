package mtscheme

import org.scalatest.FunSuite

import mtscheme.Interpreter._

class InterpreterTest extends FunSuite {

  val emptyEnv = List(EnvMap())
  val entry = ("foo" -> Value(Num(1)))

  test("setEnv") {
    val resEnv = List(EnvMap(entry))
    expectResult(resEnv) { setEnv(entry, emptyEnv) }
  }

  test("setEnv on empty env") {
    intercept[IllegalArgumentException] {
      setEnv(entry, List())
    }
  }

  test("simple lookup") {
    val env = setEnv(entry, emptyEnv)
    expectResult(Some(Value(Num(1)))) { lookUp("foo", env) }
  }

  test("nested lookup") {
    val env = expandEnv(setEnv(entry, emptyEnv))
    expectResult(Some(Value(Num(1)))) { lookUp("foo", env) }
  }

  test("failing lookup") {
    val env = setEnv(entry, emptyEnv)
    expectResult(None) { lookUp("bar", env) }
  }

}
