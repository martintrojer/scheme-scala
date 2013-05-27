package mtscheme

import org.scalatest.FunSuite

class InterpreterTest extends FunSuite{

  test("setEnv on empty env") {
    intercept[IllegalArgumentException] {
      Interpreter.setEnv("foo", Value(Num(1)), List())
    }
  }

}
