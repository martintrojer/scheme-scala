package mtscheme

import org.scalatest.FunSuite

import mtscheme.Interpreter._
import mtscheme.Parser._
import mtscheme.BuiltIn._

class InterpreterTest extends FunSuite {

  def getNumResult(env: Environment, expr: Expression) = eval(env, expr)._2 match {
      case Value(Num(n))  => n
      case _              => throw new IllegalArgumentException("expression failure")
  }

  def testNumber(env: Environment, exprS: String, correct: Double) = {
    expectResult(correct) { getNumResult(env, parse(exprS).head) }
  }

  // ------------------------------------------

  test("add") {
    testNumber(globalEnv, "(+ 1 2)",        (1+2))
    testNumber(globalEnv, "(+ 1 (+ 2 3))",  (1+2+3))
    testNumber(globalEnv, "(+ 1)",          (1))
    testNumber(globalEnv, "(+ 1 1 1)",      (1+1+1))
  }

  test("sub") {
    testNumber(globalEnv, "(- 1 2)",        (1-2))
    testNumber(globalEnv, "(- 1 (- 2 3))",  (1-(2-3)))
    // testNumber(globalEnv, "(- 1)",          (-1))
    testNumber(globalEnv, "(- 1 1 1)",      (1-1-1))
  }

  test("mul") {
    testNumber(globalEnv, "(* 2 3.14)",     (2.0*3.14))
    testNumber(globalEnv, "(+ 1 (* 2 3))",  (1+2*3))
    testNumber(globalEnv, "(* 1)",          (1))
    testNumber(globalEnv, "(* 2 1 2 2)",    (2*1*2*2))
  }

  test("div") {
    testNumber(globalEnv, "(/ 9 3)",        (9/3))
    testNumber(globalEnv, "(+ 1 (/ 2 3))",  (1.0+2.0/3.0))
    testNumber(globalEnv, "(/ 1)",          (1))
    // testNumber(globalEnv, "(/ 2)",          (1.0/2.0))      // TODO; special case not handled correctly
    testNumber(globalEnv, "(/ 1 2 3)",      (1.0/2.0/3.0))
  }
}