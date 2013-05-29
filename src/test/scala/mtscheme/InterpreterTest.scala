package mtscheme

import org.scalatest.FunSuite

import mtscheme.Interpreter._
import mtscheme.Parser._
import mtscheme.BuiltIn._

class InterpreterTest extends FunSuite {

  // TODO; extractors to remove duplication?

  def getNumResult(env: Environment, expr: Expression) = eval(env, expr)._2 match {
      case Value(Num(n))  => n
      case _              => throw new IllegalArgumentException("expression failure")
  }

  def getBoolResult(env: Environment, expr: Expression) = eval(env, expr)._2 match {
    case Value(Bool(n)) => n
    case _              => throw new IllegalArgumentException("expression failure")
  }

  def testNumber(env: Environment, exprS: String, correct: BigDecimal) = {
    expectResult(correct) { getNumResult(env, parse(exprS).head) }
  }

  def testBool(env: Environment, exprS: String, correct: Boolean) = {
    expectResult(correct) { getBoolResult(env, parse(exprS).head) }
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
    testNumber(globalEnv, "(+ 1 (/ 2 3))",  (1.0+BigDecimal(2.0)/3.0))
    testNumber(globalEnv, "(/ 1)",          (1))
    // testNumber(globalEnv, "(/ 2)",          (1.0/2.0))      // TODO; special case not handled correctly
    testNumber(globalEnv, "(/ 1 2 3)",      (1.0/BigDecimal(2.0)/3.0))
  }

  test("eq") {
    testBool(globalEnv, "(= 2 2)",          (2==2))
    testBool(globalEnv, "(= 2 (+ 1 1))",    (2==(1+1)))
    testBool(globalEnv, "(= 1)",            (true))
    testBool(globalEnv, "(= 1 1 2)",        (false))
    testBool(globalEnv, "(= 1 1 (+ 1 1) 1)",(false))
  }

  test("gt") {
    testBool(globalEnv, "(> 2 2)",          (2>2))
    testBool(globalEnv, "(> 1 2)",          (1>2))
    testBool(globalEnv, "(> 2 1)",          (2>1))
    testBool(globalEnv, "(> (+ 1 1 1) 2)",  ((1+1+1)>2))
    testBool(globalEnv, "(> 1)",            (true))
    testBool(globalEnv, "(> 1 1 (+ 1 1) 1)",(false))
  }

  test("lt") {
    testBool(globalEnv, "(< 2 2)",          (2<2))
    testBool(globalEnv, "(< 1 2)",          (1<2))
    testBool(globalEnv, "(< 2 1)",          (2<1))
    testBool(globalEnv, "(< (+ 1 1 1) 2)",  ((1+1+1)<2))
    testBool(globalEnv, "(< 1)",            (true))
    testBool(globalEnv, "(< 1 1 (+ 1 1) 1)",(false))
  }

  test("ge") {
    testBool(globalEnv, "(>= 2 2)",         (2>=2))
    testBool(globalEnv, "(>= 1 2)",         (1>=2))
    testBool(globalEnv, "(>= 2 1)",         (2>=1))
    testBool(globalEnv, "(>= (+ 1 1 1) 2)", ((1+1+1)>=2))
    testBool(globalEnv, "(>= 1)",           (true))
    testBool(globalEnv, "(>= 1 1 (+ 1 1) 1)",(false))
  }

  test("le") {
    testBool(globalEnv, "(<= 2 2)", (2<=2))
    testBool(globalEnv, "(<= 1 2)", (1<=2))
    testBool(globalEnv, "(<= 2 1)", (2<=1))
    testBool(globalEnv, "(<= (+ 1 1 1) 2)", ((1+1+1)<=2))
    testBool(globalEnv, "(<= 1)", (true))
    testBool(globalEnv, "(<= 1 1 (+ 1 1) 1)", (false))
  }
}
