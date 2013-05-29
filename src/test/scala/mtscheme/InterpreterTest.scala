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

  def getEnv(env: Environment, exprS: String) = eval(env, parse(exprS).head)._1

  def testNumber(env: Environment, exprS: String, correct: BigDecimal) = {
    expectResult(correct) { getNumResult(env, parse(exprS).head) }
  }

  def testBool(env: Environment, exprS: String, correct: Boolean) = {
    expectResult(correct) { getBoolResult(env, parse(exprS).head) }
  }

  def testNil(env: Environment, exprS: String) = {
    expectResult(NullExpr()) { eval(env, parse(exprS).head)._2 }
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
    testBool(globalEnv, "(<= 2 2)",         (2<=2))
    testBool(globalEnv, "(<= 1 2)",         (1<=2))
    testBool(globalEnv, "(<= 2 1)",         (2<=1))
    testBool(globalEnv, "(<= (+ 1 1 1) 2)", ((1+1+1)<=2))
    testBool(globalEnv, "(<= 1)",           (true))
    testBool(globalEnv, "(<= 1 1 (+ 1 1) 1)",(false))
  }

  test("not") {
    testBool(globalEnv, "(not (= 1 1))",    (false))
    testBool(globalEnv, "(not (not (= 1 1)))",(true))
  }

  test("if") {
    testNumber(globalEnv, "(if (< 2 1) 10 11)",              11)
    testNumber(globalEnv, "(if (< (+ 1 1 1) 1) 11 (* 2 5))", 10)
    testNumber(globalEnv, "(if true 1)",                     1)
    testNil(globalEnv, "(if false 1)")
  }

  test("cond") {
    testNumber(globalEnv, "(cond (true 1) ((= 1 2) 2))",  1)
    testNumber(globalEnv, "(cond ((= 1 2) 1) (true 2))",  2)
    testNumber(globalEnv, "(cond (false 1) (false 2) (else 3))",  3)
    testNil(globalEnv, "(cond (false 1) (false 2))")
  }

  test("define") {
    expectResult(Some(Value(Num(4)))) { getEnv(globalEnv, "(define lisa 4)").lookUp("lisa") }
    expectResult(Some(Value(Num(3)))) { getEnv(globalEnv, "(define nisse (+ 1 1 1))").lookUp("nisse") }
  }

}
