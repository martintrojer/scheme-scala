package mtscheme

import org.scalatest.FunSuite

import mtscheme.Interpreter._
import mtscheme.Parser._
import mtscheme.BuiltIn._

class InterpreterTest extends FunSuite {

  // TODO; extractors to remove duplication?

  def getNumResult(env: Env, expr: ExprT) = eval(env, expr)._2 match {
      case Value(Num(n))  => n
      case _              => throw new IllegalArgumentException("expression failure")
  }

  def getBoolResult(env: Env, expr: ExprT) = eval(env, expr)._2 match {
    case Value(Bool(n)) => n
    case _              => throw new IllegalArgumentException("expression failure")
  }

  def getEnv(env: Env, exprS: String) = eval(env, parse(exprS).head)._1

  def testNumber(env: Env, exprS: String, correct: BigDecimal) = {
    expectResult(correct) { getNumResult(env, parse(exprS).head) }
  }

  def testBool(env: Env, exprS: String, correct: Boolean) = {
    expectResult(correct) { getBoolResult(env, parse(exprS).head) }
  }

  def testNil(env: Env, exprS: String) = {
    expectResult(NullExpr()) { eval(env, parse(exprS).head)._2 }
  }

  val testNumberG = (testNumber _).curried(globalEnv)
  val testBoolG = (testBool _).curried(globalEnv)

  // ------------------------------------------

  test("add") {
    testNumberG("(+ 1 2)")        (1+2)
    testNumberG("(+ 1 (+ 2 3))")  (1+2+3)
    testNumberG("(+ 1)")          (1)
    testNumberG("(+ 1 1 1)")      (1+1+1)
  }

  test("sub") {
    testNumberG("(- 1 2)")        (1-2)
    testNumberG("(- 1 (- 2 3))")  (1-(2-3))
    // testNumberG("(- 1)")          (-1)
    testNumberG("(- 1 1 1)")      (1-1-1)
  }

  test("mul") {
    testNumberG("(* 2 3.14)")     (2.0*3.14)
    testNumberG("(+ 1 (* 2 3))")  (1+2*3)
    testNumberG("(* 1)")          (1)
    testNumberG("(* 2 1 2 2)")    (2*1*2*2)
  }

  test("div") {
    testNumberG("(/ 9 3)")        (9/3)
    testNumberG("(+ 1 (/ 2 3))")  (1.0+BigDecimal(2.0)/3.0)
    testNumberG("(/ 1)")          (1)
    // testNumberG("(/ 2)")          (1.0/2.0)      // TODO; special case not handled correctly
    testNumberG("(/ 1 2 3)")      (1.0/BigDecimal(2.0)/3.0)
  }

  test("eq") {
    testBoolG("(= 2 2)")          (2==2)
    testBoolG("(= 2 (+ 1 1))")    (2==(1+1))
    testBoolG("(= 1)")            (true)
    testBoolG("(= 1 1 2)")        (false)
    testBoolG("(= 1 1 (+ 1 1) 1)")(false)
  }

  test("gt") {
    testBoolG("(> 2 2)")          (2>2)
    testBoolG("(> 1 2)")          (1>2)
    testBoolG("(> 2 1)")          (2>1)
    testBoolG("(> (+ 1 1 1) 2)")  ((1+1+1)>2)
    testBoolG("(> 1)")            (true)
    testBoolG("(> 1 1 (+ 1 1) 1)")(false)
  }

  test("lt") {
    testBoolG("(< 2 2)")          (2<2)
    testBoolG("(< 1 2)")          (1<2)
    testBoolG("(< 2 1)")          (2<1)
    testBoolG("(< (+ 1 1 1) 2)")  ((1+1+1)<2)
    testBoolG("(< 1)")            (true)
    testBoolG("(< 1 1 (+ 1 1) 1)")(false)
  }

  test("ge") {
    testBoolG("(>= 2 2)")         (2>=2)
    testBoolG("(>= 1 2)")         (1>=2)
    testBoolG("(>= 2 1)")         (2>=1)
    testBoolG("(>= (+ 1 1 1) 2)") ((1+1+1)>=2)
    testBoolG("(>= 1)")           (true)
    testBoolG("(>= 1 1 (+ 1 1) 1)")(false)
  }

  test("le") {
    testBoolG("(<= 2 2)")         (2<=2)
    testBoolG("(<= 1 2)")         (1<=2)
    testBoolG("(<= 2 1)")         (2<=1)
    testBoolG("(<= (+ 1 1 1) 2)") ((1+1+1)<=2)
    testBoolG("(<= 1)")           (true)
    testBoolG("(<= 1 1 (+ 1 1) 1)")(false)
  }

  test("not") {
    testBoolG("(not (= 1 1))")    (false)
    testBoolG("(not (not (= 1 1)))")(true)
  }

  test("if") {
    testNumberG("(if (< 2 1) 10 11)")               (11)
    testNumberG("(if (< (+ 1 1 1) 1) 11 (* 2 5))")  (10)
    testNumberG("(if true 1)")                      (1)
    testNil(globalEnv, "(if false 1)")
  }

  test("cond") {
    testNumberG("(cond (true 1) ((= 1 2) 2))")      (1)
    testNumberG("(cond ((= 1 2) 1) (true 2))")      (2)
    testNumberG("(cond (false 1) (false 2) (else 3))") (3)
    testNil(globalEnv, "(cond (false 1) (false 2))")
  }

  test("define") {
    expectResult(Some(Value(Num(4)))) { getEnv(globalEnv, "(define lisa 4)").lookUp("lisa") }
    expectResult(Some(Value(Num(3)))) { getEnv(globalEnv, "(define nisse (+ 1 1 1))").lookUp("nisse") }
  }

}
