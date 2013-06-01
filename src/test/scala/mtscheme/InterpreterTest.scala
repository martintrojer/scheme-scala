package mtscheme

import org.scalatest.FunSuite

import mtscheme.Interpreter._
import mtscheme.Parser._
import mtscheme.BuiltIn._

class InterpreterTest extends FunSuite {

  // TODO; extractors to remove duplication?

  def getNumResult(env: Env, expr: ExprT) = eval(env, expr)._2 match {
      case Value(Num(n))  => n
      case _              => throw new IllegalArgumentException("num expression failure")
  }

  def getBoolResult(env: Env, expr: ExprT) = eval(env, expr)._2 match {
    case Value(Bool(n)) => n
    case _              => throw new IllegalArgumentException("bool expression failure")
  }

  def getEListResult(env: Env, expr: ExprT) = eval(env, expr)._2 match {
    case EList(l)       => l
    case _              => throw new IllegalArgumentException("list expression failure")
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

  def testEList(env: Env, exprS: String, correct: List[ExprT]) = {
    expectResult(correct) { getEListResult(env, parse(exprS).head) }
  }

  def testNumberG = (testNumber _).curried(globalEnv)
  def testBoolG = (testBool _).curried(globalEnv)
  def testEListG = (testEList _).curried(globalEnv)
  def testNilG = (testNil _).curried(globalEnv)

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
    testNilG("(if false 1)")
  }

  test("cond") {
    testNumberG("(cond (true 1) ((= 1 2) 2))")      (1)
    testNumberG("(cond ((= 1 2) 1) (true 2))")      (2)
    testNumberG("(cond (false 1) (false 2) (else 3))") (3)
    testNilG("(cond (false 1) (false 2))")
  }

  test("define") {
    expectResult(Some(Value(Num(4)))) { getEnv(globalEnv, "(define lisa 4)").lookUp("lisa") }
    expectResult(Some(Value(Num(3)))) { getEnv(globalEnv, "(define nisse (+ 1 1 1))").lookUp("nisse") }
  }

  test("cons") {
    testEListG("(cons 1 2)")                   (List (Value(Num(1)), Value(Num(2))))
    testEListG("(cons 1 (cons 2 3))")          (List (Value(Num(1)), Value(Num(2)), Value(Num(3))))
    testEListG("(cons 1 (cons 2 (cons 3 4)))") (List (Value(Num(1)), Value(Num(2)), Value(Num(3)),
      Value(Num(4))))
    testEListG("(cons (cons 1 2) 3)")          (List (EList(List(Value(Num(1)), Value(Num(2)))),
      Value(Num(3))))
    testEListG("(cons \"kalle\" 2)")           (List (Value(Name("kalle")), Value(Num(2))))
  }

  test("list") {
    testEListG("(list 1 2)")              (List(Value(Num(1)), Value(Num(2))))
    testEListG("(list 5 (list 1 1) 2)")   (List(Value(Num(5)), EList(List(Value(Num(1)), Value(Num(1)))), Value(Num(2))))
    testEListG("(list 1 \"kalle\")")      (List(Value(Num(1)), Value(Name("kalle"))))
    testEListG("(list)")                  (List())
  }

  test("append") {
    testEListG("(append (list 1 2))")     (List(Value(Num(1)), Value(Num(2))))
    testEListG("(append (list 1 2) (list 3 4)")   (List(Value(Num(1)), Value(Num(2)), Value(Num(3)), Value(Num(4))))
    testEListG("(append 1 (list 2 (list 3)))")   (List(Value(Num(1)), Value(Num(2)), EList(List(Value(Num(3))))))
  }

  test("car") {
    testNumberG("(car (list 1 2))")       (List(1,2).head)
    testEListG("(car (list (list 1) 2))") (List(Value(Num(1))))
  }

  test("cdr") {
    testEListG("(cdr (list 1))")          (List())
    testEListG("(cdr (list 1 2))")        (List(Value(Num(2))))
    testEListG("(cdr (list 1 (list 2 3)))") (List(EList(List(Value(Num(2)), Value(Num(3))))))
    testEListG("(cdr (list (list 1)))")   (List())
  }

  test("null?") {
    testBoolG("(null? (list 1))")         (List(1).isEmpty)
    testBoolG("(null? (cdr (list 1)))")   (List(1).tail.isEmpty)
    testBoolG("(null? (cdr (cdr (list 1))))") (true)
    testBoolG("(null? (list))")           (List().isEmpty)
  }

  test("let") {
    testNumberG("(let ((a 1)) a")         (1)
    testNumberG("(let ((a 1)(b (+ 1 1))) (+ a b)") (3)
  }

  test("begin") {
    testNumberG("(begin 1 2)")            (2)
    testNumberG("(begin (define x 2) x)") (2)
  }

  test("function") {
    val e1 = getEnv(globalEnv, "(define (add a b) (+ a b))")
    testNumber(e1, "(add 1 2", 3)

    val e2 = getEnv(globalEnv, "(define (fact x) (if (= x 0) 1 (* x (fact (- x 1)))))")
    testNumber(e2, "(fact (+ 5 5))", 3628800)

    val e3 = getEnv(globalEnv, "(define (add a b) (begin (define (worker x y) (+ x y)) (worker a b)))")
    testNumber(e3, "(add 1 3)", 4)
  }

}
