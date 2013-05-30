package mtscheme

import org.scalatest.FunSuite

import mtscheme.Parser._

class ParserTest extends FunSuite {

  test("Tokens Open/Close") {
    expectResult(List(TOpen(), TClose())) {
      tokenize("()".toList)
    }
  }

  test("Tokens Symbol") {
    expectResult(List(TSymbol("kalle"))) {
      tokenize("kalle".toList)
    }
  }

  test("Tokens String") {
    expectResult(List(TString("kalle"))) {
      tokenize("\"kalle\"".toList)
    }
  }

  test("Tokens Numbers") {
    expectResult(List(TNumber("42"), TNumber("42"), TNumber("-42"))) {
      tokenize("42 +42 -42".toList)
    }
  }

  test("Parse OpenClose") {
    expectResult(List(Comb(List()))) {
      parse("()")
    }
  }

  test("Parse Multiple") {
    expectResult(List(Value(Num(1.0)), Value(Num(2.0)))) {
      parse("1 2")
    }
  }

  test("Parse Expr") {
    expectResult(List(Comb(List(Symbol("+"), Value(Num(1.0)), Value(Num(2.0)))))) {
      parse("(+ 1 2")
    }
  }
}
