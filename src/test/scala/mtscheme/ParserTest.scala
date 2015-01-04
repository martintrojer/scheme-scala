package mtscheme

import org.scalatest.FunSuite

import mtscheme.Parser.parse
import mtscheme.HandRolledParser._

class ParserTest extends FunSuite {

  // ----

  test("Tokens Open/Close") {
    assertResult(List(TOpen(), TClose())) {
      tokenize("()".toList)
    }
  }

  test("Tokens Symbol") {
    assertResult(List(TSymbol("kalle"))) {
      tokenize("kalle".toList)
    }
  }

  test("Tokens String") {
    assertResult(List(TString("kalle"))) {
      tokenize("\"kalle\"".toList)
    }
  }

  test("Tokens Numbers") {
    assertResult(List(TNumber("42"), TNumber("42"), TNumber("-42"))) {
      tokenize("42 +42 -42".toList)
    }
  }

  test("Tokens Factional Number") {
    assertResult(List(TNumber("3.14"))) { tokenize("3.14".toList)}
  }

  // ----

  test("Parse OpenClose") {
    val res = List(Comb(List()))
    assertResult(res) { HandRolledParser.parse("()") }
    assertResult(res) { Parser.parse("()") }
  }

  test("Parse Multiple") {
    val res = List(Value(Num(1.0)), Value(Num(2.0)))
    assertResult(res) { HandRolledParser.parse("1 2") }
    assertResult(res) { Parser.parse("1 2") }
  }

  test("Parse Expr") {
    val res = List(Comb(List(Symbol("+"), Value(Num(1.0)), Value(Num(2.0)))))
    assertResult(res) { HandRolledParser.parse("(+ 1 2") }
    assertResult(res) { Parser.parse("(+ 1 2)") }
  }
}
