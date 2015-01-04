package mtscheme

import org.scalatest.FunSuite

class EnvTest extends FunSuite {

  val entry = ("foo" -> Value(Num(1)))
  val testEnv = Env().addEntry(entry)

  test("addEntry") {
    val resEnv = Env(EnvT(EnvMapT(entry)))
    assertResult(resEnv) { Env().addEntry(entry) }
  }

  test("simple lookup") {
    assertResult(Some(Value(Num(1)))) { testEnv lookUp "foo" }
  }

  test("nested lookup") {
    val env = testEnv.expand()
    assertResult(Some(Value(Num(1)))) { env lookUp "foo" }
  }

  test("shadowed nested lookup") {
    val env = testEnv.expand().addEntry("foo"->Value(Num(2)))
    assertResult(Some(Value(Num(2)))) { env lookUp "foo" }
  }

  test("failing lookup") {
    assertResult(None) { testEnv lookUp "bar" }
  }

}
