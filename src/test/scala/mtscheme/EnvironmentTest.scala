package mtscheme

import org.scalatest.FunSuite

class EnvironmentTest extends FunSuite {

  val entry = ("foo" -> Value(Num(1)))
  val testEnv = Environment().addEntry(entry)

  test("addEntry") {
    val resEnv = Environment(Env(EnvMap(entry)))
    expectResult(resEnv) { Environment().addEntry(entry) }
  }

  test("simple lookup") {
    expectResult(Some(Value(Num(1)))) { testEnv lookUp "foo" }
  }

  test("nested lookup") {
    val env = testEnv.expand()
    expectResult(Some(Value(Num(1)))) { env lookUp "foo" }
  }

  test("shadowed nested lookup") {
    val env = testEnv.expand().addEntry("foo"->Value(Num(2)))
    expectResult(Some(Value(Num(2)))) { env lookUp "foo" }
  }

  test("failing lookup") {
    expectResult(None) { testEnv lookUp "bar" }
  }

}
