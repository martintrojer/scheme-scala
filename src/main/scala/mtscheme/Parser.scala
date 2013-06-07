package mtscheme

import scala.util.parsing.combinator._

object Parser extends JavaTokenParsers {

  val value: Parser[ValueT] = stringLiteral ^^ (x => Name(x.tail.init)) |
                              floatingPointNumber ^^ (x => Num(BigDecimal(x)))

  val expression: Parser[ExprT] = value ^^ (x => Value(x)) |
                                  """[^()\s]+""".r ^^ (x => Symbol(x)) |
                                  combination

  val combination: Parser[Comb] = "(" ~> rep(expression) <~ ")" ^^ (x => Comb(x))

  val program: Parser[List[ExprT]] = rep(expression)

  // ---

  def parse(source: String) = parseAll(program, source).get
}
