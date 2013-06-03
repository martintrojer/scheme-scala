package mtscheme

import scala.util.parsing.combinator._

object Parser extends JavaTokenParsers {

  def value: Parser[ValueT] = stringLiteral ^^ (x => Name(x.tail.init)) |
                              floatingPointNumber ^^ (x => Num(BigDecimal(x)))

  def expression: Parser[ExprT] = value ^^ (x => Value(x)) |
                                  """[^()\s]+""".r ^^ (x => Symbol(x)) |
                                  combination

  def combination: Parser[Comb] = "(" ~> rep(expression) <~ ")" ^^ (x => Comb(x))

  def program: Parser[List[ExprT]] = rep(expression)

  // ---

  def parse(source: String) = parseAll(program, source).get
}
