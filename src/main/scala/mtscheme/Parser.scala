package mtscheme

// ----------------------------------------
// Case Types

sealed trait Token
case class TOpen() extends Token
case class TClose() extends Token
case class TNumber(v: String) extends Token
case class TString(v: String) extends Token
case class TSymbol(v: String) extends Token

// ----------------------------------------

object Parser {

  def tokenize(source: List[Char]) = {

    def string(acc: String, cs: List[Char]): (String, List[Char]) = cs match {
      case '\\' :: '"' :: t   =>  string(acc + "\"", t)                 // escaped quote becomes quote
      case '"' :: t           => (acc, t)                               // closing quote terminates
      case c :: t             => string(acc + c, t)                     // otherwise accumulate chars
      case _                  => throw new IllegalArgumentException("malformed string")
    }

    def token(acc: String, cs: List[Char]): (String, List[Char]) = cs match {
      case t @ ')' :: _               => (acc, t)                       // closing paren terminates
      case w :: t if w.isWhitespace   => (acc, t)                       // whitespace terminates
      case List()                     => (acc, List())                  // end of list terminates
      case c :: t                     => token(acc + c, t)              // otherwise accumulate chars
    }

    def doTok(acc: List[Token], cs: List[Char]): List[Token] = cs match {
      case w :: t if w.isWhitespace   => doTok(acc, t)                  // skip whitespace
      case '(' :: t                   => doTok(TOpen() :: acc, t)
      case ')' :: t                   => doTok(TClose() :: acc, t)
      case '"' :: t                   => {                              // start of string
        val (s, rst) = string("", t)
        doTok(TString(s) :: acc, rst)
      }
      case '-' :: d :: t if d.isDigit => {                              // start of neg number
        val (n, rst) = token("-" + d, t)
        doTok(TNumber(n) :: acc, rst)
      }
      case '+' :: d :: t if d.isDigit => {                              // start of positive number
        val (n, rst) = token(d.toString, t)
        doTok(TNumber(n) :: acc, rst)
      }
      // TODO; how to remove this duplication? (extractors?)
      case d :: t if d.isDigit        => {
      val (n, rst) = token(d.toString, t)
        doTok(TNumber(n) :: acc, rst)
      }
      case s :: t                     => {                              // otherwise start of symbol
        val (ts, rst) = token(s.toString, t)
        doTok(TSymbol(ts) :: acc, rst)
      }
      case List()                     => acc.reverse                    // terminate
    }

    doTok(List(), source)
  }

  def parse(source: String) = {
    def mapTok(tok: Token) = tok match {
      case TNumber(n)     => Value(Num(BigDecimal(n)))
      case TString(s)     => Value(Name(s))
      case TSymbol(s)     => Symbol(s)
      case _              => throw new IllegalArgumentException("syntax error")
    }
    def doParse(acc: List[Expression], toks: List[Token]): (List[Expression], List[Token]) = toks match {
      case TOpen() :: t   => {
        val (e, rst) = doParse(List(), t)
        doParse(Combination(e) :: acc, rst)
      }
      case TClose() :: t  => (acc.reverse, t)
      case h :: t         => doParse(mapTok(h) :: acc, t)
      case List()         => (acc.reverse, List())
    }

    val (res, _) = doParse(List(), (tokenize(source.toList)))
    res
  }
}
