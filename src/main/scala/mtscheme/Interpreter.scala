package mtscheme

// ----------------------------------------
// Case Types

sealed trait ValueType
case class Num(v: Double) extends ValueType
case class Bool(v: Boolean) extends ValueType
case class Name(v: String) extends ValueType

sealed trait ListType
case class LValue(v: ValueType) extends ListType
case class LList(v: ListType) extends ListType

sealed trait Expression
case class NullExpr() extends Expression
case class Combination(v: List[Expression]) extends Expression
case class EList(v: List[ListType]) extends Expression
case class Function(args: List[ListType], body: List[Expression]) extends Expression
//case class Procedure(...) extends Expression
case class Symbol(v: String) extends Expression
case class Value(v: ValueType) extends Expression

// ----------------------------------------
class Interpreter
