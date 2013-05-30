package object mtscheme {

  // Is this kosher?

  type EnvT = List[EnvMapT]
  type EnvMapT = Map[String,ExprT]

  def EnvT() = List(EnvMapT())
  def EnvT(xs: EnvMapT*) = List(xs:_*)
  def EnvMapT(xs: (String, ExprT)*) = Map(xs:_*)
}
