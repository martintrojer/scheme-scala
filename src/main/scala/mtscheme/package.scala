package object mtscheme {

  // Is this kosher?

  type Env = List[EnvMap]
  type EnvMap = Map[String,Expression]

  def Env() = List(EnvMap())
  def Env(xs: EnvMap*) = List(xs:_*)
  def EnvMap(xs: (String, Expression)*) = Map(xs:_*)
}
