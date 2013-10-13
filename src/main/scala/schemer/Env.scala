package schemer

case class Env(
  parent: Option[Env],
  syms: Env.Table) {

  def lookup(symbol: SymbolExpression): Option[Expression] = {
    if (syms contains symbol) Some(syms(symbol))
    else parent match {
      case Some(env) => env lookup symbol
      case None      => None
    }
  }

  def set(symbol: String, value: Expression): Env =
    Env(parent, syms + (Expression.sym(symbol) -> value))

  def set(symbol: SymbolExpression, value: Expression): Env =
    Env(parent, syms + (symbol -> value))
}

object Env {
  type Table = Map[SymbolExpression, Expression]

  def apply(): Env = Env(None, Map())
  def apply(parent: Env): Env = Env(Some(parent), Map())
  def from(syms: Table): Env = Env(None, syms)
}
