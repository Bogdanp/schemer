package schemer

case class Env(
  parent: Option[Env],
  symbolTable: Map[SymbolExpression, Expression]) {

  def lookup(symbol: SymbolExpression): Option[Expression] = {
    if (symbolTable contains symbol) Some(symbolTable(symbol))
    else parent match {
      case Some(env) => env lookup symbol
      case None      => None
    }
  }

  def set(symbol: SymbolExpression, value: Expression): Env =
    Env(parent, symbolTable + (symbol -> value))
}

object Env {
  def apply(): Env = Env(None, Map())
  def apply(parent: Env): Env = Env(Some(parent), Map())
}
