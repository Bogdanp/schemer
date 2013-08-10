package schemer

sealed trait Expression
case class BooleanExpression(b: Boolean) extends Expression
case class NumberExpression(n: Double) extends Expression
case class StringExpression(s: String) extends Expression
case class SymbolExpression(s: String) extends Expression
case class ExpSymbolExpression(s: String) extends Expression
case class ListExpression[A <: Expression](xs: Seq[A]) extends Expression
case class QuotedExpression(e: Expression) extends Expression
case class UnqotedExpression(e: Expression) extends Expression
case class MacroExpression(s: SymbolExpression, ps: ListExpression[Expression], body: Seq[Expression]) extends Expression
case class ApplicationExpression(f: Expression, ps: Seq[Expression]) extends Expression
