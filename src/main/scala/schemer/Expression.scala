package schemer

sealed trait Expression {
  override def toString: String =
    this match {
      case BooleanExpression(true)  => "#t"
      case BooleanExpression(false) => "#f"
      case NumberExpression(n)      => n.toString
      case StringExpression(s)      => "\"" + s + "\""
      case SymbolExpression(s)      => s
      case ExpSymbolExpression(s)   => "&" + s
      case ListExpression(xs)       => s"[${xs.map(_.toString).mkString(" ")}]"
      case QuotedExpression(e)      => "'" + e.toString
      case UnqotedExpression(e)     => "@" + e.toString

      case MacroExpression(s, ps, body) =>
        s"""|(defmacro ${s.toString} ${ps.toString}
            |  ${body.map(_.toString).mkString("\n  ")})""".stripMargin

      case FunctionExpression(s, ps, body) =>
        s"""|(defn ${s.toString} ${ps.toString}
            |  ${body.map(_.toString).mkString("\n  ")})""".stripMargin

      case ApplicationExpression(f, ps) =>
        s"""(${f.toString} ${ps.map(_.toString).mkString(" ")})"""
    }
}

case class BooleanExpression(b: Boolean) extends Expression
case class NumberExpression(n: Double) extends Expression
case class StringExpression(s: String) extends Expression
case class SymbolExpression(s: String) extends Expression
case class ExpSymbolExpression(s: String) extends Expression
case class ListExpression[A <: Expression](xs: Seq[A]) extends Expression
case class QuotedExpression(e: Expression) extends Expression
case class UnqotedExpression(e: Expression) extends Expression
case class MacroExpression(s: SymbolExpression, ps: ListExpression[Expression], body: Seq[Expression]) extends Expression
case class FunctionExpression(s: SymbolExpression, ps: ListExpression[Expression], body: Seq[Expression]) extends Expression
case class ApplicationExpression(f: Expression, ps: Seq[Expression]) extends Expression
