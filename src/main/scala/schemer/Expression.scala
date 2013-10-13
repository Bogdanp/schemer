package schemer

sealed trait Expression {
  override def toString: String =
    this match {
      case UnitExpression()         => "()"
      case NativeExpression(_)      => "(...)"
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

  def show: String =
    this match {
      case StringExpression(s) => s
      case _                   => toString
    }
}

case class UnitExpression() extends Expression
case class NativeExpression[A <: Expression](fn: (ListExpression, Env) => Either[String, (A, Env)]) extends Expression
case class BooleanExpression(b: Boolean) extends Expression
case class NumberExpression(n: Double) extends Expression
case class StringExpression(s: String) extends Expression
case class SymbolExpression(s: String) extends Expression
case class ExpSymbolExpression(s: String) extends Expression
case class ListExpression(xs: Seq[Expression]) extends Expression
case class QuotedExpression(e: Expression) extends Expression
case class UnqotedExpression(e: Expression) extends Expression
case class MacroExpression(s: SymbolExpression, ps: ListExpression, body: Seq[Expression]) extends Expression
case class FunctionExpression(s: SymbolExpression, ps: ListExpression, body: Seq[Expression]) extends Expression
case class ApplicationExpression(f: Expression, ps: Seq[Expression]) extends Expression

object Expression {
  val unit = UnitExpression()

  def bool(b: Boolean)          = BooleanExpression(b)
  def num(n: Double)            = NumberExpression(n)
  def str(s: String)            = StringExpression(s)
  def sym(s: String)            = SymbolExpression(s)
  def list(xs: Seq[Expression]) = ListExpression(xs)
  def quoted(e: Expression)     = QuotedExpression(e)
  def unquoted(e: Expression)   = UnqotedExpression(e)

  def m(s: SymbolExpression, ps: ListExpression, body: Seq[Expression]) =
    MacroExpression(s, ps, body)

  def fn(s: SymbolExpression, ps: ListExpression, body: Seq[Expression]) =
    FunctionExpression(s, ps, body)

  def app(f: Expression, ps: Seq[Expression]) =
    ApplicationExpression(f, ps)

  def native[A <: Expression]
      (fn: (ListExpression, Env) => Either[String, (A, Env)]):
      NativeExpression[A] =
    NativeExpression(fn)
}
