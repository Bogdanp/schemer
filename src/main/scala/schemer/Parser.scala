package schemer

import scala.util.parsing.combinator._

object Parser extends RegexParsers {
  override val whiteSpace = """(;.*|[ \t\r\n]+)""".r

  def expression: Parser[Expression] =
    boolean   |
    number    |
    string    |
    expSymbol |
    symbol    |
    list      |
    quoted    |
    unqoted   |
    macro_    |
    function  |
    application

  def boolean: Parser[BooleanExpression] =
    "#" ~> """(t|f)""".r ^^ {
      case "t" => BooleanExpression(true)
      case "f" => BooleanExpression(false)
    }

  def number: Parser[NumberExpression] = {
    val zero: Parser[NumberExpression] =
      "0" ^^ { _ => NumberExpression(0) }

    val whole: Parser[NumberExpression] =
      """[1-9][0-9]*""".r ^^ { number =>
        NumberExpression(number.toDouble)
      }

    val real: Parser[NumberExpression] =
      """([1-9][0-9]*)?\.\d+""".r ^^ { number =>
        NumberExpression(number.toDouble)
      }

    real | whole | zero
  }

  def string: Parser[StringExpression] =
    """"([^"]|"")*"""".r ^^ { string =>
      if (string.length == 2) {
        StringExpression("")
      } else {
        val escaped = string.replace("\"\"", "\"")

        StringExpression(escaped.substring(1, escaped.length - 1))
      }
    }

  def symbol: Parser[SymbolExpression] =
    """[^ \t\r\n\(\)\[\]'@]+""".r ^^ { symbol =>
      SymbolExpression(symbol)
    }

  def expSymbol: Parser[ExpSymbolExpression] =
    "&" ~> symbol ^^ {
      case symbol => ExpSymbolExpression(symbol.s)
    }

  def quoted: Parser[QuotedExpression] =
    "'" ~> expression ^^ {
      case e => QuotedExpression(e)
    }

  def unqoted: Parser[UnqotedExpression] =
    "@" ~> expression ^^ {
      case e => UnqotedExpression(e)
    }

  def list: Parser[ListExpression] =
    "[" ~> rep(expression) <~ "]" ^^ {
      case xs => ListExpression(xs)
    }

  def parameterList: Parser[ListExpression] =
    "[" ~> rep(expSymbol | symbol) <~ "]" ^^ {
      case xs => ListExpression(xs)
    }

  def macro_ : Parser[MacroExpression] =
    "(defmacro" ~> symbol ~ parameterList ~ rep(expression) <~ ")" ^^ {
      case s ~ ps ~ body => MacroExpression(s, ps, body)
    }

  def function : Parser[FunctionExpression] =
    "(defn" ~> symbol ~ parameterList ~ rep(expression) <~ ")" ^^ {
      case s ~ ps ~ body => FunctionExpression(s, ps, body)
    }

  def application: Parser[ApplicationExpression] =
    "(" ~> expression ~ rep(expression) <~ ")" ^^ {
      case f ~ ps => ApplicationExpression(f, ps)
    }

  def apply(filename: String, input: String): Either[String, (String, Seq[Expression])] =
    parseAll(rep(expression), input) match {
      case Success(expressions, _) => Right((filename, expressions))
      case NoSuccess(error, next)  =>
        Left(s"${filename}:${next.pos.line}:${next.pos.column}:${error}.")
    }
}
