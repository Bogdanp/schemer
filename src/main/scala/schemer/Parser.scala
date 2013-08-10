package schemer

import scala.util.parsing.combinator._

object Parser extends RegexParsers {
  def expression: Parser[Expression] =
    number  |
    string  |
    symbol  |
    list    |
    quoted  |
    unqoted |
    application

  def number: Parser[NumberExpression] =
    """\d+(\.\d+)?""".r ^^ { number =>
      NumberExpression(number.toDouble)
    }

  def string: Parser[StringExpression] =
    """"([^"]|"")*"""".r ^^ { string =>
      val escaped = string.replace("\"\"", "\"")

      StringExpression(escaped.substring(1, escaped.length - 1))
    }

  def symbol: Parser[SymbolExpression] =
    """[^ \t\r\n\(\)\[\]'@]+""".r ^^ { symbol =>
      SymbolExpression(symbol)
    }

  def quoted: Parser[QuotedExpression] =
    "'" ~> expression ^^ {
      case e => QuotedExpression(e)
    }

  def unqoted: Parser[UnqotedExpression] =
    "@" ~> symbol ^^ {
      case e => UnqotedExpression(e)
    }

  def list: Parser[ListExpression[Expression]] =
    "[" ~> rep(expression) <~ "]" ^^ {
      case xs => ListExpression(xs)
    }

  def application: Parser[ApplicationExpression] =
    "(" ~> expression ~ rep(expression) <~ ")" ^^ {
      case f ~ ps => ApplicationExpression(f, ps)
    }
}
