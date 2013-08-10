package schemer

import org.specs2.mutable._

import scala.util._
import scala.util.parsing.combinator._

class ParserSpec extends Specification {
  "The Parser" should {
    "parse integers" in {
      Parser.parse(Parser.number, "123").get must be equalTo(NumberExpression(123))
    }

    "parse doubles" in {
      Parser.parse(Parser.number, "3.14").get must be equalTo(NumberExpression(3.14))
    }

    "parse string literals" in {
      Parser.parse(Parser.string, """"Hello, World!"""").get must be equalTo(
        StringExpression("Hello, World!"))
    }

    "parse string literals with escaping" in {
      Parser.parse(Parser.string, """"Hello, ""World""!"""").get must be equalTo(
        StringExpression("""Hello, "World"!"""))
    }

    "parse symbols" in {
      Parser.parse(Parser.symbol, "+").get must be equalTo(SymbolExpression("+"))
      Parser.parse(Parser.symbol, ">=>").get must be equalTo(SymbolExpression(">=>"))
    }

    "parse expansion symbols" in {
      Parser.parse(Parser.expSymbol, "&xs").get must be equalTo(
        ExpSymbolExpression("xs"))
    }

    "parse quoted expressions" in {
      Parser.parse(Parser.quoted, "'123").get must be equalTo(
        QuotedExpression(NumberExpression(123)))
      Parser.parse(Parser.quoted, "'\"String\"").get must be equalTo(
        QuotedExpression(StringExpression("String")))
      Parser.parse(Parser.quoted, "'+").get must be equalTo(
        QuotedExpression(SymbolExpression("+")))
      Parser.parse(Parser.quoted, "'(+ 1 2)").get must be equalTo(
        QuotedExpression(
          ApplicationExpression(
            SymbolExpression("+"),
            Seq(
              NumberExpression(1),
              NumberExpression(2)))))
    }

    "parse unqoted expressions" in {
      Parser.parse(Parser.unqoted, "@+").get must be equalTo(
        UnqotedExpression(SymbolExpression("+")))
    }

    "parse lists" in {
      Parser.parse(Parser.list, "[1 \"String\" 'quote]").get must be equalTo(
        ListExpression(Seq(
          NumberExpression(1),
          StringExpression("String"),
          QuotedExpression(SymbolExpression("quote")))))
    }

    "parse parameter lists" in {
      Parser.parse(Parser.parameterList, "[name &body]").get must be equalTo(
        ListExpression(Seq(
          SymbolExpression("name"),
          ExpSymbolExpression("body"))))
    }

    "parse macros" in {
      Parser.parse(Parser.macro_, """
(defmacro or [p1 p2]
  (let [ep1 @p1]
    (if ep1
        ep1
        @p2)))
""").get must be equalTo(
        MacroExpression(
          SymbolExpression("or"),
          ListExpression(Seq(
            SymbolExpression("p1"),
            SymbolExpression("p2"))),
          Seq(
            ApplicationExpression(
              SymbolExpression("let"),
              Seq(
                ListExpression(Seq(
                  SymbolExpression("ep1"),
                  UnqotedExpression(SymbolExpression("p1")))),
                ApplicationExpression(
                  SymbolExpression("if"),
                  Seq(
                    SymbolExpression("ep1"),
                    SymbolExpression("ep1"),
                    UnqotedExpression(SymbolExpression("p2")))))))))

      Parser.parse(Parser.macro_, """
(defmacro defn [name ps &body]
  (define @name (lambda @ps @body)))
""").get must be equalTo(
        MacroExpression(
          SymbolExpression("defn"),
          ListExpression(Seq(
            SymbolExpression("name"),
            SymbolExpression("ps"),
            ExpSymbolExpression("body"))),
          Seq(
            ApplicationExpression(
              SymbolExpression("define"),
              Seq(
                UnqotedExpression(SymbolExpression("name")),
                ApplicationExpression(
                  SymbolExpression("lambda"),
                  Seq(
                    UnqotedExpression(SymbolExpression("ps")),
                    UnqotedExpression(SymbolExpression("body")))))))))
    }

    "parse simple application" in {
      Parser.parse(Parser.application, "(do-stuff)").get must be equalTo(
        ApplicationExpression(
          SymbolExpression("do-stuff"), Seq()))
      Parser.parse(Parser.application, "(+ 1 2)").get must be equalTo(
        ApplicationExpression(
          SymbolExpression("+"),
          Seq(
            NumberExpression(1),
            NumberExpression(2))))
    }

    "parse nested application" in {
      Parser.parse(Parser.application, "(+ (+ 2 3) (/ 6 2))").get must be equalTo(
        ApplicationExpression(
          SymbolExpression("+"),
          Seq(
            ApplicationExpression(
              SymbolExpression("+"),
              Seq(
                NumberExpression(2),
                NumberExpression(3))),
            ApplicationExpression(
              SymbolExpression("/"),
              Seq(
                NumberExpression(6),
                NumberExpression(2))))))
    }

    "parse valid schemer source code" in {
      Parser("stdin", """
(define s "Hello, World!")
(puts s)
""") match {
        case Right((filename, expressions)) => {
          filename must be equalTo("stdin")
          expressions must be equalTo(Seq(
            ApplicationExpression(
              SymbolExpression("define"),
              Seq(
                SymbolExpression("s"),
                StringExpression("Hello, World!"))),
            ApplicationExpression(
              SymbolExpression("puts"),
              Seq(
                SymbolExpression("s")))))
        }
        case Left(s) => failure(s)
      }
    }

    "parse invalid schemer source code" in {
      Parser("stdin", "(+ 1 2") match {
        case Right(_)  => failure("Should never have gotten here.")
        case Left(err) =>
          err must be equalTo("stdin:1:7:`)' expected but end of source found.")
      }
    }
  }
}
