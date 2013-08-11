package schemer

import jline._

import scala.language.postfixOps

object REPL {
  val prompt = "> "

  def main(args: Array[String]): Unit = {
    Terminal.getTerminal.initializeTerminal

    val reader = new ConsoleReader()

    Iterator
      .continually(reader.readLine(prompt))
      .takeWhile(l => l != ":q" && l != null)
      .foldLeft(std.Prelude.env) {
        case (env, line) => {
          Schemer.eval("stdin", line, env) match {
            case Right((ListExpression(xs), env)) => {
              xs.reverse match {
                case e :: _ => println(e)
                case _      =>
              }

              env
            }
            case Right((_, env)) => env
            case Left(err) => {
              println(s"error: ${err}.")
              env
            }
          }
        }
      }

      println
  }
}
