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
      .takeWhile(_ != ":q")
      .foldLeft(Env()) {
        case (env, line) => {
          Schemer.eval("stdin", line, env) match {
            case Right((r, env)) => {
              println(r)
              env
            }
            case Left(err) => {
              println("error: " ++ err)
              env
            }
          }
        }
      }
  }
}
