package schemer

object REPL {
  def prompt = print(">> ")

  def main(args: Array[String]): Unit = {
    prompt

    Iterator.continually(Console.readLine)
      .takeWhile(_ != ":q")
      .foldLeft(Env()) {
      case (env, line) => {
        Schemer.eval("stdin", line, env) match {
          case Right((r, env)) => {
            println(r)
            prompt
            env
          }
          case Left(err) => {
            println(err)
            prompt
            env
          }
        }
      }
    }
  }
}
