package schemer.std

import schemer._

object Prelude {
  import Expression._
  import Schemer._
  import Control._
  import IO._
  import String._

  def arityErr(ps: Seq[Expression], n: Int, name: String): EvalResult =
    if (ps.length < n) Left(s"not enough arguments for function ${name}")
    else Left(s"too many arguments for function ${name}")

  val env =
    Env(None, Map(
      sym("print")   -> print_,
      sym("println") -> println_,
      sym("if")      -> if_,
      sym("set!")    -> set
    ))
}
