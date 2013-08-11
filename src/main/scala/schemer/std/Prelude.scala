package schemer.std

import schemer._

object Prelude {
  import Expression._
  import Schemer._
  import Control._
  import IO._

  def arityErr(ps: Seq[Expression], n: Int, name: String): EvalResult =
    if (ps.length < n) Left(s"not enough arguments for function ${name}")
    else Left(s"too many arguments for function ${name}")

  val env =
    Env()
      .set(sym("print"), print_)
      .set(sym("println"), println_)
      .set(sym("if"), if_)
      .set(sym("set!"), set)
}
