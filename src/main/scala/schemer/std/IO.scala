package schemer.std

import schemer._

object IO {
  import Expression._
  import Schemer._

  protected def display[A](fn: String => A) =
    native {
      case (ListExpression(ps), env) =>
        withEvalList(ps, env) {
          case (xs, env) => {
            fn(xs.map(_.show).mkString(" "))
            Right(unit, env)
          }
        }
    }

  val print_   = display(print _)
  val println_ = display(println _)
}
