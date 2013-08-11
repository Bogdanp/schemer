package schemer.std

import schemer._

object Prelude {
  import Expression._
  import Schemer._

  val set =
    native {
      case (ListExpression(Seq(s: SymbolExpression, v)), env) => {
        eval(unquoted(v), env).right.flatMap {
          case (v, env) => Right((v, env.set(s, v)))
        }
      }
      case (ListExpression(Seq(_, _)), _) =>
        Left("the first parameter to set! must be a symbol")
      case (ListExpression(ps), _) =>
        if (ps.length < 2) Left("not enough arguments for function set!")
        else Left("too many arguments for function set!")
    }

  val if_ =
    native {
      case (ListExpression(Seq(c, t, f)), env) => {
        eval(unquoted(c), env).right.flatMap {
          case (BooleanExpression(false), env) => eval(unquoted(f), env)
          case (BooleanExpression(true), env)  => eval(unquoted(t), env)
          case (NumberExpression(0), env)      => eval(unquoted(f), env)
          case (NumberExpression(_), env)      => eval(unquoted(t), env)
          case (StringExpression(""), env)     => eval(unquoted(f), env)
          case (StringExpression(_), env)      => eval(unquoted(t), env)
          case (ListExpression(Seq()), env)    => eval(unquoted(f), env)
          case (ListExpression(_), env)        => eval(unquoted(t), env)
          case _                               => Left("operation not supported")
        }
      }
    }

  val env =
    Env()
      .set(sym("if"), if_)
      .set(sym("set!"), set)
}
