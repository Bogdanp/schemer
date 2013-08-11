package schemer

// TODO: Clean this up.
object Schemer {
  def eval(expression: Expression, env: Env): Either[String, (Expression, Env)] =
    expression match {
      case e: ApplicationExpression => {
        eval(e.f, env) match {
          //case Right((macro: MacroExpression, env)) => {
          //}
          case Right((fn: FunctionExpression, env)) =>
            withEvalList(e.ps, env) { ps =>
              val fnEnv = fn.ps.xs.zipWithIndex.foldLeft(env) {
                case (env, (sym, idx)) =>
                  sym match {
                    case s: SymbolExpression    => env.set(s, ps(idx))
                    case s: ExpSymbolExpression => env.set(
                      SymbolExpression(s.s),
                      ListExpression(ps.slice(idx, ps.length)))
                  }
              }

              val body = withEvalList(fn.body, fnEnv) { xs =>
                Right((ListExpression(xs), env))
              }

              body match {
                case Right((body: ListExpression[_], env)) => Right((body.xs.last, env))
                case Left(err)                             => Left(err)
              }
            }
          case Left(err) => Left(err)
        }
      }
      case e: SymbolExpression =>
        env.lookup(e) match {
          case Some(v) => Right(v, env)
          case None    => Left(s"failed to look up '${e.s}'")
        }
      case e: ListExpression[_] =>
        withEvalList(e.xs, env) { xs =>
          Right((ListExpression(xs), env))
        }
      case e: UnqotedExpression   => eval(e.e, env)
      case e: ExpSymbolExpression => eval(SymbolExpression(e.s), env)
      case e: QuotedExpression    => Right(e, env)
      case e: MacroExpression     => Right(e, env.set(e.s, e))
      case e: FunctionExpression  => Right(e, env.set(e.s, e))
      case e                      => Right(e, env)
    }

  protected def evalList(xs: Seq[Expression], env: Env): Either[String, Seq[Expression]] = {
    var fail: String = ""
    val rs = xs.map { x =>
      eval(x, env) match {
        case Right((v, env)) => Some(v)
        case Left(err)       => {
          if (fail isEmpty) fail = err
          None
        }
      }
    }

    if (fail isEmpty) Right(rs.flatten)
    else Left(fail)
  }

  protected def withEvalList(xs: Seq[Expression], env: Env)
                            (fn: Seq[Expression] => Either[String, (Expression, Env)]):
      Either[String, (Expression, Env)] =
    evalList(xs, env) match {
      case Right(xs) => fn(xs)
      case Left(err) => Left(err)
    }
}
