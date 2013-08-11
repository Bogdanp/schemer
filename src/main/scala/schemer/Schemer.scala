package schemer

object Schemer {
  import Expression._

  type Expressions = Seq[Expression]
  type EvalResult  = Either[String, (Expression, Env)]
  type EvalResults = Either[String, (Expressions, Env)]

  def eval(source: String, input: String, env: Env): EvalResult =
    Parser(source, input).right.flatMap {
      case (_, xs) => {
        withEvalList(xs, env) {
          case (xs, env) =>
            Right((list(xs), env))
        }
      }
    }

  def eval(expression: Expression, env: Env): EvalResult =
    expression match {
      case ae: ApplicationExpression => {
        eval(ae.f, env) match {
          case Right((NativeExpression(fn), env)) =>
            fn(list(ae.ps), env)
          case Right((macro_ : MacroExpression, env)) =>
            try {
              evalBody(macro_.body, paramsToEnv(macro_.ps.xs, ae.ps, env))
            } catch {
              case _: IndexOutOfBoundsException =>
                Left(s"invalid number of arguments passed to function ${macro_.s}")
            }
          case Right((fn: FunctionExpression, env)) =>
            withEvalList(ae.ps, env) {
              case (ps, env) =>
                try {
                  evalBody(fn.body, paramsToEnv(fn.ps.xs, ae.ps, env))
                } catch {
                  case _: IndexOutOfBoundsException =>
                    Left(s"invalid number of arguments passed to function ${fn.s}")
                }
            }
          case Left(err) => Left(err)
          case _         => undefinedState
        }
      }
      case e: SymbolExpression =>
        env.lookup(e) match {
          case Some(v) => Right(v, env)
          case None    => Left(s"failed to look up '${e.s}'")
        }
      case e: ListExpression =>
        withEvalList(e.xs, env) {
          case (xs, env) =>
            Right((list(xs), env))
        }
      case e: UnqotedExpression   => eval(e.e, env)
      case e: ExpSymbolExpression => eval(sym(e.s), env)
      case e: QuotedExpression    => Right(e, env)
      case e: MacroExpression     => Right(e, env.set(e.s, e))
      case e: FunctionExpression  => Right(e, env.set(e.s, e))
      case e                      => Right(e, env)
    }

  protected def evalList(xs: Expressions, env: Env): EvalResults = {
    val empty: EvalResults = Right((Seq[Expression](), env))

    (empty /: xs) {
      case (Left(err), _)        => Left(err)
      case (Right((xs, env)), x) => {
        eval(x, env).right.flatMap {
          case (v, env) => Right((xs ++ Seq(v), env))
        }
      }
    }
  }

  protected def withEvalList
      (xs: Expressions, env: Env)
      (fn: (Seq[Expression], Env) => EvalResult):
      EvalResult =
    evalList(xs, env).right.flatMap {
      case (xs, env) => fn(xs, env)
    }

  protected def paramsToEnv(ss: Seq[Expression], ps: Seq[Expression], env: Env): Env = {
    if (ps.length > ss.length) {
      if (ss.length == 0)
        throw new IndexOutOfBoundsException()

      ss.last match {
        case _: ExpSymbolExpression =>
        case _                      => throw new IndexOutOfBoundsException()
      }
    }

    ss.zipWithIndex.foldLeft(env) {
      case (env, (symbol, idx)) =>
        symbol match {
          case s: SymbolExpression    => env.set(s, ps(idx))
          case s: ExpSymbolExpression => env.set(
            sym(s.s), list(ps.slice(idx, ps.length)))
          case _ => undefinedState
        }
    }
  }

  protected def evalBody(xs: Seq[Expression], env: Env): EvalResult = {
    val body = withEvalList(xs, env) {
      case (xs, env) => Right((list(xs), env))
    }

    body.right.flatMap {
      case (body: ListExpression, env) =>
        Right((body.xs.last, env))
      case _ => undefinedState
    }
  }

  protected def undefinedState =
    sys.error("the interpreter is in an unefined state")
}
