package marmot

object Evaluator {
  def evalAll(expr: Expr, env: Env[Value]): List[Value] = expr match {
    case Prog(exprs) => exprs.map(eval(_, env))
    case x => List(eval(x, env))
  }

  def eval(e: Expr, env: Env[Value]): Value = e match {
    case IntLit(x) => IntValue(x)
    case DoubleLit(x) => DoubleValue(x)
    case BoolLit(x) => BoolValue(x)
    case VarLit(x) => env.get(x) match {
      case Some(x) => x
      case None => throw new Exception(s"Unknow variable: $x")
    }
    case ArrayLit(x) => {
      val xx = x.map(eval(_, env))
      ArrayValue(xx)
    }
    case ArrayApp(VarLit(name), idx) => env.get(name) match {
      case Some(ArrayValue(ary)) => {
        eval(idx, env) match {
          case IntValue(i) => ary(i)
          case x  => throw new Exception(s"Array access is only Int but $x")
        }
      }
      case Some(other) => throw new Exception(s"Required Array but $other")
      case None => throw new Exception(s"Unknow variable: $name")
    }
    case ArrayCons(e1, e2) => (eval(e1, env), eval(e2, env)) match {
      case (f@_, ArrayValue(xx)) => ArrayValue(f :: xx)
      case (x, y) => throw new Exception(s"invalid cons $x :: $y")
    }
    case Prim(op, e1, e2) => (eval(e1, env), eval(e2, env)) match {
      case (IntValue(x), IntValue(y)) => op match {
        case Op("+") => IntValue(x + y)
        case Op("-") => IntValue(x - y)
        case Op("*") => IntValue(x * y)
        case Op("/") => IntValue(x / y)
        case Op("==") => BoolValue(x == y)
        case Op("<") => BoolValue(x < y)
        case Op(">") => BoolValue(x > y)
      }
      case (DoubleValue(x), DoubleValue(y)) => op match {
        case Op("+.") => DoubleValue(x + y)
        case Op("-.") => DoubleValue(x - y)
        case Op("*.") => DoubleValue(x * y)
        case Op("/.") => DoubleValue(x / y)
        case Op("==") => BoolValue(x == y)
        case Op("<") => BoolValue(x < y)
        case Op(">") => BoolValue(x > y)
      }
      case (_, _) => throw new Exception("Unmatched type")
    }
    case IfExp(cond, e1, e2) => (eval(cond, env)) match {
      case BoolValue(true) => eval(e1, env)
      case BoolValue(false) => eval(e2, env)
    }
    case Let(VarLit(n), value, body) => {
      val r = eval(value, env)
      env.put(n, r)
      val newEnv = Env.build(env, (n, r))
      eval(body, newEnv)
    }
    case Fun(ags, body) => FunValue(ags, body, env)
    case App(name, exprs) => eval(name, env) match {
      case FunValue(ags, body, lEnv) => {
        val argValues = exprs.map(eval(_, env))
        val argLit = ags.map(_.v)
        val newEnv = lEnv.updateWithPairs(argLit, argValues)
        eval(body, newEnv)
      }
      case _ => throw new Exception(s"$name is not a function")
    }
    case _ => throw new Exception(s"unknow term $e")
  }
}
