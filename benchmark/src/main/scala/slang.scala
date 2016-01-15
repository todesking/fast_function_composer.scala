package com.todesking.fast_function_composer.benchmark.slang

import scala.language.implicitConversions

object SMain {
  def main(args: Array[String]): Unit = {
    val g = Env.newGlobal
    p("(+ 1 2)", g)
    p("(+ 1 (+ 1 1))", g)
    p("(define x (+ 1 1))", g)
    p("(if (< x 1) 10 20)", g)
    p("(if (< 1 x) 10 20)", g)
    p("x", g)
    p("(+ 1 x)", g)
    p("((lambda (x) (+ x 1)) 10)", g)
    p("""(define (fib n)
      (if (< n 2)
        n
        (+ (fib (- n 2)) (fib (- n 1)))))""", g)
    p("(- 3 2)", g)
    p("(fib 1)", g)
    p("(fib 3)", g)
    p("(fib 10)", g)
    p("(fib 38)", g)
  }

  def p(s: String, e: Env): Unit = {
    println(s)
    println("=> " + Expr.parse(s).eval(e).rep)
  }
}

class SyntaxError(msg: String) extends RuntimeException(msg)
class EvalError(msg: String) extends RuntimeException(msg)
class CastError(tpe: String, value: String) extends EvalError(s"${value} can't cast to ${tpe}")
class LookupError(sym: Symbol) extends EvalError(s"Symbol not found: ${sym}")

object ExprConversions {
  implicit def intToExpr(v: Int): Expr = Expr(v)
  implicit def stringToExpr(v: String): Expr = Expr(v)
  implicit def booleanToExpr(v: Boolean): Expr = Expr(v)
  implicit def symbolToExpr(v: Symbol): Expr = Expr(v)
  implicit def seqToExpr(v: Seq[_]): Expr = Expr(v)
  implicit def tuple2ToExpr(v: (_, _)): Expr = Expr(v)
}

object Parser extends scala.util.parsing.combinator.RegexParsers {
  override def skipWhitespace = false

  import Expr._

  def parse(src: String): Expr = parseAll(expr, src) match {
    case Success(expr, _) => expr
    case NoSuccess(msg, _) => throw new SyntaxError(s"${src}: ${msg}")
  }

  def expr: Parser[Expr] = cons | list | atom
  def cons: Parser[Expr] = (((('(' ~> expr) <~ '.') ~ expr) <~ ')') ^^ { case car ~ cdr => SCons(car, cdr) }
  def list: Parser[Expr] = ('(' ~> repsep(expr, "\\s+".r)) <~ ')' ^^ { exprs => Expr.list(exprs: _*) }
  def atom: Parser[Expr] = int | symbol | string | boolean
  def int = """\d+""".r ^^ { i => SInt(i.toInt) }
  def symbol = """[-+*/<>!?a-zA-Z][-+*/<>!?a-zA-Z0-9]*""".r ^^ { sym => SSymbol(Symbol(sym)) }
  def string = '"' ~> ("""[^"]*""".r | """\\"""").* <~ '"' ^^ { s => SString(s.mkString) }
  def boolean = ("#t" ^^ { _ => SBoolean(true) }) | ("#f" ^^ { _ => SBoolean(false) })
}

class Env(parent: Env) {
  private[this] val data = scala.collection.mutable.HashMap.empty[Symbol, Expr]

  def spawn: Env = new Env(this)

  def lookup(sym: Symbol): Option[Expr] =
    data.get(sym) orElse { if (parent != null) parent.lookup(sym) else None }

  def define(sym: Symbol, value: Expr): Unit =
    data(sym) = value

  def set(sym: Symbol, value: Expr): Unit =
    if (parent != null) parent.set(sym, value)
    else define(sym, value)
}
object Env {
  def newGlobal: Env = {
    import Expr._
    val e = new Env(null)
    e.define('+, primitiveP { case SCons(car, cdr: SList) => SInt(cdr.foldLeft(car.castToInt) { (a, v) => a + v.castToInt }) })
    e.define('-, primitiveP { case SCons(car, cdr: SList) => SInt(cdr.foldLeft(car.castToInt) { (a, v) => a - v.castToInt }) })
    e.define('*, primitiveP { case SCons(car, cdr: SList) => SInt(cdr.foldLeft(car.castToInt) { (a, v) => a * v.castToInt }) })
    e.define('<, primitiveP { case SCons(a, SCons(b, SNil)) => SBoolean(a.castToInt < b.castToInt) })
    e.define('define, special("define") { (args, env) =>
      args match {
        case SCons(SSymbol(name), SCons(value, SNil)) =>
          env.define(name, value.eval(env))
          SNil
        case SCons(SCons(SSymbol(name), params: SList), SCons(body, SNil)) =>
          env.define(name, SLambda(env, params.toSeq.map(_.castToSymbol), body))
          SNil
        case err => specialError("define", err)
      }
    })
    e.define('if, special("if") { (args, env) =>
      args match {
        case SCons(cond, SCons(th, SCons(el, SNil))) =>
          if (cond.eval(env).castToBoolean) th.eval(env) else el.eval(env)
        case err => specialError("define", err)
      }
    })
    e.define('lambda, special("lambda") { (args, env) =>
      args match {
        case SCons(params, SCons(body, SNil)) =>
          SLambda(env, params.asList.toSeq.map(_.castToSymbol), body)
        case err => specialError("lambda", err)
      }
    })
    e
  }

  private[this] def specialError(name: String, args: Expr.SList): Nothing =
    throw new EvalError(s"Illegal arguments for define: ${args.rep}")
}

sealed abstract class Expr {
  def eval(env: Env): Expr

  def castToInt: Int = castError("Int")
  def castToBoolean: Boolean = castError("Boolean")
  def castToSymbol: Symbol = castError("Symbol")
  def castToFunction: (Expr.SList, Env) => Expr = castError("lambda/special")
  def asList: Expr.SList = castError("list")

  def rep: String = Expr.rep(this)

  private[this] def castError(tpe: String): Nothing = throw new CastError(tpe, this.rep)
}
object Expr {
  sealed abstract class SList extends Expr {
    override def asList = this
    def foldLeft[A](init: A)(f: (A, Expr) => A): A
    def foldRight[A](init: A)(f: (Expr, A) => A): A
    def map(f: Expr => Expr): SList
    def toSeq: Seq[Expr] = foldRight[List[Expr]](Nil) { (e, l) => e :: l }
  }

  case class SCons(car: Expr, cdr: Expr) extends SList {
    override def eval(env: Env): Expr =
      car.eval(env).castToFunction.apply(cdr.asList, env)
    override def foldLeft[A](init: A)(f: (A, Expr) => A): A =
      cdr.asList.foldLeft(f(init, car))(f)
    override def foldRight[A](init: A)(f: (Expr, A) => A): A =
      f(car, cdr.asList.foldRight(init)(f))
    override def map(f: Expr => Expr): SList =
      SCons(f(car), cdr.asList.map(f))
  }
  case object SNil extends SList {
    override def eval(env: Env): Expr = throw new EvalError("can't eval ()")
    override def foldLeft[A](init: A)(f: (A, Expr) => A): A = init
    override def foldRight[A](init: A)(f: (Expr, A) => A): A = init
    override def map(f: Expr => Expr): SList = SNil
  }

  sealed abstract class Atom[A] extends Expr {
    override def eval(env: Env): Expr = this
  }
  case class SInt(value: Int) extends Atom {
    override def castToInt = value
  }
  case class SBoolean(value: Boolean) extends Atom {
    override def castToBoolean = value
  }
  case class SString(value: String) extends Atom {
  }
  case class SSymbol(value: Symbol) extends Atom {
    override def castToSymbol = value
    override def eval(env: Env): Expr =
      env.lookup(value).getOrElse { throw new LookupError(value) }
  }
  case class SLambda(env: Env, params: Seq[Symbol], body: Expr) extends Atom {
    override def castToFunction = { (args, callerEnv) =>
      val newEnv = env.spawn
      params.zip(args.map(_.eval(callerEnv)).toSeq).foreach {
        case (p, a) =>
          newEnv.define(p, a)
      }
      body.eval(newEnv)
    }
  }
  case class SPrim(f: SList => Expr) extends Atom {
    override def castToFunction = { (args, env) => f(args.map(_.eval(env))) }
  }
  case class SSpecial(name: String, f: (SList, Env) => Expr) extends Atom {
    override def castToFunction = f
  }

  def apply(v: Any): Expr = v match {
    case e: Expr => e
    case i: Int => SInt(i)
    case b: Boolean => SBoolean(b)
    case s: String => SString(s)
    case s: Symbol => SSymbol(s)
    case seq: Seq[_] => seq.map(apply).foldRight[Expr](SNil) { (e, l) => SCons(e, l) }
    case (a, b) => SCons(apply(a), apply(b))
    case _ => throw new IllegalArgumentException
  }

  def rep(e: Expr): String = e match {
    case e: SList => repList(e)
    case SInt(value) => value.toString
    case SBoolean(value) => value.toString
    case SString(value) => s""""${value.replace("\\", "\\\\").replace("\"", "\\\"")}""""
    case SSymbol(value) => value.name
    case SPrim(value) => "<primitive>"
    case SLambda(_, _, _) => "<lambda>"
    case SSpecial(name, value) => s"<special:${name}>"
  }

  private[this] def repList(e: SList, close: Boolean = true): String = {
    val elms =
      e match {
        case SNil => ""
        case SCons(car, SNil) => rep(car)
        case SCons(car, cdr: SCons) => s"${rep(car)} ${repList(cdr, false)}"
        case SCons(car, cdr) => s"${rep(car)} . ${rep(cdr)}"
      }
    if (close) s"($elms)" else elms
  }

  def list(exprs: Expr*): Expr =
    exprs.foldRight[Expr](SNil) { (e, l) => SCons(e, l) }

  def parse(src: String): Expr = Parser.parse(src)

  def primitive(f: SList => Expr): Expr =
    SPrim(f)

  def primitiveP(f: PartialFunction[SList, Expr]): Expr =
    primitive(f orElse { case err => throw new EvalError(s"Illegal argument: ${err.rep}") })

  def special(name: String)(f: (SList, Env) => Expr): Expr =
    SSpecial(name, f)
}

