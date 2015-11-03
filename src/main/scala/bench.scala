package com.todesking.fast_function_composer
import scala.reflect.runtime.universe.TypeTag

import org.openjdk.jmh.annotations.{ Benchmark, State }

object Bench {
  // javassist.CtClass.debugDump = "./tmp"
  val f1: Int => Int = { x => x + 1 }
  val f2: Int => Double = { x => x + 10.0 }
  val f3: Double => Int = { x => (x * 100).toInt }
  val f4: Int => Double = { x => x + 1.5 }
  val f5: Double => Double = { x => x * 0.01 }
  val f6: Double => Double = { x => x - 200.0 }
  val f7: Double => Int = { x => x.toInt }
  val f8: Int => Int = { x => x + 10 }

  val baseline = {
    def F(x: Int) = f8(f7(f6(f5(f4(f3(f2(f1(x))))))))
    x: Int => F(F(F(F(x))))
  }

  val standardF = {
    def F = f1 andThen f2 andThen f3 andThen f4 andThen f5 andThen f6 andThen f7 andThen f8
    F andThen F andThen F andThen F
  }

  val fastF = {
    import FastComposable.{ hint, compile }
    def F = hint(f1) andThen hint(f2) andThen hint(f3) andThen hint(f4) andThen hint(f5) andThen hint(f6) andThen hint(f7) andThen hint(f8)
    compile(F andThen F andThen F andThen F)
  }

  val aggressiveF = {
    // Experimental: Same performance as fast, but no type hints required
    import FastComposable.{ hint, compile }
    def F = hint(f1) andThen f2 andThen f3 andThen f4 andThen f5 andThen f6 andThen f7 andThen f8
    compile(F andThen F andThen F andThen F, aggressive = true)
  }

  type Env = Map[Symbol, Any]

  sealed trait E[@specialized(Int, Double) A] {
    def eval(c: Env): A
    def compile(): Env => A
  }

  case class Const[@specialized(Int, Double) A](value: A) extends E[A] {
    override def eval(c: Env) = value
    override def compile() = c => value
  }

  case class VarRef[@specialized(Int, Double) A: TypeTag](name: Symbol) extends E[A] {
    override def eval(c: Env) = c(name).asInstanceOf[A]
    override def compile() = FastComposable.hint[Env, A](_(name).asInstanceOf[A])
  }

  case class Plus[@specialized(Int, Double) A: Numeric](lhs: E[A], rhs: E[A]) extends E[A] {
    override def eval(c: Env) = implicitly[Numeric[A]].plus(lhs.eval(c), rhs.eval(c))
    override def compile() = {
      val n = implicitly[Numeric[A]]
      FastComposable.splitJoin(lhs.compile(), rhs.compile()) { (vl, vr) => n.plus(vl, vr) }
    }
  }

  case class Eq[@specialized(Int, Double) A](lhs: E[A], rhs: E[A]) extends E[Boolean] {
    override def eval(c: Env) = lhs.eval(c) == rhs.eval(c)
    override def compile() =
      FastComposable.splitJoin(lhs.compile(), rhs.compile()) { (l, r) => l == r }

  }

  case class If[@specialized(Int, Double) A](cond: E[Boolean], th: E[A], el: E[A]) extends E[A] {
    override def eval(c: Env) = if (cond.eval(c)) th.eval(c) else el.eval(c)
    override def compile() = {
      val cc = cond.compile()
      val thc = th.compile()
      val elc = el.compile()
      FastComposable.select(cc, thc, elc)
    }
  }

  val env = Map('i -> 1, 's -> "foo", 'd -> 1.0)

  val exp: E[Double] = If(Eq(Const(1), Plus(Const(1), Const(1))), Const(0.1), Plus(Const(5.0), VarRef('d)))

  def normalExp() = exp.eval(env)
  lazy val compiledExp = exp.compile()
  def fastExp() = compiledExp(env)
}

class Bench {
  @Benchmark
  def baseline(): Any = {
    var x = 0
    (0 until 1000).foreach { i => x += Bench.baseline(i) }
    x
  }

  @Benchmark
  def standard(): Any = {
    var x = 0
    (0 until 1000).foreach { i => x += Bench.standardF(i) }
    x
  }

  @Benchmark
  def fast(): Any = {
    var x = 0
    (0 until 1000).foreach { i => x += Bench.fastF(i) }
    x
  }

  @Benchmark
  def aggressive(): Any = {
    var x = 0
    (0 until 1000).foreach { i => x += Bench.aggressiveF(i) }
    x
  }

  @Benchmark
  def normalExp(): Any = {
    var x = 0.0
    (0 until 1000).foreach { i => x += Bench.normalExp() }
    x
  }

  def fastExp(): Any = {
    var x = 0.0
    (0 until 1000).foreach { i => x += Bench.fastExp() }
    x
  }
}
