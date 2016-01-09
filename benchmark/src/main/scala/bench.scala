package com.todesking.fast_function_composer
import scala.reflect.runtime.universe.TypeTag

import org.openjdk.jmh.annotations.{ Benchmark, State }

object Main {
  def main(args: Array[String]): Unit = {
    println(Bench.standardF(123))
    println(Bench.fastF(123))
  }
}

object Bench {
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
    x: Int => F(F(F(F(F(F(
      F(F(F(F(F(
        F(F(F(F(x))))
      )))))
    ))))))
  }

  val standardF = {
    def F = f1 andThen f2 andThen f3 andThen f4 andThen f5 andThen f6 andThen f7 andThen f8
    F andThen F andThen F andThen F
  }

  val fastF = {
    import FastComposable.{ hint, compile }
    def F = hint(f1) andThen hint(f2) andThen hint(f3) andThen hint(f4) andThen hint(f5) andThen hint(f6) andThen hint(f7) andThen hint(f8)
    val c1 = compile(F andThen F andThen F andThen F andThen F andThen F, aggressive = false)
    val c2 = compile(F andThen F andThen F andThen F andThen F, aggressive = false)
    val c3 = compile(F andThen F andThen F andThen F, aggressive = false);
    { i: Int => c1(c2(c3(i))) }
  }

  val aggressiveF = {
    // Experimental: Same performance as fast, but no type hints required
    import FastComposable.{ hint, compile }
    def F = hint(f1) andThen f2 andThen f3 andThen f4 andThen f5 andThen f6 andThen f7 andThen f8
    val c1 = compile(F andThen F andThen F andThen F andThen F andThen F, aggressive = true)
    val c2 = compile(F andThen F andThen F andThen F andThen F, aggressive = true)
    val c3 = compile(F andThen F andThen F andThen F, aggressive = true);
    { i: Int => c1(c2(c3(i))) }
  }

  val fastMHF = {
    import FastComposable.{ hint, compileMH }
    def F = hint(f1) andThen hint(f2) andThen hint(f3) andThen hint(f4) andThen hint(f5) andThen hint(f6) andThen hint(f7) andThen hint(f8)
    val c1 = compileMH(F andThen F andThen F andThen F andThen F andThen F)
    val c2 = compileMH(F andThen F andThen F andThen F andThen F)
    val c3 = compileMH(F andThen F andThen F andThen F);
    { i: Int => c1(c2(c3(i))) }
  }
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
  def fastMH(): Any = {
    var x = 0
    (0 until 1000).foreach { i => x += Bench.fastMHF(i) }
    x
  }
}
