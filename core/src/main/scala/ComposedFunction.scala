package com.todesking.fast_function_composer

sealed abstract class ComposedFunction[A, B] extends Function1[A, B]

object ComposedFunction {
  type ~~>[A, B] = ComposedFunction[A, B]

  case class Native[A, B](f: A => B, sigA: Char, sigB: Char) extends ComposedFunction[A, B] {
    override def apply(a: A): B = f(a)
  }

  case class NativeNoHint[A, B](f: A => B) extends ComposedFunction[A, B] {
    override def apply(a: A): B = f(a)
  }

  case class Identity[A]() extends ComposedFunction[A, A] {
    override def apply(a: A): A = a
  }

  case class Compose[A, B, C](f1: B ~~> C, f2: A ~~> B) extends ComposedFunction[A, C] {
    override def apply(a: A): C = f1(f2(a))
  }

  case class Split[A]() extends ComposedFunction[A, (A, A)] {
    override def apply(a: A): (A, A) = (a -> a)
  }

  case class Join[A, B](f: (A, A) ~~> B) extends ComposedFunction[(A, A), B] {
    override def apply(a: (A, A)): B = f(a)
  }

  case class Parallel[A, B, C, D](first: A ~~> B, second: C ~~> D) extends ComposedFunction[(A, C), (B, D)] {
    override def apply(ac: (A, C)): (B, D) = (first(ac._1) -> second(ac._2))
  }
}
