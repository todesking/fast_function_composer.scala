package com.todesking.fast_function_composer

import scala.reflect.runtime.universe.{ typeTag, TypeTag }

import scala.language.existentials

// class FastComposed* is auto generated in build time. See build.sbt

object FastComposable {
  def hint[A: TypeTag, B: TypeTag](f: A => B): FastComposable[A, B] = f match {
    case f1: FastComposable1NoHint[A, B] => f1.hint
    case f1: FastComposable[A, B] => f1
    case f1 => new FastComposable1Hint(f1)
  }

  def noHint[A, B](f: A => B): A => B = f match {
    case f1: FastComposable[A, B] => f1
    case f1 => new FastComposable1NoHint(f1)
  }

  def splitJoin[A, B, C, D](f: A => B, g: A => C)(j: (B, C) => D): FastComposable[A, D] =
    SplitJoin(f, g, j)

  def select[A, B](cond: A => Boolean, the: A => B, els: A => B): FastComposable[A, B] =
    Select(cond, the, els)

  def compile[A, B](f: A => B, aggressive: Boolean = false): A => B =
    Compiler.compile(f, aggressive)

  def compileMH[@specialized(Double, Int) A, @specialized(Double, Int) B](f: A => B): A => B =
    Compiler.compileMH[A, B](f)

  def inspect[A, B](f: A => B): String = f match {
    case f1: FastComposable1Hint[A, B] => s"(${f1.typeHintA.tpe} => ${f1.typeHintB.tpe})"
    case f1: FastComposable1NoHint[A, B] => s"[${inspect(f1)}]"
    case FastComposable2(f1, f2) => s"${inspect(f1)} >>> ${inspect(f2)}"
    case f1: Compiled[A, B] => f1.inspect()
    case f1 => "(native)"
  }
}

sealed abstract class FastComposable[A, B] extends (A => B) {
  override def andThen[C](g: B => C): FastComposable[A, C] =
    new FastComposable2(this, FastComposable.noHint(g))

  override def compose[C](g: C => A): FastComposable[C, B] =
    new FastComposable2(FastComposable.noHint(g), this)

  def andThenH[C](g: B => C)(implicit t1: TypeTag[B], t2: TypeTag[C]): FastComposable[A, C] =
    andThen(FastComposable.hint(g))
}

sealed abstract class FastComposable1[A, B](f: A => B) extends FastComposable[A, B] {
  def unwrap: A => B = f
  override def apply(a: A): B = f(a)
}

final class FastComposable1Hint[A: TypeTag, B: TypeTag](f: A => B) extends FastComposable1[A, B](f) {
  def typeHintA: TypeTag[A] = implicitly[TypeTag[A]]
  def typeHintB: TypeTag[B] = implicitly[TypeTag[B]]
}

final class FastComposable1NoHint[A, B](f: A => B) extends FastComposable1[A, B](f) {
  def hint(implicit t1: TypeTag[A], t2: TypeTag[B]): FastComposable1Hint[A, B] =
    new FastComposable1Hint(f)
}

final case class FastComposable2[A, B, C](f1: A => B, f2: B => C) extends FastComposable[A, C] {
  override def apply(a: A): C = f2(f1(a))

  def toSeq: Seq[Function1[_, _]] = toSeq(f1) ++ toSeq(f2)

  private[this] def toSeq(f: Function1[_, _]): Seq[Function1[_, _]] = f match {
    case f1: FastComposable2[_, _, _] => f1.toSeq
    case f1 => Seq(f1)
  }
}

final case class SplitJoin[A, B, C, D](f: A => B, g: A => C, j: (B, C) => D) extends FastComposable[A, D] {
  override def apply(a: A) = j(f(a), g(a))
}

final case class Select[A, B](cond: A => Boolean, the: A => B, els: A => B) extends FastComposable[A, B] {
  override def apply(a: A) = if (cond(a)) the(a) else els(a)
}

abstract class Compiled[A, B] extends (A => B) {
  def sig: (Char, Char)
  def inspect(): String
}
