package com.todesking.fast_function_composer

import scala.reflect.runtime.universe.{ typeTag, TypeTag }

import scala.language.existentials

sealed abstract class FastComposable[A, B] extends Function1[A, B] {
  override def compose[C](f: C => A): C => B =
    FastComposable.Compose(FastComposable.noHint(f), this)
  override def andThen[C](f: B => C): A => C =
    FastComposable.Compose(this, FastComposable.noHint(f))
}

object FastComposable {
  def hint[A: TypeTag, B: TypeTag](f: A => B): FastComposable[A, B] = f match {
    case NativeNoHint(f1) => NativeHint(f1, Sig.of[A], Sig.of[B])
    case f1: FastComposable[A, B] => f1
    case f1 => NativeHint(f1, Sig.of[A], Sig.of[B])
  }

  def noHint[A, B](f: A => B): FastComposable[A, B] = f match {
    case f1: FastComposable[A, B] => f1
    case f1 => NativeNoHint(f1)
  }

  def compile[A, B](f: A => B, aggressive: Boolean = false): A => B =
    Compiler.compile(f, aggressive)

  def compileMH[@specialized(Double, Int) A, @specialized(Double, Int) B](f: A => B): A => B =
    Compiler.compileMH[A, B](f)

  def inspect[A, B](f: A => B): String = f match {
    case NativeHint(_, sigA, sigB) => s"(${sigA.char} => ${sigB.char})"
    case Compose(f1, f2) => s"${inspect(f1)} >>> ${inspect(f2)}"
    case c: Compiled[A, B] => s"Compiled[${inspect(c.src)}]"
    case f1 => "(native)"
  }

  sealed abstract class Native[A, B] extends FastComposable[A, B] {
    def unwrap: A => B
  }

  case class NativeHint[A, B](override val unwrap: A => B, sigA: Sig, sigB: Sig) extends Native[A, B] {
    override def apply(a: A): B = unwrap(a)
  }

  case class NativeNoHint[A, B](override val unwrap: A => B) extends Native[A, B] {
    override def apply(a: A): B = unwrap(a)
  }

  case class Compose[A, B, C](f1: FastComposable[A, B], f2: FastComposable[B, C]) extends FastComposable[A, C] {
    override def apply(a: A): C = f2(f1(a))

    def toSeq: Seq[Native[_, _]] =
      toSeq(f1) ++ toSeq(f2)

    private[this] def toSeq(f: FastComposable[_, _]): Seq[Native[_, _]] = f match {
      case c @ Compose(_, _) => c.toSeq
      case f: Native[_, _] => Seq(f)
    }
  }
}

