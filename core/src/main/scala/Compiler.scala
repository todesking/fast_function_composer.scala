package com.todesking.fast_function_composer

import scala.reflect.runtime.universe.{ typeTag, TypeTag }
import java.lang.invoke.{ MethodHandles, MethodHandle, MethodType }

import scala.language.existentials

object Compiler {
  import FastComposable.{Native, NativeHint,NativeNoHint, Compose}

  def typeHintA[A, B](f: A => B, aggressive: Boolean): Sig =
    typeHint(f, aggressive)._1

  def typeHintB[A, B](f: A => B, aggressive: Boolean): Sig =
    typeHint(f, aggressive)._2

  def typeHint[A, B](f: A => B, aggressive: Boolean): (Sig, Sig) = f match {
    case NativeHint(_, sigA, sigB) => (sigA -> sigB)
    case NativeNoHint(f1) => typeHint(f1, aggressive)
    case Compose(f1, f2) => (typeHintA(f1, aggressive) -> typeHintB(f2, aggressive))
    case f1 if aggressive => typeHintAggressive(f1)
    case _ => (Sig.AnyRef -> Sig.AnyRef)
  }

  def compile[A, B](f: A => B, aggressive: Boolean = false): A => B = f match {
    case f @ Compose(f1, f2) => compileComposed(f, aggressive)
    case f1: Native[A, B] => f1.unwrap
    case f1 => f1
  }

  def compileMH[A, B](f: A => B): A => B = f match {
    case f1: Native[A, B] => f1.unwrap
    case c @ Compose(_, _) => compileMH(c.toSeq)
    case f1 => f1
  }

  def compileMH[A, B](fs: Seq[Native[_, _]], aggressive: Boolean = true): A => B = {
    val lookup = MethodHandles.lookup()
    if (fs.isEmpty) {
      (identity _).asInstanceOf[A => B]
    } else {
      val sigFirst = typeHintA(fs.head, aggressive)
      val methodHandle =
        fs.foldLeft(MethodHandles.identity(sigFirst.klass)) { (mhL, r) =>
          val rA = typeHintA(r, aggressive)
          val rB = typeHintB(r, aggressive)
          val lB = Sig.of(mhL.`type`.returnType)
          val lr = Sig.common(lB, rA)
          val (apB, apA, apName) = specializedApply(rB, lr)
          val nakedR = r.unwrap
          val apType = MethodType.methodType(apB.klass, apA.klass)
          val mhR = lookup.findVirtual(nakedR.getClass, apName, apType).bindTo(nakedR)
          MethodHandles.filterReturnValue(autoBox(mhL, apA), mhR)
        }
      val sigLast = Sig.of(methodHandle.`type`.returnType)

      MethodHandleFunction1(methodHandle, sigFirst, sigLast).asInstanceOf[A => B]
    }
  }

  private[this] def autoBox(mh: MethodHandle, sig: Sig): MethodHandle = {
    val mhSig = Sig.of(mh.`type`.returnType)
    if(mhSig == sig) mh
    else if(mhSig == Sig.AnyRef) { // need unbox
      val unbox = MethodHandles.lookup().findVirtual(sig.getClass, "unbox", MethodType.methodType(sig.klass, Sig.AnyRef.klass))
      MethodHandles.filterReturnValue(mh, unbox)
    } else if(sig == Sig.AnyRef) { // need box
      val box = MethodHandles.lookup().findVirtual(sig.getClass, "box", MethodType.methodType(Sig.AnyRef.klass, mhSig.klass))
      MethodHandles.filterReturnValue(mh, box)
    } else { // incompatible primitive
      throw new IllegalArgumentException(s"Incompatible primitive: ${mhSig} and ${sig}")
    }
  }

  private[this] def specializedApply(ret: Sig, arg: Sig): (Sig, Sig, String) =
    if(!Function1Meta.specializedSigsB.contains(ret) || !Function1Meta.specializedSigsA.contains(arg)) (Sig.AnyRef, Sig.AnyRef, "applly")
    else (ret, arg, s"apply$$mc${ret.char}${arg.char}$$sp")

  private[this] def typeHintAggressive[A, B](native: A => B): (Sig, Sig) = {
    val fallback = (Sig.AnyRef -> Sig.AnyRef)
    val re = """^apply\$mc(.)(.)\$sp$""".r
    val declaredMethods = native.getClass.getDeclaredMethods.map(_.getName).filter(_.matches(re.regex))
    if (declaredMethods.size == 1) {
      declaredMethods.head match {
        case `re`(a, b) => (Sig.of(b(0)) -> Sig.of(a(0)))
      }
    } else {
        fallback
    }
  }

  private[this] def compileComposed[A, B, C](c: Compose[A, B, C], aggressive: Boolean): A => C = {
    val functions = c.toSeq
    val naked = functions.map {
      case f1: Native[_, _] => f1.unwrap
      case f1 => f1
    }

    val klass = ClassGen.composerClass(functions.map(typeHint(_, aggressive)).map { case (s1, s2) => (s1.char -> s2.char) })

    klass.getConstructor(functions.map { _ => classOf[Function1[_, _]] }: _*)
      .newInstance(naked: _*)
      .asInstanceOf[A => C]
  }
}

