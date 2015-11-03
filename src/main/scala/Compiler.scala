package com.todesking.fast_function_composer

import scala.reflect.runtime.universe.{ typeTag, TypeTag }

import scala.language.existentials

object Compiler {
  def typeHintA[A, B](f: A => B, aggressive: Boolean): Option[TypeTag[_]] = f match {
    case f1: FastComposable1Hint[A, B] => Some(f1.typeHintA)
    case FastComposable2(f1, f2) => typeHintA(f1, aggressive)
    case f1 if aggressive => Some(typeHintAggressive(f1)._1)
    case _ => None
  }

  def typeHintA[A, B, C](f: (A, B) => C, aggressive: Boolean): (Option[TypeTag[_]], Option[TypeTag[_]]) = f match {
    case f1 if aggressive =>
      val (t1, t2) = typeHintAggressive(f1)._1
      (Some(t1) -> Some(t2))
    case _ => (None, None)
  }

  def typeHintB[A, B](f: A => B, aggressive: Boolean): Option[TypeTag[_]] = f match {
    case f1: FastComposable1Hint[A, B] => Some(f1.typeHintB)
    case FastComposable2(f1, f2) => typeHintB(f2, aggressive)
    case f1 if aggressive => Some(typeHintAggressive(f1)._2)
    case _ => None
  }

  def typeHintB[A, B, C](f: (A, B) => C, aggressive: Boolean): Option[TypeTag[_]] = f match {
    // case f1 @ Functionn2Hinted(_) => f1.hintB
    case f1 if aggressive => Some(typeHintAggressive(f1)._2)
    case _ => None
  }

  def compile[A, B](f: A => B, aggressive: Boolean = false): A => B = f match {
    case f1: Compiled[A, B] => f1
    case f1: FastComposable[A, B] => f1 match {
      case f1: FastComposable2[_, _, _] => compileComposed(f1, aggressive)
      case f1: FastComposable1[A, B] => f1.unwrap
      case SplitJoin(f1, f2, j) => compileSplitJoin(f1, f2, j, aggressive)
      case Select(c, t, e) => ???
    }
    case f1 => f1
  }

  private[this] val typeTagSigs = Seq[(TypeTag[_], Char)](
    (TypeTag.Boolean -> 'Z'),
    (TypeTag.Char -> 'C'),
    (TypeTag.Byte -> 'B'),
    (TypeTag.Short -> 'S'),
    (TypeTag.Int -> 'I'),
    (TypeTag.Float -> 'F'),
    (TypeTag.Long -> 'J'),
    (TypeTag.Double -> 'D')
  )

  private[this] val typeTag2Sig = typeTagSigs.toMap

  private[this] val sig2TypeTag = typeTagSigs.map(_.swap).toMap

  private[this] def typeHintAggressive[A, B](f: A => B): (TypeTag[_], TypeTag[_]) = {
    f match {
      case f1: Compiled[A, B] =>
        val (a, b) = f1.sig
        (typeTagFromSig(a) -> typeTagFromSig(b))
      case f1: FastComposable1NoHint[A, B] =>
        typeHintAggressive(f1.unwrap)
      case f1 =>
        val fallback = (TypeTag.Any -> TypeTag.Any)
        val re = """^apply\$mc(.)(.)\$sp$""".r
        val declaredMethods = f1.getClass.getDeclaredMethods.map(_.getName).filter(_.matches(re.regex))
        if (declaredMethods.size == 1) {
          declaredMethods.head match {
            case `re`(a, b) => (typeTagFromSig(b(0)) -> typeTagFromSig(a(0)))
          }
        } else {
          fallback
        }
    }
  }

  private[this] def typeHintAggressive[A, B, C](f: (A, B) => C): ((TypeTag[_], TypeTag[_]), TypeTag[_]) = {
    f match {
      case f1 =>
        val fallback = ((TypeTag.Any -> TypeTag.Any) -> TypeTag.Any)
        val re = """^apply\$mc(.)(.)(.)\$sp$""".r
        val declaredMethods = f1.getClass.getDeclaredMethods.map(_.getName).filter(_.matches(re.regex))
        if (declaredMethods.size == 1) {
          declaredMethods.head match {
            case `re`(a, b, c) => ((typeTagFromSig(b(0)) -> typeTagFromSig(c(0))) -> typeTagFromSig(a(0)))
          }
        } else {
          fallback
        }
    }
  }

  private[this] def typeTagFromSig(sig: Char): TypeTag[_] =
    sig2TypeTag.get(sig).getOrElse(TypeTag.Any)

  private[this] def sigFromTypeTag(tag: TypeTag[_]): Char =
    typeTag2Sig.get(tag).getOrElse('L')

  private[this] def sigFromTypeTag(tag: Option[TypeTag[_]]): Char =
    tag.map { t => sigFromTypeTag(t) }.getOrElse('L')

  private[this] def sigFromTypeTag(t1: Option[TypeTag[_]], t2: Option[TypeTag[_]]): Char = {
    val s1 = sigFromTypeTag(t1)
    val s2 = sigFromTypeTag(t2)

    if (s1 == 'L') s2
    else if (s2 == 'L') s1
    else if (s1 == s2) s1
    else throw new AssertionError(s"BUG: ${t1} is incompatible to ${t2}")
  }

  private[this] def compileComposed[A, B, C](c: FastComposable2[A, B, C], aggressive: Boolean): A => C = {
    val functions = c.toSeq
    val naked = functions.map {
      case f1: FastComposable1[_, _] => f1.unwrap
      case f1 => f1
    }

    val klass = ClassGen.composerClass(functions.map { f =>
      sigFromTypeTag(typeHintA(f, aggressive)) -> sigFromTypeTag(typeHintB(f, aggressive))
    })

    klass.getConstructor(functions.map { _ => classOf[Function1[_, _]] }: _*)
      .newInstance(naked: _*)
      .asInstanceOf[A => C]
  }

  private[this] def compileSplitJoin[A, B, C, D](f1: A => B, f2: A => C, j: (B, C) => D, aggressive: Boolean): A => D = {
    val sig1 = sigFromTypeTag(typeHintA(f1, aggressive), typeHintA(f2, aggressive))
    val sig21 = sigFromTypeTag(typeHintB(f1, aggressive), typeHintA(j, aggressive)._1)
    val sig22 = sigFromTypeTag(typeHintB(f2, aggressive), typeHintA(j, aggressive)._2)
    val sig3 = sigFromTypeTag(typeHintB(j, aggressive))

    val klass = ClassGen.splitJoinClass(sig1, sig21, sig22, sig3)

    klass.getConstructor(classOf[Function1[_, _]], classOf[Function1[_, _]], classOf[Function2[_, _, _]])
      .newInstance(compile(f1), compile(f2), j)
      .asInstanceOf[A => D]
  }

}

