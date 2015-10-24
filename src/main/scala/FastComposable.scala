package com.todesking.fast_function_composer

import scala.reflect.runtime.universe.{ typeTag, TypeTag }

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

  def typeHintA[A, B](f: A => B, aggressive: Boolean): Option[TypeTag[_]] = f match {
    case f1: FastComposable1Hint[A, B] => Some(f1.typeHintA)
    case FastComposable2(f1, f2) => typeHintA(f1, aggressive)
    case f1 if aggressive => Some(typeHintAggressive(f1)._1)
    case _ => None
  }

  def typeHintB[A, B](f: A => B, aggressive: Boolean): Option[TypeTag[_]] = f match {
    case f1: FastComposable1Hint[A, B] => Some(f1.typeHintB)
    case FastComposable2(f1, f2) => typeHintB(f2, aggressive)
    case f1 if aggressive => Some(typeHintAggressive(f1)._2)
    case _ => None
  }

  def compile[A, B](f: A => B, aggressive: Boolean = false): A => B = f match {
    case FastComposable2(f1, f2) => compileComposed(f1, f2, aggressive)
    case f1: FastComposable1[A, B] => f1.unwrap
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
      case f1: FastComposed[A, B] =>
        val (a, _, b) = f1.sig
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

  private[this] def typeTagFromSig(sig: Char): TypeTag[_] =
    sig2TypeTag.get(sig).getOrElse(TypeTag.Any)

  private[this] def sigFromTypeTag(tag: TypeTag[_]): Char =
    typeTag2Sig.get(tag).getOrElse('L')

  private[this] def compileComposed[A, B, C](f1: A => B, f2: B => C, aggressive: Boolean): A => C = {
    val cf1 = compile(f1, aggressive)
    val cf2 = compile(f2, aggressive)

    val klass = composerKlass(
      typeHintA(f1, aggressive),
      typeHintB(f1, aggressive),
      typeHintA(f2, aggressive),
      typeHintB(f2, aggressive)
    )

    klass.getConstructor(classOf[Function1[_, _]], classOf[Function1[_, _]])
      .newInstance(cf1, cf2)
      .asInstanceOf[A => C]
  }

  private[this] def composerKlass(t1: Option[TypeTag[_]], t2: Option[TypeTag[_]], t3: Option[TypeTag[_]], t4: Option[TypeTag[_]]): Class[_] = {
    def sig(t: Option[TypeTag[_]]): Char =
      t.map(sigFromTypeTag).getOrElse('L')

    def sig2(t1: Option[TypeTag[_]], t2: Option[TypeTag[_]]): Char = {
      val s1 = sig(t1)
      val s2 = sig(t2)

      if (s1 == 'L') s2
      else if (s2 == 'L') s1
      else if (s1 == s2) s1
      else throw new AssertionError(s"BUG: ${t1} is incompatible to ${t2}")
    }

    def getClass(name: String): Option[Class[_]] = try {
      Some(Class.forName(this.getClass.getPackage.getName + ".FastComposed" + name, true, this.getClass.getClassLoader))
    } catch {
      case _: ClassNotFoundException => None
    }

    val sa = sig(t1)
    val sb = sig2(t2, t3)
    val sc = sig(t4)

    val template = getClass(s"${sa}${sb}${sc}").orElse(getClass(s"L${sb}L")).orElse(getClass("LLL")).get

    import javassist.{ ClassPool, ClassClassPath }
    val pool = ClassPool.getDefault()
    pool.appendClassPath(new ClassClassPath(this.getClass))
    val ct = pool.get(template.getName)

    ct.setName(s"${template.getName}_${nextClassId()}")
    ct.toClass()
  }

  private[this] var _nextClassId = 0
  private[this] def nextClassId(): Int = synchronized {
    _nextClassId += 1
    _nextClassId
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
}
