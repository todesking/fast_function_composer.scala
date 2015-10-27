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
}

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
    case f1: FastComposed[A, B] => f1
    case f1: FastComposable[A, B] => f1 match {
      case FastComposable2(f1, f2) => compileComposed(f1, f2, aggressive)
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

  private[this] def compileComposed[A, B, C](f1: A => B, f2: B => C, aggressive: Boolean): A => C = {
    val cf1 = compile(f1, aggressive)
    val cf2 = compile(f2, aggressive)

    val sig1 = sigFromTypeTag(typeHintA(f1, aggressive))
    val sig2 = sigFromTypeTag(typeHintB(f1, aggressive), typeHintA(f2, aggressive))
    val sig3 = sigFromTypeTag(typeHintB(f2, aggressive))

    val klass = ClassGen.composerClass(sig1, sig2, sig3)

    klass.getConstructor(classOf[Function1[_, _]], classOf[Function1[_, _]])
      .newInstance(cf1, cf2)
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

object ClassGen {
  import javassist.{ ClassPool, ClassClassPath, CtClass, CtMethod }
  private[this] val classPool = {
    val p = new ClassPool(null)
    p.appendClassPath(new ClassClassPath(this.getClass))
    p
  }

  def composerClass(sig1: Char, sig2: Char, sig3: Char): Class[_] = {
    def getClass(name: String): Option[Class[_]] = try {
      Some(Class.forName(this.getClass.getPackage.getName + ".FastComposed" + name, true, this.getClass.getClassLoader))
    } catch {
      case _: ClassNotFoundException => None
    }

    val template = getClass(s"${sig1}${sig2}${sig3}").orElse(getClass(s"L${sig2}L")).orElse(getClass("LLL")).get

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

  def splitJoinClass(sig1: Char, sig21: Char, sig22: Char, sig3: Char): Class[_] = {
    val sup = classPool.get(getClass.getPackage.getName + ".FastComposed")
    val klass = classPool.makeClass(s"${getClass.getPackage.getName}.SplitJoinCompiled${sig1}${sig21}${sig22}${sig3}_${nextClassId()}", sup)

    import javassist.{ CtNewMethod, CtConstructor, CtField, Modifier }
    import javassist.bytecode.AccessFlag

    val ctF1 = classPool.get("scala.Function1")
    val ctF2 = classPool.get("scala.Function2")
    val ctObject = classPool.get("java.lang.Object")

    val _f1 = new CtField(ctF1, "_f1", klass)
    val _f2 = new CtField(ctF1, "_f2", klass)
    val _j = new CtField(ctF2, "_j", klass)
    Seq(_f1, _f2, _j) foreach { field =>
      field.getFieldInfo.setAccessFlags(AccessFlag.PRIVATE | AccessFlag.FINAL)
      klass.addField(field)
    }

    val co = new CtConstructor(Array[CtClass](ctF1, ctF1, ctF2), klass)
    co.setBody("""{
      this._f1 = $1;
      this._f2 = $2;
      this._j = $3;
    }""")
    klass.addConstructor(co)

    def specializedName(base: String, sigR: Char, sigs: Char*): String =
      if (sigR == 'L' || sigs.exists(_ == 'L')) base
      else s"${base}$$mc${sigR}${sigs.mkString("")}$$sp"

    def box(sig: Char, expr: String): String = {
      sig match {
        case 'L' => expr
        case _ => s"($$w)(${expr})"
      }
    }
    def unbox(sig: Char, expr: String) =
      sig match {
        case 'L' => expr
        case 'I' => s"((java.lang.Integer)(${expr})).intValue()"
        case 'D' => s"((java.lang.Double)(${expr})).doubleValue()"
      }

    def specialCall(base: String, sigR: Char, args: (Char, String)*): String = {
      val method = specializedName(base, sigR, args.map(_._1): _*)
      if (method == base) {
        s"${method}(${args.map { case (s, a) => box(s, a) }.mkString(", ")})"
      } else {
        s"${method}(${args.map(_._2).mkString(", ")})"
      }
    }

    val baseName = "apply"
    val spName = specializedName(baseName, sig3, sig1)
    if (spName == baseName) {
      val ap = CtNewMethod.make(
        Modifier.PUBLIC | Modifier.FINAL,
        ctObject,
        baseName,
        Array[CtClass](ctObject),
        Array[CtClass](),
        "{ return ($w)(" + specialCall(
          "this._j.apply",
          sig3,
          (sig21 -> specialCall("this._f1.apply", sig21, sig1 -> unbox(sig1, "$1"))),
          (sig22 -> specialCall("this._f2.apply", sig22, sig1 -> unbox(sig1, "$1")))
        ) + ");}",
        klass
      )
      klass.addMethod(ap)
    } else {
      val apsp = CtNewMethod.make(
        Modifier.PUBLIC | Modifier.FINAL,
        ctClassFromSig(sig3),
        spName,
        Array[CtClass](ctClassFromSig(sig1)),
        Array[CtClass](),
        "{ return " + specialCall(
          "this._j.apply",
          sig3,
          (sig21 -> specialCall("this._f1.apply", sig21, sig1 -> "$1")),
          (sig22 -> specialCall("this._f2.apply", sig22, sig1 -> "$1"))
        ) + ";}",
        klass
      )
      klass.addMethod(apsp)

      val ap = CtNewMethod.make(
        Modifier.PUBLIC | Modifier.FINAL,
        ctClassFromSig(sig3),
        baseName,
        Array[CtClass](ctClassFromSig(sig1)),
        Array[CtClass](),
        s"{return ${spName}(${box(sig1, "$1")});}",
        klass
      )
      klass.addMethod(ap)
    }

    val k = klass.toClass()
    k
  }

  private[this] def ctClassFromSig(sig: Char): CtClass = {
    val classes = {
      import CtClass._
      Seq(booleanType, charType, byteType, shortType, intType, longType, floatType, doubleType).map { c =>
        c.asInstanceOf[javassist.CtPrimitiveType].getDescriptor -> c
      }.toMap
    }
    if (sig == 'L')
      classPool.get("java.lang.Object")
    else
      classes(sig)
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

final case class SplitJoin[A, B, C, D](f: A => B, g: A => C, j: (B, C) => D) extends FastComposable[A, D] {
  override def apply(a: A) = j(f(a), g(a))
}

final case class Select[A, B](cond: A => Boolean, the: A => B, els: A => B) extends FastComposable[A, B] {
  override def apply(a: A) = if (cond(a)) the(a) else els(a)
}

abstract class FastComposed[A, B] extends (A => B) {
  def sig: (Char, Char, Char)
}
