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

  def inspect[A, B](f: A => B): String = f match {
    case f1: FastComposable1Hint[A, B] => s"(${f1.typeHintA.tpe} => ${f1.typeHintB.tpe})"
    case f1: FastComposable1NoHint[A, B] => s"[${inspect(f1)}]"
    case FastComposable2(f1, f2) => s"${inspect(f1)} >>> ${inspect(f2)}"
    case f1: Compiled[A, B] => f1.inspect()
    case f1 => "(native)"
  }
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

object ClassGen {
  import javassist.{ ClassPool, ClassClassPath, CtClass, CtMethod }
  private[this] val classPool = {
    val p = new ClassPool(null)
    p.appendClassPath(new ClassClassPath(this.getClass))
    p
  }
  private[this] lazy val ctFunction1 = classPool.get("scala.Function1")
  private[this] lazy val ctFunction2 = classPool.get("scala.Function2")
  private[this] lazy val ctTuple2 = classPool.get("scala.Tuple2")
  private[this] lazy val ctObject = classPool.get("java.lang.Object")
  private[this] lazy val ctString = classPool.get("java.lang.String")
  private[this] lazy val ctCompiled = classPool.get(getClass.getPackage.getName + ".Compiled")
  private[this] lazy val ctPrimitiveClasses: Map[Char, CtClass] = {
    import CtClass._
    Seq(booleanType, charType, byteType, shortType, intType, longType, floatType, doubleType).map { c =>
      c.asInstanceOf[javassist.CtPrimitiveType].getDescriptor -> c
    }.toMap
  }

  private[this] var _nextClassId = 0
  private[this] def nextClassId(): Int = synchronized {
    _nextClassId += 1
    _nextClassId
  }

  private[this] var composerClassCache = Map.empty[String, Class[_]]

  def composerClass(sigs: Seq[(Char, Char)]): Class[_] = {
    require(sigs.nonEmpty)

    val fullSig = sigs.map { case (l, r) => s"${l}${r}" }.mkString("")

    if (composerClassCache.contains(fullSig))
      return composerClassCache(fullSig)

    val sigFirst = sigs.head._1
    val sigLast = sigs.last._2

    val klass = createBase(
      s"CompiledComposed${fullSig}",
      sigLast,
      sigFirst,
      sigs.zipWithIndex.map { case (_, i) => s"_f${i + 1}" -> ctFunction1 }: _*
    )

    val baseName = "apply"
    val spName = specializedName(baseName, sigLast, sigFirst)

    val fc = FastComposable.getClass.getName + ".MODULE$"
    addMethod(klass, ctString, "inspect")(
      s"""{ return "[${sigs.map { case (l, r) => s"(${l} => ${r})" }.mkString(" >>> ")}]"; }"""
    )

    def callComposed(sigR: Char, arg: String, sigArg: Char) = {
      val (expr, sigExpr) = sigs.zipWithIndex.foldLeft(arg -> sigArg) {
        case ((a, sigIn1), ((sigIn2, sigOut), i)) =>
          val expr = specialCall(
            s"this._f${i + 1}.apply",
            sigOut,
            sigIn2 -> autoBox(sigIn1, sigIn2, a)
          )
          expr -> sigOut
      }
      autoBox(sigExpr, sigR, expr)
    }

    addMethod(klass, ctObject, baseName, ctObject)(s"""{return ${callComposed('L', "$1", 'L')};}""")
    if (spName != baseName) {
      addMethod(klass, ctClassFromSig(sigLast), spName, ctClassFromSig(sigFirst))(
        s"""{ return ${callComposed(sigLast, "$1", sigFirst)};}"""
      )
    }

    val c = klass.toClass()
    composerClassCache += (fullSig -> c)
    c
  }

  def splitJoinClass(sig1: Char, sig21: Char, sig22: Char, sig3: Char): Class[_] = {
    val klass = createBase(
      s"SplitJoinCompiled${sig1}${sig21}${sig22}${sig3}",
      sig1,
      sig3,
      "_f1" -> ctFunction1,
      "_f2" -> ctFunction1,
      "_j" -> ctFunction2
    )

    val baseName = "apply"
    val spName = specializedName(baseName, sig3, sig1)
    if (spName == baseName) {
      addMethod(klass, ctObject, baseName, ctObject)(
        "{ return " + box(sig3, specialCall(
          "this._j.apply",
          sig3,
          (sig21 -> specialCall("this._f1.apply", sig21, sig1 -> unbox(sig1, "$1"))),
          (sig22 -> specialCall("this._f2.apply", sig22, sig1 -> unbox(sig1, "$1")))
        )) + ";}"
      )
    } else {
      addMethod(klass, ctClassFromSig(sig3), spName, ctClassFromSig(sig1))(
        "{ return " + specialCall(
          "this._j.apply",
          sig3,
          (sig21 -> specialCall("this._f1.apply", sig21, sig1 -> "$1")),
          (sig22 -> specialCall("this._f2.apply", sig22, sig1 -> "$1"))
        ) + ";}"
      )

      addMethod(klass, ctClassFromSig(sig3), baseName, ctClassFromSig(sig1))(
        s"{return ${spName}(${unbox(sig1, "$1")});}"
      )
    }
    addMethod(klass, ctString, "inspect")(s"""{
      return "[${sig1} ={${sig21}|${sig22}}=> ${sig3}]";
    }""")

    klass.toClass()
  }

  private[this] def addMethod(klass: CtClass, retType: CtClass, name: String, args: CtClass*)(body: String): Unit = {
    import javassist.CtNewMethod
    import javassist.Modifier
    val method = CtNewMethod.make(
      Modifier.PUBLIC | Modifier.FINAL,
      retType,
      name,
      args.toArray,
      Array[CtClass](),
      body,
      klass
    )
    klass.addMethod(method)
  }

  private[this] def createBase(baseName: String, sigA: Char, sigB: Char, fields: (String, CtClass)*): CtClass = {
    import javassist.{ CtField, CtConstructor }
    import javassist.bytecode.AccessFlag

    val name = getClass.getPackage.getName + "." + baseName + nextClassId().toString

    val klass = classPool.makeClass(name, ctCompiled)

    fields.map {
      case (fname, ftype) =>
        new CtField(ftype, fname, klass)
    }.foreach { field =>
      field.getFieldInfo.setAccessFlags(AccessFlag.PRIVATE | AccessFlag.FINAL)
      klass.addField(field)
    }

    val co = new CtConstructor(fields.map(_._2).toArray, klass)
    co.setBody("{ "
      + fields.map(_._1).zipWithIndex.map { case (fname, i) => s"this.${fname} = $$${i + 1};" }.mkString("\n")
      + "}")
    klass.addConstructor(co)

    addMethod(klass, ctTuple2, "sig")(s"""{ return new scala.Tuple2(($$w)'${sigA}', ($$w)'${sigB}'); }""")

    klass
  }

  private[this] def ctClassFromSig(sig: Char): CtClass = {
    if (sig == 'L') ctObject
    else ctPrimitiveClasses(sig)
  }

  private[this] def autoBox(sigFrom: Char, sigTo: Char, expr: String): String =
    if (sigFrom == sigTo) expr
    else if (sigFrom == 'L') unbox(sigTo, expr)
    else if (sigTo == 'L') box(sigFrom, expr)
    else throw new IllegalArgumentException(s"Incompatible: ${sigFrom} to ${sigTo}")

  private[this] def box(sig: Char, expr: String): String = {
    sig match {
      case 'L' => expr
      case _ => s"($$w)(${expr})"
    }
  }
  private[this] def unbox(sig: Char, expr: String) =
    sig match {
      case 'L' => expr
      case 'I' => s"((java.lang.Integer)(${expr})).intValue()"
      case 'D' => s"((java.lang.Double)(${expr})).doubleValue()"
    }

  private[this] def specializedName(base: String, sigR: Char, sigs: Char*): String =
    if (sigR == 'L' || sigs.exists(_ == 'L')) base
    else s"${base}$$mc${sigR}${sigs.mkString("")}$$sp"

  private[this] def specialCall(base: String, sigR: Char, args: (Char, String)*): String = {
    val method = specializedName(base, sigR, args.map(_._1): _*)
    if (method == base) {
      unbox(sigR, s"${method}(${args.map { case (s, a) => box(s, a) }.mkString(", ")})")
    } else {
      s"${method}(${args.map(_._2).mkString(", ")})"
    }
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
