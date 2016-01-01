package com.todesking.fast_function_composer

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
