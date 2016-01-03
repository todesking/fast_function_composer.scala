name := "fast_function_composer"

organization := "com.todesking"

version := "0.9.0-SNAPSHOT"

scalaVersion := "2.11.7"

publishTo := Some(Resolver.file("com.todesking",file("./repo/"))(Patterns(true, Resolver.mavenStyleBasePattern)))

libraryDependencies += "org.javassist" % "javassist" % "3.20.0-GA"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

scalacOptions ++= Seq("-feature", "-deprecation")

sourceGenerators in Compile <+= sourceManaged in Compile map { outDir: File =>
  val sigs = Seq(
    ("Boolean", 'Z', "java.lang.Boolean.TYPE"),
    ("Short", 'S', "java.lang.Short.TYPE"),
    ("Char", 'C', "java.lang.Character.TYPE"),
    ("Byte", 'B', "java.lang.Byte.TYPE"),
    ("Int", 'I', "java.lang.Integer.TYPE"),
    ("Long", 'J', "java.lang.Long.TYPE"),
    ("Float", 'F', "java.lang.Float.TYPE"),
    ("Double", 'D', "java.lang.Double.TYPE"),
    ("AnyRef", 'L', "classOf[java.lang.Object]")
  )
  val f1A = Set("Int", "Long", "Float", "Double")
  val f1B = Set("Boolean", "Int", "Float", "Long", "Double")
  val f1Asigs = sigs.filter { s => f1A.contains(s._1) }
  val f1Bsigs = sigs.filter { s => f1B.contains(s._1) }
  val function1Meta = outDir / "Function1Meta.scala"
  IO.write(function1Meta, s"""
package com.todesking.fast_function_composer
object Function1Meta {
  val specializedSigsA = Set[Sig](${f1A.map { s => s"Sig.${s}" }.mkString(", ")})
  val specializedSigsB = Set[Sig](${f1B.map { s => s"Sig.${s}" }.mkString(", ")})
}
""")
  val sig = outDir / "Sig.scala"
  IO.write(sig, s"""
package com.todesking.fast_function_composer
import scala.reflect.runtime.universe.TypeTag
sealed abstract class Sig(val char: Char, val klass: Class[_]) {
}
object Sig {
${sigs.map { case (ty, ch, kl) =>
  s"""case object ${ty} extends Sig('${ch}', ${kl})"""
}.mkString("\n")}
  val all = Seq(${sigs.map(_._1).mkString(",")})
  def of[A](implicit t: TypeTag[A]): Sig =
    ${sigs.filterNot(_._1 == "AnyRef").map { s => s"if(t == TypeTag.${s._1}) ${s._1}"}.mkString("\n    else ")}
    else AnyRef
  def of(c: Char): Sig =
    all.find(_.char == c) getOrElse AnyRef
  def of(k: Class[_]): Sig =
    all.find(_.klass == k) getOrElse AnyRef
  def common(a: Sig, b: Sig): Sig =
    if(a == b) a
    else if(a == AnyRef || b == AnyRef) AnyRef
    else throw new IllegalArgumentException(s"Incompatible sig: $${a} and $${b}")
}
""")
  val mhf1 = outDir / "MethodHandleFunction1.scala"
  IO.write(mhf1, s"""
package com.todesking.fast_function_composer
import java.lang.invoke.MethodHandle
sealed abstract class MethodHandleFunction1[A, B](src: FastComposable[A, B]) extends Compiled[A, B](src)
object MethodHandleFunction1 {
${f1Asigs.flatMap { case (ty1, ch1, _) => f1Bsigs.map { case (ty2, ch2, _) =>
    s"""
class ${ch1}${ch2}(mh: MethodHandle, src: FastComposable[${ty1}, ${ty2}]) extends MethodHandleFunction1[${ty1}, ${ty2}](src) {
  override def apply(v: ${ty1}): ${ty2} = mh.invokeExact(v)
}"""
  }
}.mkString("\n")}
  def apply[A, B](mh: MethodHandle, sigA: Sig, sigB: Sig, src: FastComposable[A, B]): MethodHandleFunction1[A, B] = (sigA.char, sigB.char) match {
${f1Asigs.flatMap { case (ty1, ch1, _) => f1Bsigs.map { case (ty2, ch2, _) =>
s"""    case ('${ch1}', '${ch2}') => new ${ch1}${ch2}(mh, src.asInstanceOf[FastComposable[${ty1}, ${ty2}]]).asInstanceOf[MethodHandleFunction1[A, B]]"""
}}.mkString("\n")}
    case (a, b) => throw new IllegalArgumentException(s"Unsupported MethodHandle type: $${a} => $${b}")
  }
}
""")
  Seq(function1Meta, sig, mhf1)
}
