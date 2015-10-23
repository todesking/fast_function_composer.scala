scalaVersion := "2.11.7"

libraryDependencies += "org.javassist" % "javassist" % "3.20.0-GA"
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

scalariformSettings

enablePlugins(JmhPlugin)

sourceGenerators in Compile <+= sourceManaged in Compile map { dir: File =>
  val file = dir / "fast_composed.scala"
  var src = """package com.todesking.fast_function_composer
sealed abstract class FastComposed[A, B] extends (A => B) {
  def sig: (Char, Char, Char)
}

  """
  val sigs = Seq(
    'L' -> "Any",
    'I' -> "Int",
    'D' -> "Double"
  )
  sigs.foreach { case (s1, t1) =>
    sigs.foreach { case (s2, t2) =>
      sigs.foreach { case (s3, t3) =>
        src += s"""class FastComposed${s1}${s2}${s3}(f1: ${t1} => ${t2}, f2: ${t2} => ${t3}) extends FastComposed[${t1}, ${t3}] {
  override def apply(x: ${t1}): ${t3} = f2(f1(x))
  override def sig: (Char, Char, Char) = ('$s1', '$s2', '$s3')
}
"""
      }
    }
  }
  IO.write(file, src)
  Seq(file)
}
