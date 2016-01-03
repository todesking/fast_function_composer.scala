package com.todesking.fast_function_composer.test

import org.scalatest.{ FunSpec, Matchers }

import com.todesking.fast_function_composer.FastComposable

class Spec extends FunSpec with Matchers {
  describe("FastComposable") {
    it("should make function fast composable") {
      import FastComposable.{ hint, noHint }

      val f1 = hint { x: Int => x + 1 }
      val f2 = hint { x: Int => x.toString }

      (f1 andThen f2)(1) should be("2")
      (f2 compose f1)(1) should be("2")

      FastComposable.compile(f1 andThen f2)(1) should be("2")
      FastComposable.compile(f2 compose f1)(1) should be("2")
      FastComposable.compile(hint(f2) compose noHint(f1 andThen identity))(1) should be("2")
    }

    it("should combine primitive/boxed types") {
      import FastComposable.{ hint, noHint }
      val fII = hint { x: Int => x + 1 }
      val fID = hint { x: Int => x.toDouble }
      val fAnyString = hint { x: Any => x.toString }
      val fStringI = hint { x: String => x.toDouble.toInt }

      val fc = fII andThen fID andThen fAnyString andThen fStringI

      FastComposable.inspect(fc) should be("(I => I) >>> (I => D) >>> (L => L) >>> (L => I)")

      fc(1) should be(2)
      val cfc = FastComposable.compile(fc, false)
      cfc(1) should be(2)

      FastComposable.inspect(cfc) should be("Compiled[(I => I) >>> (I => D) >>> (L => L) >>> (L => I)]")
    }
  }
}
