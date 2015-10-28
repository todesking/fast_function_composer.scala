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

      fc(1) should be(2)
      FastComposable.compile(fc, false)(1) should be(2)

      val fs = FastComposable.splitJoin(fII, fID) { (a: Any, b: Any) => a.toString + b.toString }
      fs(1) should be("21.0")
      FastComposable.compile(fs)(1) should be("21.0")

      val fsPrim = FastComposable.splitJoin(fII, fID) { (a: Int, b: Double) => a.toString + b.toString }
      fsPrim(1) should be("21.0")
      FastComposable.compile(fsPrim)(1) should be("21.0")
    }

    it("should make a function from two function1 and a combinator") {
      import FastComposable.{ hint, noHint }
      val f = FastComposable.splitJoin[Int, String, Double, String](hint { x: Int => (x + 1).toString }, hint { x: Int => x + 10.0 }) { (x1, x2) => x1 + x2.toString }

      f(99) should be("100109.0")

      FastComposable.compile(f, true)(99) should be("100109.0")
    }
  }
}
