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

    it("should make a function from two function1 and a combinator") {
      import FastComposable.{ hint, noHint }
      val f = FastComposable.splitJoin[Int, String, Double, String](hint { x: Int => (x + 1).toString }, hint { x: Int => x + 10.0 }) { (x1, x2) => x1 + x2.toString }

      f(99) should be("100109.0")

      FastComposable.compile(f, true)(99) should be("100109.0")
    }
  }
}
