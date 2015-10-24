# Fast Function Composer

If you want to compose MANY small functions into big one, and use it 10000 times/sec,
Scala's function composition is not fast enough.

## Usage

```scala
import com.todesking.fast_function_composer.FastComposable.{hint, noHint, compile}

// hint(f) wrap the function to 'fast composable' form.
// And preserve its type information.
val f1 = hint { x: Int => x + 1 }
val f2 = hint[Int, Double](_.toDouble)

def foo(f: A => B) = {
  // noHint(f) is hint(f) without type information.
  // Could be used when type information is unavailable.
  noHint(f) andThen ...
}

// Composition of wrapped function is also fast composable.
val f = f1 andThen f2 andThen identity


// You can use composed function directly (but not so fast)
f(1)

// Compiled version of f. Fast.
val compiled = compile(f)
compiled(1)


val f1 = noHint { x: Int => x + 1 }
val f2 = { x: Int => x.toDouble }
// EXPERIMENTAL: When aggressive, compile() could extract type information from native function.
val compiledA = compile(f1 andThen f2, aggressive = true)

```
