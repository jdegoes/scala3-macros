package multistageProgramming

/**
 * STAGING
 *
 * Imagine that we wanted to compile a method at runtime, based on values that weren't available
 * at compile-time. Fragments of code could be included or excluded based on runtime values, and
 * calculations could be replaced with constant values. Staging provides this facility, reusing
 * much of the same machinery of quotes and splices. You can think of it as a more elegant way of
 * writing a program which a) generates some source files, b) starts a new compiler process to turn
 * them into bytecode, and c) dynamically loads them.
 *
 * There are a variety of different uses for staging, but most are motivated by performance.
 */

object exercise1
  /**
   * EXERCISE 1
   * 
   * Take another look at `polynomial.scala`, in particular the `apply` method of the `Polynomial`
   * case class. What are the steps it must take to evaluate a polynomial? The coefficients are
   * all stored in a data structure (a `Map[Int, Double]`), and we must navigate that data structure
   * every time. If we were performing a numerical integration, we may need to do that 100,000
   * times!
   *
   * If the coefficients were all known at compile-time, we could write a macro which optimizes the
   * implementation. But what if those coefficients only become known at runtime? Thankfully, this
   * is where runtime staging comes in.
   * 
   * Let's try to find a fast way to implement the calculation of the polynomial, `3.2x³ - x + 2.8`.
   * How does this look?
   * ```
   * def calc(x: Double) =
   *   var a = 0.0
   *   var p = 1.0
   *   a = a + p*2.8  // a = 2.8
   *   p = p*x        // p = x
   *   a = a + p*(-1) // a = 2.8 - x
   *   p = p*x        // p = x²
   *   p = p*x        // p = x³
   *   a = a + p*3.2  // a = 2.8 - x + 3.2x³
   *   a
   * ```
   *
   * Check that the calculation above is correct. It should be clear that any polynomial (with
   * terms of positive powers) can be written as a sequence of just two operations: one which
   * increases the power of x for the "current" term, and one which multiplies that x^n by a
   * coefficient parameter. The initial values are always the same, and we always return the
   * accumulator at the end.
   *
   * 1. Write an enumeration called `Op` representing these two operations
   * 2. Write an extension method on `Polynomial` called `ops` which generates a sequence of `Op`s
   *    for the polynomial, based on the values in its `terms` `Map`.
   * 3. Write a runtime interpreter for `ops` to check that it gives the same answer as the `apply`
   *    method. Its signature should be similar to:
   *    ```
   *    @tailrec def calculate(todo: List[Op], a: Double, p: Double, x: Double): Double
   *    ```
   */

object exercise2
  /**
   * EXERCISE 2
   *
   * A while ago we saw how we could convert a value to an expression by wrapping it in `Expr(...)`
   * if there's a contextual `ToExpr` type-class instance available. The reverse operation is to be
   * able to call `.value` or `.valueOrAbort` on an existing `Expr[T]` instance and get an
   * `Option[T]` (or a `T`) while the macro is running. This requires a given `FromExpr[T]`
   * instance.
   *
   * Implement a `FromExpr[Op]` instance by implementing the method signature,
   * ```
   *   def unapply(value: Expr[Op])(using Quotes): Option[Op]
   * ```
   * in a given `FromExpr`, by pattern matching on quotes.
   */

object exercise3
  /**
   * EXERCISE 3
   *
   * Write a macro implementation for the method,
   * ```
   * inline def calc(inline ops: List[Op]): Double = ${calcMacro('ops)}
   * ```
   * This macro will compile a method to evaluate the polynomial from the `Op`s sequence, though
   * for now, it will still only be able to work on input which is statically known.
   *
   * In the body, first call `valueOrAbort` on the `Expr[List[Op]]`. This will need the given
   * `FromExpr[Op]` we created in Exercise 2 to be in scope.
   *
   * Now we need to implement the macro to using quotes and splices. This macro will not construct
   * an expression like the previous examples: it will construct a function object
   * (`Double => Double`) once, which we can call many times.
   *
   * This will be our most complex quotes example! Start with the code below:
   * ```
   * '{
   *   (x: Double) =>
   *     var p: Double = 1.0
   *     var a: Double = 0.0
   *     ${
   *       ops.foldLeft('{()}):
   *         case (instrs, Op.Raise)  => '{ $insts; p = p*x }
   *     }
   *     a
   * }
   * ```
   * 
   * The code is incomplete, and needs one more case to be added. Try to understand what is being
   * constructed.
   *  - it as an expression, so it's surrounded by quotes
   *  - we are constructing a function, which takes a parameter, `(x: Double) =>`
   *  - we need two mutable `var`s to hold our state while we compute the result with side-effecting
   *    operations
   *  - we construct those side-effecting operations _within_ a splice, with a fold over the `ops`
   *    sequence
   *  - the fold starts with an empty statement (`()`), and for each operation constructs a new
   *    expression from the previous instructions followed by the instruction for the current `Op`.
   *
   * Carefully check the phase consistency of the variables, `x`, `p`, `a` and `instrs`,
   * particularly where those terms cross more than one splice/quote boundary.
   *
   * Add the second case to handle `Op.Acc(k)`, where `k` is the coefficient of the next term to
   * add.
   *
   * We should now be able to call `calc` with an explicit list of `Op`s, and it will compile a
   * function we can call at runtime. The method will run as fast as a hardcoded polynomial, thanks
   * to the macro, but it ONLY works if the coefficients are known at compile time.
   */

object exercise4
  /**
   * EXERCISE 4
   *
   * We will take this one step further with multistage programming. Let's define a method called
   * `runtimeCalc` which will return a `Double => Double` on an extension method of `Polynomial`.
   * This should not be an inline method.
   *
   * Import `scala.staging` if it is not already in scope. Now, copy the macro body from Exercise 3
   * into this method, but make the following changes:
   *
   *  1. Wrap the entire quoted body of `runtimeCalc` in `staging.run { ... }`.
   *  2. Replace `ops.valueOrAbort` with a direct reference to the `ops` value on the polynomial.
   *     value.
   *  3. Provide a `given staging.Compiler` instance at the top-level for `staging.run`:
   *     `given Compiler = Compiler(getClass.getClassLoader)`
   *
   * This is an ordinary method, which will be invoked at runtime, and it has access to normal
   * runtime values, like the polynomial. So we no longer need to work with an `Expr[List[Op]]`
   * because we have an actual `List[Op]` instance. The rest of the macro should be unchanged.
   *
   * In `playground.scala`, construct a couple of `Polynomial`s and compare the results of
   * calculating the value for a given `x` using the original `apply` method, and the new
   * `runtimeCalc` method.
   *
   * Remember, calling `runtimeCalc` compiles a new function every time it is invoked, which is
   * slow. But that function can then be called many times for different values of `x` without
   * recompilation!
   */
  
object exercise5:
  /**
   * EXERCISE 5
   *
   * Add an extension method on `Polynomial` which calculates the numerical integral of a polynomial
   * between a range of values for a number of steps, with the signature:
   * ```
   * def integrate(start: Double, end: Double, steps: Int): Double
   * ```
   * The implementation will need to call `runtimeCalc` to get a `calc` function, then apply it to each
   * of the evenly-spaced `x` values between `start` and `end`. These results should be summed,
   * and the result multiplied by `end - start` and divided by `steps`.
   * 
   * Write a new polynomial and integrate it between -10 and 10 with 10000000 (ten million) steps.
   * How long does it take to complete? Switch back to using the standard `apply` method on
   * `Polynomial` in place of the compiled method. How long does it take now?
   */