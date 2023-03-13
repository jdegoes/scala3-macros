package inlineMethods
/**
 * INLINE METHODS
 *
 * Scala 3 introduces the `inline` keyword which can "copy/paste" a method's implementation to the
 * call-site. Technically speaking, code inlining is a form of macros: indeed, the original "macro"
 * in C was #define, whose power subsumes inlining (every good C macro is a Scala 3 inline).
 * 
 * Code inlining is not only the foundation of more powerful macros in Scala 3, but it is very 
 * powerful in its own right, and can be used to improve performance and reduce code duplication in 
 * cases that previously could not afford the cost of abstraction.
 * 
 * In this section, you will learn about the `inline` keyword and the guarantees it provides.
 */
object exercise1:
  /**
   * EXERCISE 1 (10 mins)
   *
   * Run the main method of this class (in `sbt` use `runMain exercise1`), and see how `raise`
   * appears at the top of the stack trace. Make `raise` an `inline` method so the stack trace does
   * not contain `raise`.
   */
  def raise(): String = throw new Exception()
  def defer(): Unit = println(raise())

  def main(args: Array[String]): Unit = defer()

object exercise2:
  /**
   * EXERCISE 2 (20 mins)
   *
   * Use `javap` on the class `exercise2$` to compare the bytecode of `printMessage` and
   * `inlinePrintMessage`. You can do this in `sbt` with `javap inlineMethods.exercise2\$` or by running
   * `javap -c` directly in the `target` directory after compiling.
   */
   def message(): String = "Hello world!"
   inline def inlineMessage(): String = "Hello world!"

   def printMessage(): Unit = println(message())
   def inlinePrintMessage(): Unit = println(inlineMessage())
  
object exercise3a:
  /**
   * EXERCISE 3a (20 mins)
   *
   * "Constant folding" is a feature in Scala which has always existed, and performs arithmetic
   * operations on numeric types and concatenation on `String`s at compile-time. See how `a`, `b`,
   * `c`, and `d` are represented in bytecode. Calculate the actual value of `d`, and understand
   * the bytecode representation.
   */
  final val a = 6*7
  final val b = 10
  final val c = a*b
  final val d = (4*4 + 2*a/7)*13 - (5 - 10)*11 - c

object exercise3b:
 /**
  * EXERCISE 3b
  * 
  * Experiment with removing `final` from some or all of the values, `a`, `b`, `c` and `d`. What
  * happens if you make them inline? What are the exact types of each value?
  *
  * (Tip: you can get the compiler to tell you an expression's type by forcing a type error!)
  */
  val a = 6*7
  val b = 10
  val c = a*b
  val d = (4*4 + 2*a/7)*13 - (5 - 10)*11 - c

object exercise4a:
  /**
   * EXERCISE 4a (30 mins)
   * 
   * Examine the implementation of `raise`. Assuming n > 0, what does it calculate (very
   * inefficiently)?
   */
  def raise(x: Int): Int = if x == 0 then 1 else raise(x - 1) + raise(x - 1)

object exercise4b:
  /**
   * EXERCISE 4b
   *
   * The method `time` will give a rough estimate of the average time taken to execute `fn` `n`
   * times. Experiment with values of `n` and the `x` parameter of `raise` until running the main
   * method of this object gives reasonably stable output. Check how the running time varies as
   * `x` is increased or decreased.
   */
  inline def raise(x: Int): Int = if x == 0 then 1 else raise(x - 1) + raise(x - 1)
  
  def time(n: Int)(fn: => Unit): Long =
    val t0 = System.nanoTime
    for i <- 1 to n do fn
    (System.nanoTime - t0)/n
  
  def main(args: Array[String]): Unit = println(exercise4b.time(100)(exercise4b.raise(7)))

object exercise4c:
  /**
   * EXERCISE 4c
   *
   * What happens to the running time if we change `raise` into an `inline` method? How does it vary
   * when `x` is changed? How is `e` represented in bytecode? Note that you may need to set `sbt` to
   * use `javap` in verbose mode with:
   *
   * sbt> set javaOpts := List("-c", "-v")
   *
   * N.B. 2¹² = 4096
   */
  def raise(x: Int): Int = if x == 0 then 1 else raise(x - 1) + raise(x - 1)
  final val e: Int = raise(12)

/**
 * TRANSPARENT INLINE METHODS
 *
 * An `inline` method can additionally be marked as `transparent`, i.e. `transparent inline def`.
 * These methods will be "interpreted" at compile time, where possible, and the return type of the
 * transparent method may be refined, if it can be proven to have a more a more specialised type
 * after inlining.
 *
 * They are called "transparent" because the compiler can "see inside" the implementation at the
 * call-site.
 */

object exercise5:
  /**
   * EXERCISE 5 (10 mins)
   *
   * Using the previous `inline` implementation of `raise`, make it `transparent` and give `result`
   * a precise, singleton literal return type. Check what happens if `transparent` is removed.
   */
  inline def raise(x: Int): Int = if x == 0 then 1 else raise(x - 1) + raise(x - 1)
  val result: Int = raise(10)

object exercise6a:
  /**
   * EXERCISE 6a (30 mins)
   *
   * The extension method `pairs` will transform an `Iterable[T]` into an `Iterable[(T, T)]`, as
   * shown in the `numberPairs` example. Change the type of `numberPairs` to `List[(Int, Int)]` and
   * confirm that it is a type error.
   */
  extension [T](xs: Iterable[T])
    def pairs: Iterable[(T, T)] = xs.map { x => (x, x) }
  
  val numbers: List[Int] = List(1, 2, 3)
  val numberPairs: Iterable[(Int, Int)] = numbers.pairs

object exercise6b:
  /**
   * EXERCISE 6b
   *
   * Using a `BuildFrom` contextual value, write a generic `genPairs` method which returns a value
   * of the same collection type: first remove the `???` and uncomment the body of `genPairs`, then
   * choose appropriate type parameters for `BuildFrom` so that it compiles.
   *
   * Check that the return type of `numberPairs` can be refined from `Iterable` to `List`.
   */
  import scala.collection.BuildFrom
  
  extension [Coll[T1] <: Iterable[T1], T](xs: Coll[T])
    def genPairs(using bf: BuildFrom[?, ?, Coll[(T, T)]]): Coll[(T, T)] = ??? /*
      val builder = bf.newBuilder(xs)
      xs.foreach { x => builder += (x -> x) }
      builder.result()
    */
  
  val numbers: List[Int] = List(1, 2, 3)
  val numberPairs: Iterable[(Int, Int)] = numbers.genPairs

object exercise6c: 
  /**
   * EXERCISE 6c
   *
   * That was hard work! Go back to the definition of `pairs` we started with. Change the return
   * type of `numberPairs3` to `List[(Int, Int)]`. This should be a compile error.
   *
   * Now, add two "magic" keywords in front of `def easyPairs` to fix the compile error!
   *
   * If you have time, take a look at the bytecode for `numberPairs` in each implementation
   */
  extension [T](xs: Iterable[T])
    def pairs: Iterable[(T, T)] = xs.map { x => (x, x) }
  
  val numbers: List[Int] = List(1, 2, 3)
  val numberPairs: Iterable[(Int, Int)] = numbers.pairs

object example7a:
  /**
   * EXERCISE 7a (30 mins)
   *
   * The `rollDice` method will return a `String` if your two dice score 12. We will start with
   * fixed dice which always show a `6` and a `5`.
   *
   * Try refining the types of `rollDice` and `outcome` to a union of two singleton literal types.
   * If you write the strings correctly, the code should still compile.
   *
   * Now try removing one of the strings from the union type. (Remove the one which doesn't make
   * sense based on the numbers!) Does the code still compile?
   */
  val fixedDice: Int = 6 + 5
  def rollDice(): String = if fixedDice == 12 then "Winner!" else "Loser!"
  
  val outcome: String = rollDice()

object exercise7b:
  /**
   * EXERCISE 7b
   *
   * Now, uncomment `val outcome2` and experiment making the `rollDice2` method `inline` or
   * `transparent inline`. It looks like the compiler has all the information it needs to deduce the
   * precise return type is `"Loser!"`, but it needs a bit more cooperation from us!
   *
   * The `inline` keyword can also be used in the body of `rollDice2` to guarantee that inlining is
   * happening when we expect it. It can help to produce more helpful error messages.
   *
   * Add the `inline` keyword in front of `if`, and check the error message. Tweak the definition of
   * `fixedDice2` so that `outcome2` returns the singleton type, `"Loser!"`. */
  
  val fixedDice2 = 6 + 5
  def rollDice2(): "Loser!" | "Winner!" = if fixedDice2 == 12 then "Winner!" else "Loser!"
  //val outcome2: "Loser!" = rollDice2()

object exercise7c:
  /**
   * EXERCISE 7c
   *
   * Using the previous definition of `rollDice`, uncomment and change the definition of `rollDice3`
   * to replace the `inline if` with an `inline match`, without changing the behavior. (Note that
   * the scrutinee expression is sandwiched between `inline` and `match`.)
   */
  inline val fixedDice3 = 6 + 5
  
  // transparent inline def rollDice3(): "Loser!" | "Winner!" =
  //   inline if fixedDice3 == 12 then "Winner!" else "Loser!"
  // val outcome3: "Loser!" = rollDice3()

object exercise7d:
  /**
   * EXERCISE 7d
   *
   * Now modify `rollDice4` to take two parameters representing the numbers scored on the two dice.
   *
   * Bonus enhancement: Make the parameters precise union types!
   *
   * Uncomment `outcome4` to check it compiles. What happens if you replace one of the `6`s with
   * `((math.random*6).toInt + 1)`?
   */
  transparent inline def rollDice4(): "Loser!" | "Winner!" =
    inline if ??? == 12 then "Winner!" else "Loser!"
  
  // val outcome4: "Winner!" = rollDice4(6, 6)

/**
 * BRANCH ELIMINATION
 *
 * When Scala reduces an inline `if` or `match` in a `transparent inline def`, the branches which
 * are not followed will be eliminated, i.e. erased from the compiled code. This means that code
 * appearing in these branches which would have compile-time side-effects will also be deleted.
 *
 * What are compile-time side-effects, though? We will see more later, but a useful example is
 * emitting a compile error.
 */

object exercise8a:
  /**
   * EXERCISE 8a (20 mins)
   *
   * Uncomment the code below and check the compiler error and position. Change the text and check
   * the compile error again.
   *
   * The `error` message is *always* failing to compile because it is never eliminated. Make the
   * method `inline`, and see where the error moves to. Change the parameter passed to
   * `unitInterval` to something else to make the compile error disappear.
   *
   * `error` must be used in an `inline` context to be useful!
   */
  import scala.compiletime.error
  // def unitInterval(value: Double): Double =
  //   if value > 1.0 || value < 0.0 then error("number is not guaranteed to be in the unit interval!")
  //   value
  
  // val n = unitInterval(1.5)

object exercise8b: 
  /**
   * EXERCISE 8b
   *
   * We can provide more useful error messages with `codeOf`, which "captures" the code (at
   * compile-time) as a `String`.
   *
   * Uncomment `val n` and see what the error is. Note that we see `1.5d` instead of `1.5`. The
   * captured code is a reserialization of the AST; not the code itself. */
  import scala.compiletime.{error, codeOf}

  inline def unitInterval(value: Double): Double =
    if value > 1.0 || value < 0.0
    then error("The value "+codeOf(value)+" is not guaranteed to be in the unit interval!")
   
    value
  
  //val n = unitInterval(1.5)

object exercise8c: 
  /**
   * EXERCISE 8c
   *
   * Try calling `unitInterval` with a method such as `math.random`. What error message do you see?
   *
   * Why is this? Note the call-site of `codeOf(value)` in the body of `unitInterval`. At this
   * position, the compiler cannot "see" the code of `value` from whichever call-site `unitInterval`
   * was called from.
   *
   * What happens if we make the parameter itself `inline`?
   *
   * Does `unitInterval(math.random)` compile now? If not, why not, since it always returns a value
   * between 0.0 and 1.0?
   */
  import scala.compiletime.{error, codeOf}
  
  inline def unitInterval(value: Double): Double =
    if value > 1.0 || value < 0.0
    then error("The value "+codeOf(value)+" is not guaranteed to be in the unit interval!")
   
    value
  
  val m = unitInterval(0.5)

object exercise8d:
  /**
   * EXERCISE 8d
   *
   * We can only compare `value` to `0.0` and `1.0` if its value is known statically.
   * Call the `compiletime.requireConst` method with `value` at the start of `unitInterval` and
   * see how it changes its behavior.
   */
  
  import scala.compiletime.*
  
  inline def unitInterval(inline value: Double): Double =
    if value > 1.0 || value < 0.0
    then error("The value "+codeOf(value)+" is not guaranteed to be in the unit interval!")
   
    value


object exercise9a:
  /**
   * EXERCISE 9a (15 mins)
   *
   * A less obvious form of conditional branching in Scala arises from contextual values
   * (implicits) which may be chosen statically, based on a type (which may be inferred).
   *
   * Read and understand the code below. Note the "shorthand" style for the two provided givens.
   *
   * Uncomment the last line, passing a `Double` to the `accept` method, and note the error message.
   * Implement a special `given` instance of type `Permitted[Double]` whose implementation calls the
   * `scala.compiletime.error` method with the custom error message, "No doubles, please!". Don't
   * forget that `error` can only be called in an `inline` context.
   */
  trait Permitted[T]()
  
  def accept[T](value: T)(using Permitted[T]): Unit = println(s"$value is allowed")
  
  given Permitted[Int]()
  given Permitted[String]()

  accept(1)
  accept("two")
  //accept(3.0)

object exercise9b:
  /**
   * EXERCISE 9b
   *
   * Although implicit *conversions* are soft-deprecated in Scala 3, we can still use them to
   * produce some custom type-error messages.
   *
   * Provide an `inline given` instance of `Conversion[String, Int]` which causes the `val x` line
   * to report a custom compile error.
   */
   import language.implicitConversions
   import scala.compiletime.error

   given Conversion[String, Int] = ???
   
   val x: Int = "1"

object exercise10a:
  /**
   * EXERCISE 10a (30 mins)
   *
   * Read and understand the implementation of `Show`, its instances and extension method.
   *
   * Uncomment and fix the implementation of `log` so that the `main` method runs without error.
   */

  import scala.compiletime.*
  
  trait Show[T]:
    def show(value: T): String
  
  object Show:
    extension [T](value: T)(using show: Show[T]) def show: String = show.show(value)
    given Show[Int] = _.toString
    given Show[Double] = java.text.DecimalFormat("#.###").format(_).nn
  
  def log[T](value: T): Unit = ??? // println(value.show)

  def main(args: Array[String]): Unit = log(math.Pi)

object exercise10b:
  /**
   * EXERCISE 10b
   *
   * Look at the typeclasses `Debug` and the instances which exist for `Int` and `Char`. Compare it
   * to `Show`, above.
   * 
   * Uncomment the implementation of `log` and fix the compile error by adding a `using` parameter
   * (an implicit parameter) or a context bound on `T`. Comment out the calls to `log` which can't
   * possibly work.
   */
  trait Debug[T]:
    def debug(value: T): String
  
  object Debug:
    extension [T](value: T)(using debug: Debug[T]) def debug: String = debug.debug(value)
    given Debug[Int] = int => s"Int($int)"
    given Debug[Char] = ch => s"'$ch'"
  
  def log[T](value: T): Unit = ??? //println(value.debug)
  
  def main(args: Array[String]): Unit =
    log(math.Pi)
    log(42)
    log('x')
    log("Hello world")

object exercise10c:
  /**
   * EXERCISE 10c
   *
   * Using the same definition of `Debug` (imported from the previous exercise), try an alternative
   * method of making the body of `log` compile. Instead of adding a `using` parameter or context
   * bound, make `log` `inline`, and uncomment its body. Have we fixed it yet?
   *
   * Check the contents of the `scala.compiletime` package (check tab completion in Metals or the
   * REPL, read the Scaladocs, or look at the compiler source code in `github.com:lampepfl/dotty`)
   * to see if there's an alternative to `summon` which might work. Try it as a drop-in replacement.
   *
   * Finally, check what happens when you remove the `inline` modifier again.
   */
  import exercise10b.Debug
  
  def log[T](value: T): Unit = ??? //println(summon[Debug[T]].debug(value))
  
  def main(args: Array[String]): Unit =
    log(42)
    log('x')

object exercise10d:
  /**
   * EXERCISE 10d
   *
   * There is a generalized version of `summonInline` called `summonFrom` which allows us to choose
   * a preference of different typeclass instances, based on availability -- with a fallback option
   * if no typeclass is available.
   *
   * Look at the implementation of `log`, below. The code which looks like a partial function inside
   * `summonFrom` is interpreted by a macro (and like `summonInline`, it must be called in an inline
   * context). This example should be read: "if we find a contextual instance of `Debug[T]`, use the
   * code `debug.debug(value)`, otherwise use `value.toString`.
   *
   * Try running the `main` method, and see how each line of output is produced.
   *
   * Now, add an additional `case` to the `log` to use a `Show[T]` typeclass, if one is available.
   * Which line of `main` output changes, and why?
   */
  import exercise10a.Show
  import exercise10b.Debug
  import scala.compiletime.summonFrom

  inline def log[T](value: T): Unit = println:
    summonFrom:
      case debug: Debug[T] => debug.debug(value)
      case _               => value.toString

  def main(args: Array[String]): Unit =
    log(math.Pi)
    log(42)
    log('x')
    log("Hello world")

object exercise10e:
  /**
   * EXERCISE 10e
   *
   * Here is an implementation of `log` which uses the extension methods for `Show` and `Debug`. But
   * it doesn't compile -- uncomment the RHS of each case to see why.
   *
   * `summonFrom` can find `Debug[T]` and `Show[T]` instances, but it doesn't make the contextual on
   * the RHS of each case, by default. However, Scala has new syntax to achieve this. Try replacing
   * the bound name of the typeclass instance, `show: Show[T]` with `given Show[T]` (with no name
   * binding). Do the same for `Debug[T]`. Does it compile?
   */
  import exercise10a.Show
  import exercise10b.Debug
  import scala.compiletime.*

  inline def log[T](value: T): Unit = println:
    summonFrom:
      case debug: Debug[T] => ??? // value.debug
      case show: Show[T]   => ??? // value.show
      case _               => value.toString

object exercise11:
  /**
   * EXERCISE 11 (10 mins)
   *
   * Read and understand the simple inline method below, and definitions of `a` and `b`. We can see
   * that the compiler theoretically has all the information it would need to know `a`'s and `b`'s
   * types precisely. Change their definitions to more precise types, and check the compile errors.
   *
   * The `parse` method definition needs two additional modifier keywords to be added. With these,
   * it should compile.
   *
   * Bonus: Can you change the definition of `parse` slightly to persuade the compiler to make `a`'s
   * body conform to the precise singleton type, `Some["something"]` (not just `Some[String]`)?
   */
  def parse(value: String): Option[String] =  if value == "none" then None else Some(value)
  
  val a: Option[String] = parse("something")
  val b: Option[String] = parse("none")