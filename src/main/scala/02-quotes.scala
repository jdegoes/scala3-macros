package quotesAndSplices
/**
 * QUOTES AND SPLICES
 *
 * Scala 3 iterates on 2.x macros, introducing a new and improved macro system, which is based on 
 * quotation and splicing. Quotes and splicing are a type-safe, feature-rich way to do code 
 * generation at compile-time.
 */

import scala.quoted.*

object exercise1:
  /**
   * EXERCISE 1
   *
   * This is a simple example of a (boring) Scala 3 macro. The syntax may look unfamiliar!
   *
   * Macro methods come in pairs: a macro invocation method and a macro implementation method. In
   * this example, `sayHello()` is the macro invocation, and `sayHelloMacro` is the macro
   * implementation. The macro invocation method will always reference the macro implementation
   * method. They don't have to be in the same file.
   *
   * We will overlook macro method parameters for now. There are several features of both methods to
   * be aware of:
   *
   * Macro Invocation methods:
   *  1. The invocation is always `inline`
   *  2. The entire RHS of the invocation is always a `${}` splice (but may be more complex)
   *  3. The invocation is a normal inline method -- as "normal" as other inline methods!
   *
   * Macro Implementation methods:
   *  4. The implementation will take a contextual `Quotes` parameter
   *  5. The return type of the implementation will be an `Expr` of the invocation's return type
   *  6. The implementation method is a normal (not even inline) method -- you can call it if you
   *     have a `Quotes` instance.
   *
   * The only new syntax is the usage of `'{...}` and `${...}`, called "quotes" and "splices"
   * respectively, which is going to look like magic until we become familiar with it.
   *
   * Some let's get more familiar and make it seem a bit less magic. *Pretend* that we are writing
   * a text-based source generator:
   * 
   * First Wrap remove the `inline` modifier from `sayHello` and wrap the entire `sayHello` method
   * definition in `s""` quotes. It should look just like we are substituting `sayHelloMacro` into
   * the string.
   *
   * Now change the return type of `sayHelloMacro` to `String`, delete the `Quotes` parameter and
   * replace the `'{` opening quote and `}` closing quote with string triple-quotes. It should look
   * like a method which returns a string containing some code.
   *
   * Conceptually, this is how Scala 3's quotes and splices work. But we will see that Scala 3
   * provides much more safety than Strings ever could!
   */
  
  def sayHelloMacro(using Quotes): Expr[Unit] = '{println("Hello world")}
  inline def sayHello(): Unit = ${sayHelloMacro}

object exercise2:
  /**
   * EXERCISE 2
   *
   * Now let's do the same thing in reverse. We will start with a naive string-based implementation
   * of a macro, and turn it into a real macro which writes code to pick a random Double.
   *
   * The transformed code should include all of the following:
   *
   *  1. `Quotes`
   *  2. `Expr[Double]`
   *  3. `${...}`
   *  4. `'{...}`
   *  5. `inline`
   */
  
  def randomMacro: String = "math.random"
  s"def random: Double = ${randomMacro}"


object exercise3:
  /**
   * EXERCISE 3
   * 
   * Implement the main method which calls `exercise2.random` and `exercise1.sayHello()`. Does it
   * work?
   * 
   * Apart from the restriction described in the compile error, Scala is happy to have macros
   * defined and expanded in the same run of the compiler -- we no longer need to split them into
   * separate compilation steps!
   *
   * Take a reasonable step to get the main method to compile in a single compilation run.
   */
   def main(args: Array[String]): Unit = ()

object exercise4:
  /**
   * EXERCISE 4
   *
   * Take the untransformed example from Exercise 2, and change the macro implementation slightly:
   * We will now calculate `math.random` and then put the result into a string. Consider how the
   * contents of the string differs from the version in Exercise 2, and *when* `math.random` will
   * be invoked.
   *
   * Now uncomment `randomMacro` and `random`, and check the compile error.
   *
   * In the first version of `randomMacro`, the macro produced the code `math.random`. In the second
   * version, the macro *executes* `math.random` and tries to produce code containing the `Double`
   * value generated at compile-time.
   *
   * But that Double value exists only as 64 bits of data in memory in the JVM at *compiletime*,
   * whereas we need to substitue *code representing that value* into the quotes. Specifically, the
   * compiler needs an `Expr[Double]`. Thankfully that's easy to create by wrapping the `Double`
   * with `Expr(...)`. Apply this, and try again.
   *
   * The compiler should compile the code and give some useful advice on how it can be simplified.
   * Follow this advice, and simplify the macro implementation.
   */
  def fakeRandomMacro: String =
    val rnd = math.random
    "$rnd"

//   def randomMacro(using Quotes): Expr[Double] =
//     val rnd: Double = math.random
//     '{$rnd}

//   inline def random: Double = ${randomMacro}"

object exercise5:
  /**
   * EXERCISE 5
   *
   * Macros can also take parameters. These require changes to the macro implementation and the
   * macro invocation methods. We will change `random` to take an integer parameter, and it should
   * return a random value between 1 and the `bound` value (inclusive).
   *
   * Take a look at the updated `random` and `randomMacro` methods. See how `'bound` is passed into
   * `${randomMacro(...)}` quoted. (N.B. `'bound` is the same as `'{bound}`. Note the type of the
   * `bound` parameter in each method: `Int` and `Expr[Int]`. `'bound` is a quoted reference --
   * think of it like a string containing the code which points to the parameter we need to refer
   * to: `randomMacro` can use that code, as an `Expr[Int]` (and not an `Int` itself) when it
   * produces its result. For now, the implementation always returns `0`.
   *
   * Experiment with passing `bound` instead of `'bound` to `randomMacro`, and changing the type
   * of `bound` in `randomMacro` from `Expr[Int]` to `Int`. Understand why we can't just change
   * both to make the types easier, and read the compiler's message about the restrictions.
   *
   * Understand how the Phase Consistency Principle applies to `bound`.
   *
   * Experiment with adding more parameters, such as a literal value or a String.
   */

  inline def random(bound: Int): Int = ${randomMacro('bound)}

  def randomMacro(bound: Expr[Int])(using Quotes): Expr[Int] = '{0}

object exercise6:
  /**
   * EXERCISE 6
   *
   * Uncomment the new implementation of `randomMacro`, and check the compile error, which is a
   * normal type error. If `'{...}` (quoting) can take us from an `Int` to an `Expr[Int]`, work
   * out (by intuition!) how we can go from an `Expr[Int]` to an `Int` so that we can use it in
   * the quoted macro implementation.
   *
   * Understand how each other part of the expression can be used in the implementation: `math`,
   * `.random`, `*`, `.toInt`, `+` and `1`. Try substituting other expressions into quoted block.
   *
   * Finally, try adding a `println("Hello world!")` to the body of `randomMacro`. What difference
   * does it make if you put it inside the quoted block, or before the quoted block?
   */
  
  inline def random(bound: Int): Int = ${randomMacro('bound)}
  
  def randomMacro(bound: Expr[Int])(using Quotes): Expr[Int] =
    ??? // '{(math.random*bound).toInt + 1}

/** In general, code that is not inside any `'{...}` or `${...}` blocks will be compiled at the time
 *  we call "compile-time" and will be run at the time we call "runtime". No big surprise! If we
 *  call compile-time T, then the runtime will be T+1.
 *
 *  Code which appears inside `${...}` was compiled at T-1 and is run at T.
 *
 *  Code which appears inside `'{...}` has not yet been compiled, but can be compiled at T+1 and run
 *  at T+2.
 *
 *  Here, "compilation" means the final production of runnable bytecode. One of the most elegant
 *  features of Scala 3 Macros is code inside quotes will still be fully typechecked inside quotes
 *  in whichever phase it is defined in.
 *
 *
 *  Quotes and splices were described as "magic" when they were introduced. But if we think about
 *  them in the right way, the "magic" is quite limited: code can only be quoted in the presence of
 *  an instance of `Quotes`, and a splice block automatically provides a contextual (implicit)
 *  `Quotes` instance. This means that quoting is possible inside a splice, or inside a macro
 *  implementation method which has a `using Quotes` parameter. Such a method can be invoked inside
 *  a splice. An inline method may be implemented with just such a splice, called a top-level
 *  splice. Top-level splices remain the most magic aspect of quotes and splices.
 */

object exercise7:
  /**
   * EXERCISE 7
   * Look at the implementation of `describe` below. The macro does not produce any interesting
   * code, but it does print the expression passed into it. (Note that this is an `Expr[Int]`, not
   * an actual `Int`.)
   *
   * In `playground.scala`, write a method which invokes `exercise7.describe`, passing in an integer.
   * What is printed when it is compiled?
   *
   * The `toString` method of `Expr`s treats them as somewhat opaque, but the `show` method will
   * construct a string representing the expression. Change `describeMacro` to print `expr.show`
   * instead.
   *
   * But it's not very helpful: it prints the name of a synthetic reference. This is because, at the
   * call-site, that is all `expr` is -- an integer value to be accessed by through a reference.
   *
   * Try the following steps, and at each stage, try to understand how it changes the output at
   * compile-time.
   * 1. Make `describe`'s parameter, `expr` `inline`.
   * 2. Change the parameter passed to `exercise7.describe` from an integer literal to
   *    `Int.MaxValue`.
   * 3. Change the parameter passed to `exercise7.describe` to `3*4 + 30`.
   * 4. Change the parameter to `(math.random*10).toInt`.
   */
  inline def describe(expr: Int): Unit = ${describeMacro('expr)}
  def describeMacro(expr: Expr[Int])(using Quotes): Expr[Unit] =
    println(expr)
    '{()}

object exercise8:
  /**
   * EXERCISE 8
   * 
   * This exercise is a recap of pattern matching in Scala. Remember that the case of the first
   * letter may be significant and backticks may be required.
   *
   * Rewrite the patterns in the partial function, without changing their RHSs, so that it prints
   * the names of the values from the list in the order they appear. Use all the named values in 
   * the patterns.
   */
  object Three
  object four

  object Five:
    def unapply(n: Int): Boolean = n == 5

  object six:
    def unapply(n: Int): Boolean = n == 6

  object Seven:
    def unapply(n: Int): Option[Seven.type] = if n == 7 then Some(Seven) else None

  val Eight: Int = 8

  List[Any](1, 2, Three, four, 5, 6, 7, Eight).foreach:
    case 1 => println("1")
    case 2 => println("2")
    case 3 => println("Three")
    case 4 => println("four")
    case 5 => println("Five")
    case 6 => println("six")
    case 7 => println("7")
    case 8 => println("Eight")
   


object exercise9:
  /**
   * EXERCISE 9
   *
   * Study the implementation of `describeMacro`. Call it in `playground.scala`, and check the
   * `Didn't match` message appears.
   *
   * Uncomment the commented-out `case`, and fix the compile error by adding type ascriptions and
   * parentheses.
   *
   * Spend a while trying to understand the match pattern. It's best to think of it the code you
   * would write (with some extra type ascriptions) if you already had `a` and `b` and wanted to
   * produce `expr`.
   * 
   * Modify the call to `describe` (in `playground.scala`) to try to get it to print `Sum of integers`.
   * Don't forget that constant folding happens before the macro captures the expression, so you
   * will need to write an expression that retains its `+`!
   *
   * Try including `a.show` and `b.show` in the `println` output, and see how they vary with
   * different input values.
   *
   * Try adding additional cases which match different expressions.
   */
  inline def describe(inline expr: Int): Unit = ${describeMacro('expr)}
  def describeMacro(expr: Expr[Int])(using Quotes): Expr[Unit] =
    expr match
      //case '{ $a + $b } => println("Sum of integers")
      case _ => println("Didn't match: "+expr.show)
    
    '{()}

object exercise10:
  /**
   * EXERCISE 10
   *
   * `optimizeMacro` now matches on a more complex expression: the difference of two products. If
   * `a == b` and `c == d` then this would be the difference of two squares and could be optimized
   * into `(a + c)*(a - c)`.
   *
   * Equality for expressions is more complex than a simple `a == b` check, since two expressions
   * lifted from different parts of the code will always be different, even if they _look_ the same.
   * We can use the `matches` method in place of `==` to do a more reliable comparison.
   *
   * Implement the optimized RHS for the matched case using `a` and `c`, and provide a default
   * return value if the case does not match.
   */
  inline def optimize(inline expr: Int): Int = ${optimizeMacro('expr)}
  
  def optimizeMacro(expr: Expr[Int])(using Quotes): Expr[Int] =
    expr match
      case '{ ($a: Int)*($b: Int) - ($c: Int)*($d: Int) } => ???
      case _                                              => ???

object exercise11:
  /**
   * EXERCISE 11
   *
   * We commonly need to match on lambdas. These can take several forms, and can introduce their
   * own definitions, so this requires some syntactic gymnastics!
   *
   * The `lengthFrom` macro pattern matches against a lambda of `String => String` and constructs
   * a lambda of `String => Int` which returns the length (`Int`) of the `String` after applying
   * the original lambda, but without calculating the intermediate string's value.
   *
   * Check the implementation of the pattern which matches calls to `replace` (which does not change
   * the string's length). Write a correct RHS for the second case. Add a third case which matches
   * the two-parameter variant of `substring`.
   *
   * Try calling the macro in `playground.scala`. Does it make any difference whether you call,
   * `_.substring(7)` or `str => str.substring(7)` or `str substring 7`?
   *
   * Bonus: Can you match against the lambda, `_.take(n)` or `_.drop(m)`?
   *
   * Take a look at github:lampepfl/dotty/tests/pos-macros/quotedPatterns.scala for some more
   * examples of more complex pattern matches against lambdas.
   */
  inline def calcLength(inline fn: String => String): String => Int = ${calcLengthMacro('fn)}

  def calcLengthMacro(expr: Expr[String => String])(using Quotes): Expr[String => Int] =
    expr match
      case '{ (str: String) => str.replace($_ : Char, $_ : Char): String } =>
        '{ (str: String) => str.length }
      case '{ (str: String) => str.substring($n): String } =>
        '{ (str: String) => ??? }

/**
 * An important point to understand about pattern matching (in general) is that the pattern (between
 * `case` and `=>`) introduces new identifiers which become available on the RHS, which is a new
 * scope. Most commonly this applies to terms (i.e. values), but we will see later that it applies
 * equally to types.
 */

object exercise12:
  /**
   * EXERCISE 12
   *
   * All the macros we have seen so far have concrete types, but it's common to need to have
   * macros with type parameters. Here is a very simple example, which needs some changes:
   *
   * Uncomment the `println` and fix the compiler error by adding a new `using` parameter, or
   * context bound. What output do you get (at compile-time) when calling the macro in
   * `playground.scala`?
   *
   * Remember the invocation of `typeNameMacro` is just calling a normal method, and its type
   * parameter will be inferred to _something_ if there is nothing else to constrain it!
   *
   * Finally, remove the `println` and change the macro implementation to quote the type name as its
   * return value (instead of `""`).
   */
  inline def typeName[T]: String = ${typeNameMacro}

  def typeNameMacro[T](using Quotes): Expr[String] =
    import quotes.*
    //println(Type.show[T])
    '{""}

/**
 * Macro implementations need `Type[T]` instances for every type parameter if its behavior is to
 * depend on the type in any way. Instances of `Type[T]` are primarily used as scrutinees for
 * pattern matching. `Type`s are often contextual values, and `Type.of[T]` is shorthand for
 * `summon[Type[T]]`.
 */

object exercise13:
  /**
   * EXERCISE 13
   *
   * In the same way we can match on `Expr`s with `case '{...}` patterns, we can match on `Type`s
   * with `case '[...]` patterns.
   *
   * Here's a less general version of the `typeName` macro. It matches strings and integers, but
   * fails for any other type. Check how that failure presents itself to a developer using this
   * macro.
   *
   * Uncomment the RHS of the final case and see what happens. It might not be what you would
   * expect! Remember: `typeNameMacro` is a macro, but the body of `typeNameMacro` is not an
   * `inline` context, so `compiletime.error` is *guaranteed* to produce a compile error every time.
   *
   * Use `quotes.reflect.report.errorAndAbort` to produce an error instead.
   */
  
  inline def typeName[T]: String = ${typeNameMacro}

  def typeNameMacro[T: Type](using Quotes): Expr[String] =
    import quotes.*
    Type.of[T] match
      case '[ Int ]    => '{"integer"}
      case '[ String ] => '{"string"}
      case _           => ??? // compiletime.error("Not supported")


object exercise14:
  /**
   * EXERCISE 14
   *
   * We can also match on more complex types. Remember the rules for initial capitalization of
   * identifiers in pattern matches, and that they apply to types too. Read the first case pattern
   * below:
   *
   * `case '[ Option[t] ] =>`
   *
   * This pattern performs a few duties:
   * 1. it matches if the type is a `scala.Option` of _something_
   * 2. it binds the Option type's type parameter to `t`, so that `t` is a *type* on the RHS
   * 3. it makes a contextual instance of `Type[t]` available on the RHS
   *
   * These are all subtle points, and should be understood very clearly! (1) makes it possible to
   * deconstruct types in a pattern. (2) makes it possible to refer to the deconstructed parts of
   * the type on the RHS. And (3) provides us with an on-heap object which represents that type.
   *
   * Try deconstructing some more types in the pattern match.
   */
  inline def typeName[T]: String = ${typeNameMacro}

  def typeNameMacro[T: Type](using Quotes): Expr[String] =
    import quotes.*
    Type.of[T] match
      case '[ Option[t] ] => Expr("an option"+Type.show[t])
      case '[ List[t] ]   => Expr("a list of "+Type.show[t])
      case '[ t ]         => Expr(Type.show[t])

/**
 * Unlike terms, `Type`s are not subject to the phase consistency rules. We do not have to use
 * quotes and splices when referring to types, since they are "universal".
 *
 * But a macro can generate code which will not be compiled until a later phase, and may need to
 * refer to types which do not exist yet. How can this possibly work?
 *
 * Pattern matching on types is a principal means of doing so. Different branches in the macro's
 * runtime code correspond to different ASTs being generated, and each must be consistent with the
 * assumptions which are true for its branch.
 *
 * Often we need to work with a type, such as `t`, which we know nothing about, except that it is
 * called `t` and it is the same type as other types called `t` in the same scope -- this is the
 * the same as in the body of a method parameterized on a type, say, `T`.
 */

object exercise15:
  /**
   * EXERCISE 15
   *
   * The macro `sortSortables` is designed to take a collection, either a `List` or `Set`, and
   * if it's a `List` (whose elements are ordered), it will sort the elements. If it's a `Set`, then
   * there's no need. We need behavior that's dependent on the collection type, including a
   * contextual `Ordering` instance for the element type, if (and only if) we have a `List`.
   * 
   * Look at the implementation of `sortSortablesMacro`. Note how it pattern matches on the `xs`
   * value, binding `t` to the (unknown) type parameter of `List` or `Set`, but at the same time,
   * binding `xs` to a value which is an instance of `List[t]` or `Set[t]`, and *shadowing* the
   * previous `xs` (which is typed as `T`). The relationship between `xs` and the type `t` is
   * critical for typechecking the RHS.
   *
   * Test the macro by calling it in `playground.scala` with different parameters:
   * - a `List[Int]`
   * - a `List[Exception]`
   * - a `Set[Int]`
   * - a `Set[Exception]`
   *
   * Next, compare the cases for `List` and `Set`. `Set` simply returns the same value, unaltered.
   * But the `List` case uses the `Expr.summon` method to produce code which provides an
   * `Ordering[t]` when the macro expands: an `Expr[Ordering[t]]`. Look at the quoted block at the
   * end, and check that everything typechecks, and as phase-consistent.
   *
   * To make "typechecking in your head" easier, you can do the following:
   *  1. Pretend every `Expr[T]` is just a `T`
   *  2. Pretend every `${}` splice is just the spliced value (without the dollar/braces)
   *  3. Pretend every `'{}` quote is just the quoted value (without the quote/braces)
   *
   * For the code to compile, we had to add an extra `.asExprOf[T]` to the quoted block. Can you
   * work out why?
   *
   */
  inline def sortSortables[T](xs: T): T = ${sortSortablesMacro[T]('xs)}

  def sortSortablesMacro[T: Type](xs: Expr[T])(using Quotes): Expr[T] =
    import quotes.*, reflect.*
    xs match
      case '{ $xs: List[t] } =>
        val ord: Expr[Ordering[t]] = Expr.summon[Ordering[t]].getOrElse:
          report.errorAndAbort("Can't sort "+Type.show[t])
        
        '{ $xs.sorted(using $ord) }.asExprOf[T]
      
      case '{ $xs: Set[t] } =>
        xs
      
      case _ =>
        report.errorAndAbort("This type is not supported")

import polynomials.{ParseError, Polynomial}

object exercise16a:
  /**
   * EXERCISE 16a
   *
   * Start a REPL (`sbt console`) and import `polynomials.*`. This provides a simple parser for
   * polynomials in `x` over rational coefficients, with some reasonable limitations for simplicity.
   *
   * Try parsing the following strings with `Polynomial(str)`
   *  - `x + 1`
   *  - `x^3 + 2x^2 - 1`
   *  - `1/4x^4 + 1/3x^3 + 1/2x^2 + x`
   *
   * These should produce `Polynomial` data structures. Now try applying some numeric values to them,
   * for example:
   *  - `Polynomial("2x + 1")(3.0)`
   *
   * This all works adequately, but what happens if we were to try to parse `Polynomial("t + 1")`
   * or `Polynomial("x/2 + 1")` (which are both meaningful, but are not supported by the parser. We
   * would unfortunately find out only at *runtime*.
   *
   * Let's try to write a method which checks polynomials at compile-time! The stub implementation
   * below constructs the `Polynomial` without checking if it's valid.
   *
   * Use the `.value` method on the `Expr[String]` parameter to convert it from an `Expr` representing
   * a `String` to an actual `String` object on the heap. The `value` method returns a
   * `Some[String]` if this is possible. Consider under what circumstances this is possible or not.
   *
   * Provide an appropriate error message if it's not possible to get the `String` value at
   * compile-time.
   *
   * Now we have the string, check whether a polynomial can be constructed successfully in a try/catch
   * block. If not, then produce a compile error. (Note that we will end up parsing the polynomial
   * twice: once at compiletime, and again at runtime.)
   */
  import polynomials.*
 
  inline def polynomial(value: String): Polynomial = ${polynomialMacro('value)}

  def polynomialMacro(expr: Expr[String])(using Quotes): Expr[Polynomial] =
    import quotes.*, reflect.*
    '{Polynomial($expr)}
   
object exercise16b:
  /**
   * EXERCISE 16b
   *
   * Using the previous implementation of `polynomialMacro` (remember, it's an ordinary method),
   * let's arrange for it to be called in a different way, as an interpolated string (with no
   * interpolations, like so:
   *
   * `poly"4x^3 + 2x^2 - x - 5"`
   *
   * Before typechecking, we need to know that this call is transformed into,
   *
   * `StringContext("4x^3 + 2x^2 - x - 5").poly()`
   *
   * So we need to implement `poly()` as an extension method macro on `StringContext`, and then we
   * can just call the original `polynomialMacro` method.
   *
   * Experiment with tweaking the implementation below. Try calling it in `playground.scala`. Try
   * removing the `inline` from before `ctx` or `def poly`. Try to understand how the pattern
   * (which includes the `Varargs` extractor and the `*` for repeated arguments) works.
   *
   * Look up the definition of the `Exprs` extractor in
   * `github:lampepfl/dotty/library/src/scala/quoted/Exprs.scala`, and see if it could be useful
   * inside the `Varargs` extractor.
   */

  import exercise16a.polynomialMacro
  import polynomials.*

  extension (inline ctx: StringContext) inline def poly(): Polynomial = ${polyMacro('ctx)}

  def polyMacro(ctx: Expr[StringContext])(using Quotes): Expr[Polynomial] = ctx match
    case '{ StringContext(${Varargs(strs)}*) } => strs match
      case Seq(str) => polynomialMacro(str)
    
object exercise17:
  /**
   * EXERCISE 17
   *
   * It's easy to construct `Expr`s of primitive types and even some collection types, but it
   * doesn't work in general. Below is a full, more concise implementation of the polynomial
   * StringContext macro from the last exercise, and on the line beginning `try`, we wrap a
   * newly-constructed `Polynomial` value in `Expr`.
   *
   * This only works because there is a contextual `ToExpr[Polynomial]` in scope. Comment it out,
   * and see what error we get. But the given instance of `ToExpr[Polynomial]` does not have a
   * valid implementation. Write the implementation, and then call the macro in `playground.scala`
   * to test it.
   */
  import polynomials.*
 
  extension (inline ctx: StringContext) inline def poly(): Polynomial = ${polyMacro('ctx)}

  def polyMacro(ctx: Expr[StringContext])(using Quotes): Expr[Polynomial] = ctx match
    case '{ StringContext(${Varargs(Seq(str))}*) } =>
      try Expr(Polynomial(str.valueOrAbort))
      catch case err: ParseError => quotes.reflect.report.errorAndAbort("Bad polynomial")
  
  given ToExpr[Polynomial] with
    def apply(poly: Polynomial)(using Quotes): Expr[Polynomial] = ???
      