package reflectionAndTrees

import scala.quoted.*

/**
 * REFLECTION AND TREES
 *
 * All the examples so far have worked on `Type` and `Expr` instances, which have been opaque black
 * boxes representing types and expressions respectively. They have few useful methods, and our main
 * tool for inspecting them has been pattern matching, which has granted us a limited ability to
 * deconstruct them.
 *
 * Scala 3's reflection capabilities offer a much deeper introspection into `Type`s and `Expr`s as
 * `TypeRepr`s and `Term`s, being the low-level AST elements that are used throughout the compiler.
 */

object exercise1:
  /**
   * EXERCISE 1 (20 mins)
   *
   * `inspect` is an uninteresting macro. Add a new parameter (with type `Any`) and corresponding
   * parameter in `inspectMacro`.
   *
   * Call `.asTerm` on the `Expr[String]` and print the result (at compiletime). Invoke the macro
   * with a parameter (anything!) in `playground.scala`, and see what gets printed.
   *
   * Try as many different parameter expressions as you can!
   */
  inline def inspect(): Unit = ${inspectMacro()}

  def inspectMacro()(using Quotes): Expr[Unit] =
    import quotes.*, reflect.*
    '{()}

object exercise2:
  /**
   * EXERCISE 2 (20 mins)
   *
   * Below is a simplistic macro. Call the macro in `playground.scala`, passing in a string. Change
   * the macro implementation to print the `term` value. This should show a representation of the
   * AST of the string parameter passed into `inspect`.
   *
   * Now try to pattern match on `term`. Extractors are available for different AST types in the
   * `quotes.reflect` object. The extractor for `Constant` is called `StringConstant`, but the
   * structure of `Term#toString` otherwise mirrors the structure of the extractors.
   *
   * `case Inlined(_, _, Literal(ConstantString(str))) =>`
   *
   * Check that the pattern matches successfully by printing the `str` value at compile-time.
   */
  inline def inspect(string: String): String = ${inspectMacro('string)}
   
  def inspectMacro(string: Expr[String])(using Quotes): Expr[String] =
    import quotes.reflect.*
    val term = string.asTerm
    
    string

object exercise3:
  /**
   * EXERCISE 3 (1 hour)
   *
   * Take a look at `github.com:lampepfl/dotty/compiler/src/scala/quoted/runtime/impl/QuotesImpl.scala`
   * for implementations of term extractors. This is currently our best documentation on reflection.
   *
   * Open up `inspections.scala` and run the main class, `inspections.test`. Examine the
   * implementation of `inspectMacro` below and understand how the output is being produced.
   *
   * First, without changing the output, restructure the pattern match to avoid repetition in the
   * three patterns. Split the match into different methods if that's helpful.
   *
   * Now, for each `inspect` call in `inspections.scala`, match its tree using extractors (from the
   * Dotty source code link, above) and give a brief textual description of what was matched. Make
   * sure that we distinguish between each input!
   * 
   */
  inline def inspect(inline any: Any): String = ${inspectMacro('any)}

  def inspectMacro(any: Expr[Any])(using Quotes): Expr[String] =
    import quotes.reflect.*
    
    val matched: String = any.asTerm match
      case Inlined(_, _, Literal(StringConstant(str))) => s"string constant: $str"
      case Inlined(_, _, Literal(IntConstant(int)))    => s"int constant: $int"
      case Inlined(_, _, Ident(name))                  => s"reference to $name"
      case other                                       => s"???: $other"
    
    Expr(matched)

/**
 * Pattern matching is one of the most important tools for working with terms reflectively. It is
 * important not only to be familiar with the most common Scala ASTs node types, but also to know
 * how to recursively dig deeper when you encounter less familiar node types. That skill can be
 * broken down into three steps:
 *
 *  1. Knowing what you have (`println` is your friend!)
 *  2. Understanding what that structure is composed of (search in `QuotesImpl.scala`)
 *  3. Accessing those components (trial and error, usually)
 *
 * These steps can be applied recursively.
 */

object exercise4:
  /**
   * EXERCISE 4 (45 mins)
   *
   * Let's try constructing a new tree. Imagine we have a partial function implemented with a
   * pattern match over several cases, all of which are literal strings. Naively, the scrutinee
   * will be compared to each case in turn, until a match is found.
   *
   * But what if we could map every case by a single small, precomputed integer? The compiler could
   * optimize the pattern match as a "jump" in bytecode without trying to match on every case.
   * (We would have to accept that unexpected input might result in a match.)
   *
   * The high-level steps needed to implement this macro are as follows:
   *
   * 1. Convert the `fn` expression to a term and use pattern matching to get the `CaseDef`s
   * 2. Find all the `CaseDef`s that are string literals, and convert it to a `Map[String, Term]`
   * 3. Calculate a hash function that disambiguates every key in the map (see below).
   * 4. Construct new `CaseDef`s matching the integer of the hashed keys, and put these back into
   *    the original `PartialFunction` - this is fine because its type allows `Int` keys, but
   *    requires copying parts of the original tree.
   * 5. Convert the new `PartialFunction` to an `Expr` with `asExprOf[...]`, and use a quoted block
   *    to `compose` it with a function which computes the the hash key from the `String` input.
   *
   * There are several ways we could compute a hash function for the input, with a lot of room for
   * optimization. We will use a simple approach:
   *
   * 1. Take the `hashCode` of all the `String`s, and take the absolute value (so it's positive)
   * 2. Starting with the `n`, the number of keys we have, calculate each key's hashcode modulo `n`
   * 3. Keep adding `1` to `n` until every key maps to a different value.
   *
   * Note that this does not scale well for large numbers of keys! But we are optimizing
   * time at runtime over time at compile-time and space at runtime.
   */
  inline def optimize[T](inline fn: PartialFunction[Any, T]): Function[String, T] =
    ${Optimizer.optimizeMacro('fn)}


/**
 * Type Representations
 *
 * A `TypeRepr` is a representation of a type we can inspect as a datatype, much like `Term` is a
 * datatype we can inspect, representing terms. Here's the relationship between the four
 * types representing types and terms and the quotes and reflection APIs
 *              
 *                | quotes API | reflect API
 *    ------------+------------+-------------
 *    term space  | Expr[T]    | Term
 *    type space  | Type[T]    | TypeRepr
 *
 * The meaning of type parameters (or their absence) is important, particularly for the distinction
 * between `Type[T]` and `TypeRepr`.
 *
 * - a `Type[T]` always corresponds to a type `T` that is in scope (but in the type namespace)
 * - a `Type[T]` object is not useful (and doesn't make sense) without `T`
 * - if we have a `Type[T]`, then we must have some `T` type we can use in type positions, even if
 *   that type is abstract
 * - a pattern match which binds a new type, say `t`, creates a new scope with both the type `t`
 *   AND a contextual `Type[t]` instance; the type is abstract, but can still be composed in
 *   types, etc
 * - a `TypeRepr` has no type parameter, so there's no immediate way to use it in type position
 * - but it is "just" a data structure
 * - there is overview of the hierarchy of `TypeRepr`s in the Dotty source code:
 *   `github.com:lampepfl/dotty/library/src/scala/quoted/Quotes.scala`
 *
 * To convert between them, use:
 *  - `expr.asTerm`
 *  - `term.asExpr` or `term.asExprOf[T]`
 *  - `typeRepr.asType`
 *  - `TypeRepr.of[T]` given a contextual `Type[T]` in scope
 */

object exercise5:
  /**
   * EXERCISE 5
   * 
   * Let's define a macro called `typeReflect`. It will just take one type parameter. In the body
   * of the macro below, get the `TypeRepr` instance for `T` and print it. Then, in
   * `playground.scala`, call `typeReflect` a couple of times with a selection of interesting types
   * of your choice. Compare the output from `toString` and `show` on the `TypeRepr` instance.
   *
   * Take a look at the AST type node types in
   *   `github.com:lampepfl/dotty/library/src/scala/quoted/Quotes.scala`
   *
   * and try to get the macro to display some of these different type nodes by calling `typeReflect`
   * with different types.
   *
   * Now use tab-completion in Metals to see the members of `TypeRepr`. Ignore the methods which
   * use `Symbol`s for now (we will experiment with these in the next exercise), and experiment with
   * some of the other methods. In particular, try the following:
   *
   * - check whether a type is a type of function with `isFunctionType`
   * - check for equality with the `TypeRepr` of a concrete type using `=:=`
   * - check a subtype relationship with another `TypeRepr` with `<:<`
   * - get a list of type parameters with `typeArgs`
   * - widening a singleton type with `widen`
   * - resolving a type alias with `de-alias`
   *
   * Now, make a change to the definitions of `typeReflect` and `typeReflectMacro`: an a type bound
   * of `<: AnyKind` to the type parameter, `T`. This will allow us to call `typeReflect` with
   * higher-kinded types, like `Either` or `[T] =>> Map[T, T]`.
   *
   * Now, we can try one more experiment:
   * - pass in a type constructor to `typeReflect` and in the macro apply a concrete type to it
   *
   */
  inline def typeReflect[T] = ${typeReflectMacro[T]}
  
  def typeReflectMacro[T: Type](using Quotes): Expr[Unit] =
    import quotes.reflect.*
    '{()}

/**
 * The types and terms we encounter in an AST are generally references to types and terms defined
 * elsewhere in the source or in dependent libraries. The entities themselves are described by
 * `Symbol`s, which can provide us with most of the information the compiler has about those
 * entities.
 */

object exercise6:
  /**
   * EXERCISE 6 (45 minutes)
   *
   * Let's explore symbols. Symbols contain a huge amount of information on a variety of different
   * entities that exist at compile-time, and the best way to understand them is to explore them.
   * Symbols are constructed by the compiler during typechecking. We can't create our own Symbols.
   *
   * First, let's create a macro called `explore` in which we can explore symbols. The implementation
   * below gets symbols for the parameter value and its type. Use tab-completion to find some
   * interesting members of `Symbol`, and `println` them in the macro body.
   *
   * Then, in `playground.scala`, invoke the `explore` macro a couple of times, passing in different
   * parameter values. Add more `println` statements in the macro for different properties, and add
   * more invocations of `explore` with different parameters, and compare and contrast!
   *
   * Note that many methods of `Symbol` return other `Symbol`s (or `List`s of `Symbol`s). This
   * allows us to navigate the relations between different entities.
   *
   * Here are a few interesting things to get started:
   *  - `sym.declaredMethods`
   *  - `sym.declaredMethod("apply")`
   *  - `sym.fullName`
   *  - `sym.flags` (check also the `Flags` object)
   *  - `sym.companionClass.annotations`
   *  - `sym.caseFields`
   */

  inline def explore[T](value: T): Unit = ${exploreMacro[T]('value)}

  def exploreMacro[T: Type](value: Expr[T])(using Quotes): Expr[Unit] =
    import quotes.reflect.*
    val typeSymbol: Symbol = TypeRepr.of[T].typeSymbol
    val termSymbol: Symbol = value.asTerm.symbol
    println(typeSymbol.companionClass)
    '{()}

object exercise7:
  /**
   * EXERCISE 7 (20 minutes)
   * 
   * Let's write a simple macro which takes a single type parameter, and returns the companion
   * object for that type. Below is the stub for macro, `companion`.
   *
   * 1. Write the signature for `companionMacro` based on the signature of `companion`.
   * 2. Update `companion` to invoke `companionMacro`.
   * 3. Make sure we import everything from `quotes.reflect`
   * 4. Get the `TypeRepr` for the type parameter, `T`.
   * 5. Get the `typeSymbol` for the `TypeRepr[T]`
   * 6. Call `companionModule` on the `Symbol`
   * 7. Construct a `Ref` to the symbol
   * 8. Convert the `Ref` into an expression with `asExpr`
   */
  
  transparent inline def companion[T]: Any = ???


object exercise8:
  /**
   * EXERCISE 8 (1 hour)
   *
   * Scala 3 introduces a new feature which provides us with record types. Scala will allow us to
   * define structural types with typed fields. Unlike `Dynamic`, Scala will only allow us to access
   * the fields defined in the type -- we just have to define a way to resolve a field name (as a
   * String) to a value for our particular type of record.
   *
   * Take a look at the definition of `Rec`. It inherits from `Selectable` which is a magic
   * trait! `selectDynamic` is the method that will be used to access fields for this record. So
   * we need to be sure that the `map` value contains all the fields we expect.
   *
   * 1. Create a new structural subtype of `Rec`, called `Person`, with fields `name` (a String)
   *    and `age` (an Int)
   * 2. Create a new instance of the `Rec` case class containing a `Map` with keys `"name"` and
   *    `"age"`, mapped to appropriate values.
   * 3. Use `asInstanceOf` to cast the `Rec` instance to `Person`.
   * 4. Try to access `name` and `age` on the instance of `Person`.
   * 
   * Now, see if you can break it. What happens if you try to access a field called `birthday`?
   * What happens if the map does not contain one of the fields? What if the map contains the field
   * but it has the wrong runtime type?
   *
   * Understand where record types bring new safety (compared to a `Map`, say) and where they don't.
   */
  
  case class Rec(map: Map[String, Any]) extends Selectable:
    def selectDynamic(name: String): Any = map(name)

  /**
   * EXERCISE 8b
   *
   * Remember that a `transparent` method can return a value of a more precise type than its
   * declared return type? We can use this to construct new record types at compile-time which
   * get their structure from runtime values -- such as a schema.
   *
   * We are going to use a `Map[String, String]` to represent a schema in the form
   * ```
   * name: String
   * age: Int
   * ```
   * 
   * Below is an implementation of a method to read such a schema. Write a macro which takes a
   * filename String as a parameter and returns a `Map[String, String]` containing the schema,
   * using the `readSchema` method already provided.
   *
   * This macro will allow us to write a literal filename in source code, and have the compiler
   * turn it into a `Map[String, String]`, all at compile-time. (As long as it can read the file!)
   */

  def readSchema(schema: List[String], map: Map[String, String] = Map()): Map[String, String] =
    schema match
      case s"$key: $typ" :: tail => readSchema(tail, map.updated(key, typ))
      case _ :: tail             => readSchema(tail, map)
      case Nil                   => map
  
  val schema = readSchema(scala.io.Source.fromFile("schema.txt").getLines.to(List))

  inline def readSchemaFile(filename: String): Map[String, String] = ???

  /**
   * EXERCISE 8c
   *
   * We can define a `Schema` class as a factory for creating new record types. Update the macro
   * from Exercise 6b to construct a new Schema instance. But what should its type parameter be?
   * It should be the subtype of `Rec` representing the schema, but for now, just leave it as `Rec`,
   * though, of course, this means that we are not yet using any of the information that we have
   * just read from our schema -- that will be the next step!
   */
  
  abstract class Schema[RecType <: Rec](schema: Map[String, String]):
    def make(values: Map[String, Any]): RecType = Rec(values).asInstanceOf[RecType]
  
  inline def makeSchema(filename: String): Schema[Rec] = ???

  /**
   * EXERCISE 8d
   *
   * In order to specify the `RecType` when we construct our `Schema`, we need to programmatically
   * construct the AST of the refined type.
   *
   * To do this, update the `makeSchema` macro to include a tail recursive function which iterates
   * over each key in the `schema` map, progressively adding refinements to the previous type. The
   * base case will be the `TypeRepr` of `Rec`. Here's the signature of the recursive method to get
   * you started:
   *
   * def mkType(keyTypes: List[(String, String)], tpe: TypeRepr): TypeRepr
   *
   * Don't forget that our schema represents return types as strings, like `"Int"`. We can support
   * a fixed list of a few concrete types, and we can always summon their `TypeRepr` if they're
   * non-abstract types, even without a `Type[?]` instance.
   *
   * The key building block of a refined type is `Refinement` which takes three parameters:
   *  - the TypeRepr we are refining
   *  - a string of the new member name
   *  - the TypeRepr of that member's return type
   *
   * calling `mkType(schema, TypeRepr.of[Rec])` should give us a `TypeRepr` for the new refined
   * type.
   *
   * Finally, don't forget to make `makeSchema` transparent!
   * 
   */
  
  /**
   * EXERCISE 8e
   *
   * If everything worked, you should be able to call `makeSchema` on a filename and get a new
   * `Schema[RecType]` instance that can construct new instances from a key/value map.
   *
   * Try modifying the schema file and recompiling.
   *
   * But it's still unsafe. Here are a couple of improvements to make:
   *
   * 1. If anything goes wrong with reading or parsing the schema file, produce a compile error.
   * 2. Change `def make` to return an `Option[RecType]`, handling the case where the `values` Map
   *    does not contain all the keys.
   */

  /**
   * EXERCISE 8f
   *
   * Try adding a new field to the Schema file, with a new type, such as `Char`. In order to support
   * `Char` instances, we would need to modify the pattern match in our macro to also match on the
   * string `"Char"`.
   *
   * But a user of our library would not always be able to make modifications to the library itself.
   * So, it's not as extensible as we would like. Let's use contextual values (implicits) as a
   * replacement for the hardcoded pattern match in the macro. We will start by creating a
   * contextual type, `SchemaType`, which maps strings to the types they represent.
   * ```
   * trait SchemaType[S <: String & Singleton, T]
   * given SchemaType["String", String]
   * given SchemaType["Int", Int]
   * ```
   *
   * We will change the macro to search for a `SchemaType` instance whose first parameter is the
   * type string from the schema file, and will interpret that to mean the type in the second
   * parameter.
   *
   * We can't use `summon[SchemaType[str, ?]]` in the macro because we don't have the string
   * available as a singleton type when the macro is expanding. We only have it as a runtime value.
   * So we must use `Expr.summon` instead, on a type which we will construct.
   *
   * We want to write `Expr.summon[SchemaType[str, ?]]`, but we still need `str` as a type in scope!
   * One typical way to introduce a `str` type into scope is to pattern match on a `Type[?]`, and
   * we can get a `Type[?]` from a `TypeRepr` using `.asType`, and we can construct a new `TypeRepr`
   * representing a singleton literal `String` type from just a runtime string.
   * 
   * So, in the macro,
   *   1. create a new `ConstantType` of the type name (which is a `TypeRepr`
   *   2. convert it to a `Type[?]`
   *   3. pattern match on it using `'[...]`
   *   4. use `Expr.summon` to do an implicit search for a `SchemaType` with the appropriate
   *      parameters. Note that if the compiler says that it can't prove that type A conforms to
   *      type B, but we know that it does, then we can write `A & B` in place of `A`. If it really
   *      is true that `A <: B`, then `A & B` and `A` are _identical_ types anyway.
   *   5. pattern match on the result of `Expr.summon`, and handle the failure case with an error
   *   6. pattern match the successful result of Expr.summon with,
   *      ```
   *      case Some('{ ${_}: schemaType }) => Type.of[schemaType] match
   *        case '[SchemaType[?, fieldType]] =>
   *      ```
   *   7. get the `TypeRepr` of the type `fieldType` and use it in the `Refinement`, as before.
   *
   * You can now experiment with adding new types in the schema, the data map, and new `SchemaType`
   * instances to make everything compile.
   */

object exercise9:
  /**
   * EXERCISE 9 (1 hour)
   *
   * We often want to work with quantities which have units - metres, joules, or metres per second
   * per second, for example, and perform arithmetic operations on them.
   *
   * When we multiply or divide two numbers, the units should be computed from the operands, but
   * when we add or subtract two numbers, we want to be certain that their units are the same.
   *
   * This can be achieved with a macro! But first we need to decide how to encode combinations of
   * units in a type.
   *
   * We need:
   * - arbitrary combinations of units,
   * - associated with a nonzero number (its power),
   * - whose order is unimportant
   *
   * Let's start by defining some types to represent units:
   * trait UnitType
   * trait Metre[N <: Int] extends UnitType
   * trait Second[N <: Int] extends UnitType
   * trait Kilogram[N <: Int] extends UnitType
   *
   * We can represent `m^2` as the type `Metre[2]` using a singleton type parameter, or `s^-1` as
   * `Second[-1]`.
   *
   * These can then be combined as intersection types, for example, `kg · m^-2 · s^2` would be,
   * `Kilogram[1] & Metre[-2] & Second[2]`. Note that the order of the intersection isn't important.
   *
   * We can then represent quantities with instances of the class,
   * `case class Quantity[U <: UnitType](value: Double)`
   *
   * Note that `Quantity`'s type parameter is invariant.
   *
   * We can define arithmetic operations on `Quantity`. Addition and subtraction are easy, because
   * the type system can already enforce the constraint that the two operands have the same type,
   * and hence the same units:
   * ```
   * def +(right: Quantity[U]): Quantity[U] = Quantity[U](value + right.value)
   * def -(right: Quantity[U]): Quantity[U] = Quantity[U](value - right.value)
   * ```
   *
   * The implementations of `*` and `/` are where it gets interesting!
   *
   * We will need to use a macros, and since the resultant type may be neither of the input types,
   * we know that it will need to be a `transparent` macro which computes its own type.
   * ```
   * def *[V <: UnitType](right: Quantity[V]): Quantity[UnitType] = ???
   * def /[V <: UnitType](right: Quantity[V]): Quantity[UnitType] = ???
   * ```
   *
   * Let's focus on `*` to begin with. Here's a stub:
   * ```
   * transparent inline def *[V <: UnitType](right: Quantity[V]): Any = ???
   * ```
   *
   * Leave the return type as `Any`. Why not `Quantity[UnitType]`?
   *
   * To implement this, let's take the following steps:
   *
   * 1. Pass both `U` and `V` to the macro, with `Type` contextual values
   * 2. Get the `TypeRepr`s for these types deconstruct each recursively into a `Map[TypeRef, Int]`
   *    We will need to use the pattern extractors, `AppliedType`, `TypeRef`, `ConstantType`,
   *    `IntConstant` and `AndType`.
   * 3. Combine the two maps by adding together the values for matching `TypeRef` keys, and
   *    removing any keys with value `0` after addition.
   * 4. Construct a new `TypeRepr` recursively from the keys and values in the `Map`, and convert
   *    this to a `Type`, `q`.
   * 5. Use quotes to instantiate a new `Expr[Quantity[q]]`, with the product of the double values
   *    from the two operands. N.B. The compiler doesn't know that `q` conforms to `UnitType`, but
   *    we know that `q & UnitType` does, and it's safe to write this type.
   * 8. Handle the case where all the units have the power `0` -- and just return a raw `Double`!
   * 7. The implementation of `/` is very similar -- try to implement it without copy/pasting the
   *    entire macro!
   * 8. Now, create extension methods, `*` and `/` on `Double` which take a `Quantity` instance and
   *    Return a new `Quantity`. (These do not need to be macros.)
   *
   * Now try out the macro in `playground.scala`. Try the following:
   *
   * - define unit values, `metres`, `kilograms` and `second` as explicit `Quantity`s with value `1`
   * - create some values by multiplying combinations of `Double`s and `metres`, `kilograms` and
   *   `seconds`
   * - let type inference work out the types; you can check them in Metals or by forcing a type mismatch
   * - define a method `circleArea` which takes a `radius` parameter
   * - define a new `UnitType` trait, `Yard` and a `yards` value
   * - create a new `Quantity` called `yardsPerMetre` with the value `1.09361` and appropriate units
   * - calculate the radius of a circle with radius 300 metres, and then convert the result into
   *   square yards
   * - implement some of Newton's equations of motion for uniform acceleration as methods; are they
   *   sound? (`https://en.wikipedia.org/wiki/Equations_of_motion`)
   *
   * Currently, we create a new `Quantity` object for every value, including intermediate
   * expressions. Can we do better with opaque types? Try it!
   *
   * If opaque types work, check the bytecode that gets generated for some of these methods.
   */
  val _ = ()