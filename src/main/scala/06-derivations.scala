package genericDerivation

/**
 * GENERIC DERIVATION
 *
 * Generic derivation provides the mechanism to make type-classes composable. Each type-class needs
 * to have its "method" of composition specified, usually in one way for product types (like case
 * classes and tuples) and another way for coproduct or sum types (like enumerations and sealed
 * traits).
 *
 * Each generic derivation will be different, but the mechanism for defining those derivations will
 * typically have the same signature and be similar in form.
 */


object exercise1:
  /**
   * EXERCISE 1
   *
   * Look at the `Show` type-class defined below, with two `given` instances. Together with the
   * extension method, this makes it possible to call `.show` on an `Int` or `Double` and have a
   * String returned.
   *
   * Define an additional `Show` instance for `String` (with the obvious, trivial implementation).
   *
   * Now, look at `Stock`. Write a `Show` type-class instance for `Stock` which summons the type-class
   * instances for each of its three fields, and combines them in a String which includes the field
   * names. The output should look like this, for example:
   * ```
   * Stock(symbol=META, price=179.16, quantity=600)
   * ```
   *
   * The implementation should fit on one or two lines. Now let's rewrite it to be more complex!
   * Start the definition with the following definitions:
   * ```
   * val tuple = Tuple.fromProduct(stock).toList
   * val typeclasses = (summon[Show[String]], summon[Show[Double]], summon[Show[Int]]).toList
   * val fieldNames = List("symbol", "price", "quantity")
   * val typeName = "Stock"
   * ```
   * and write a recursive implementation that uses to all four values. You will need to use an
   * unsafe `asInstanceOf` cast (for now) to get it to compile. This will be the basis of a general
   * implementation for a `Show` type-class for any product type.
   */
  trait Show[-T]:
    def show(value: T): String
    
  object Show:
    extension [T](value: T)(using show: Show[T]) def show: String = show.show(value)
    given Show[Int] = _.toString
    given Show[Double] = java.text.DecimalFormat("#.##").format(_).nn
  
import exercise1.*

object exercise2:
  /**
   * EXERCISE 2
   *
   * The derivation API, in `scala.deriving.*`, provides a representation of a type as an instance
   * of a `Mirror`, encoding some details of the type as type members of the `Mirror` type. Here is
   * the definition for the `Stock` case class, below:
   * ```
   * type Mirror {
   *   type MirroredMonoType = Stock
   *   type MirroredType = Stock
   *   type MirroredLabel = "Stock"
   *   type MirroredElemTypes = (String, Double, Int)
   *   type MirroredElemLabels = ("symbol", "price", "quantity")
   * }
   * ```
   * Note that this is a type, but it provides (at compile-time) full details about the type
   * structure of the case class, including singleton string literal types for the type name and
   * field names.
   *
   * As a type, we can't just access these as `String`s. But Scala has a lot of very useful and
   * expressive syntax which works on *values* that we would like to use, that could make working
   * with a type such as `MirroredElem` easier.
   *
   * What if we could _pretend_ that we had an instance of that type (which we never actually use),
   * just so that we can work with those types more easily? This is what `erasedValue[T]` provides.
   *
   * `compiletime.erasedValue[T]` provides, for the purposes of typechecking, an "instance" of
   * `T` which we can match on, which will be erased after typechecking -- so we *can't* ever use
   * it!
   *
   * Take a look at the implementation of `length`. Finish implementing the type-level variant,
   *`typeLength`, which takes a type parameter instead of a type. You will need to specify patterns
   * that match the erased value's type but ignore its value.
   */
  case class Stock(symbol: String, price: Double, quantity: Int)

  def length(tuple: Tuple): Int =
    tuple match
      case EmptyTuple   => 0
      case head *: tail => 1 + length(tail)

//   transparent inline def typeLength[T <: Tuple]: Int =
//     inline compiletime.erasedValue[T] match
//       case ??? => 0
//       case ??? => 1 + typeLength[tail]

object exercise3:
  /**
   * EXERCISE 3
   *
   * We can get the mirror for a product type by summoning its `Mirror`. As long as the type is
   * a product-like type, the compiler will construct the `Mirror` instance.
   *
   * Now, write an inline method which counts the number of fields in the product type passed to it.
   * You will need to add a `using` parameter, and refer to the `typeLength` method from the 
   * previous exercise.
   *
   * Now, add a value parameter to `noOfFields` so that we can call it with a value, and have its
   * type parameter _inferred_ from the value's type, for example,
   * ```
   * noOfFields(Stock("MSF", 242.4, 210))  // returns 3
   * ```
  */
  
  import scala.deriving.*

  case class Stock(symbol: String, price: Double, quantity: Int)
  
  val mirror = summon[Mirror.Of[Stock]]  // return type is a more precise subtype of `Mirror`.

  inline def noOfFields[P]: Int = ???

object exercise4:
  /**
   * EXERCISE 4
   *
   * Below is a full implementation of `fields`. Change the return type of `fields` to
   * `List[String]` and write an inline method with the same recursive structure as `typeLength`
   * which produces a list of the field names.
   *
   * Tip: `valueOf[T]` will get an instance of `T` if it's possible to, but an additional pattern
   * match may be needed to check that our instance of `T` is the type we need!
   */

  import scala.deriving.*
  import scala.compiletime.*

  transparent inline def typeLength[T <: Tuple]: Int =
    inline erasedValue[T] match
      case _: EmptyTuple     => 0
      case _: (head *: tail) => 1 + typeLength[tail]

  inline def fields[P](value: P)(using mirror: Mirror.Of[P]): Int =
    typeLength[mirror.MirroredElemLabels]

object exercise5:
  /**
   * EXERCISE 5
   *
   * We can convert a product value, such as a case class, into a `Tuple` with,
   * `Tuple.fromProductTyped`, and in a transparent inline context, we can use pattern matching to
   * deconstruct the tuple recursively.
   *
   * After matching on `head *: tail`, we have available to us the values `head` and `tail`, as
   * well their types `head.type` and `tail.type`. We now have everything we need to summon a
   * `Show` instance for the `head` and show it, and then to recurse on the list. Finish off the
   * missing part of the implementation.
   * */
  
  import exercise1.Show
  import scala.deriving.*
  import scala.compiletime.summonInline

  transparent inline def derive(tuple: Tuple): List[String] =
    inline tuple match
      case EmptyTuple => Nil
      case cons: (_ *: _) => cons match   // We would like to perform these two matches
        case head *: tail =>              // in a single match, but it doesn't work!
          val shown: String = ???
          shown :: derive(tail)

  inline def derived[P <: Product](using mirror: Mirror.ProductOf[P]): Show[P] =
    (value: P) => derive(Tuple.fromProductTyped(value)).mkString("(", ", ", ")")

object exercise6:
  /**
   * EXERCISE 6
   * 
   * Finally, using the methods `derive` and `fields` from the last two exercises and `zip`, plus
   * the provided value `typeName`, complete the implementation of `derived` below to show a case
   * class in the form `TypeName(field1=value1, field2=value2)`.
   *
   */
  import scala.deriving.*
  import exercise5.derive
  import exercise4.fields

  inline def derived[P <: Product](using mirror: Mirror.ProductOf[P]): Show[P] =
    (value: P) =>
      val tuple = Tuple.fromProductTyped(value)
      val typeName = valueOf[mirror.MirroredLabel]
      
      ""