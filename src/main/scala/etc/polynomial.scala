package polynomials

import scala.quoted.*

case class ParseError() extends Exception

case class Polynomial(terms: Map[Int, Double]):
  override def toString(): String =
    def sup(n: Int): String = if n == 1 || n == 0 then "" else n.toString.map:
      case '-' => '⁻'
      case d => (d - '0' + '⁰').toChar
    
    terms.to(List).sortBy(-_(0)).zipWithIndex.map:
      case ((n, k), idx) =>
        val p = if idx == 0 then (if k < 0 then "-" else "") else if k < 0 then " - " else " + "
        val k2 = math.abs(k)
        val c = if k2 == 1 && n > 0 then "" else if k == k.toInt then k2.toInt.toString else k2.toString
        val x = if n == 0 then "" else "x"
        val i = if k == 0 || (k == 1 && n != 0) then "" else sup(n)
        p+c+x+i
    .mkString

  def apply(value: Double): Double = terms.foldLeft(0.0):
    case (acc, (power, coefficient)) => acc + coefficient*math.pow(value, power)

object Polynomial:
  def apply(poly: String): Polynomial =
    sealed trait Term
    
    case class Integer(minus: Boolean, number: Int) extends Term:
      def value: Int = if minus then -number else number
  
    case class Fraction(numerator: Int, denominator: Int) extends Term:
      def value: Double = numerator.toDouble/denominator
  
    case class Coefficient(coefficient: Double) extends Term
    
    case class Power(coefficient: Double, minus: Boolean, power: Int) extends Term:
      def value: Int = if minus then -power else power
  
    object Digit:
      def unapply(ch: Char): Option[Int] = if ch.isDigit then Some(ch - '0') else None
  
    @annotation.tailrec
    def recur(chars: List[Char], term: Term, terms: Map[Int, Double]): Map[Int, Double] =
      
      def put(coefficient: Double, power: Int): Map[Int, Double] =
        terms.updated(power, terms.getOrElse(power, 0.0) + coefficient)
     
      term match
        case integer@Integer(minus, number) => chars match
          case Nil              => if number == 0 then throw ParseError() else put(integer.value, 0)
          case '/' :: tail      => recur(tail, Fraction(integer.value, 0), terms)
          case 'x' :: tail      => recur(tail, Coefficient(if integer.value == 0 then (if minus then -1.0 else 1.0) else integer.value), terms)
          case '-' :: tail      => recur(tail, Integer(true, 0), if number == 0 then terms else put(integer.value, 0))
          case '+' :: tail      => recur(tail, Integer(false, 0), if number == 0 then terms else put(integer.value, 0))
          case Digit(d) :: tail => recur(tail, Integer(minus, number*10 + d), terms)
          case _                => throw ParseError()
        
        case fraction@Fraction(numerator, denominator) => chars match
          case Nil              => if denominator == 0 then throw ParseError() else put(fraction.value, 0)
          case Digit(d) :: tail => recur(tail, Fraction(numerator, denominator*10 + d), terms)
          case 'x' :: tail      => recur(tail, Coefficient(fraction.value), terms)
          case '+' :: tail      => recur(tail, Integer(false, 0), put(fraction.value, 0))
          case '-' :: tail      => recur(tail, Integer(true, 0), put(fraction.value, 0))
          case _                => throw ParseError()
  
        case coefficient@Coefficient(value) => chars match
          case Nil              => put(value, 1)
          case '^' :: tail      => recur(tail, Power(value, false, 0), terms)
          case '-' :: tail      => recur(tail, Integer(true, 0), put(value, 1))
          case '+' :: tail      => recur(tail, Integer(false, 0), put(value, 1))
          case _                => throw ParseError()
        
        case power@Power(coefficient, minus, value) => chars match
          case Nil              => put(coefficient, power.value)
          case Digit(d) :: tail => recur(tail, Power(coefficient, minus, value*10 + d), terms)
          case '-' :: tail      => if value == 0 then recur(tail, Power(coefficient, true, 0), terms)
                                   else recur(tail, Integer(true, 0), put(coefficient, power.value))
          case '+' :: tail      => if value == 0 then throw ParseError()
                                   else recur(tail, Integer(false, 0), put(coefficient, power.value))
          case _                => throw ParseError()
          
    Polynomial(recur(poly.toList.filter(_ != ' '), Integer(false, 0), Map()))
  