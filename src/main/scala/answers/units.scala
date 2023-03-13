package units

import scala.quoted.*

trait UnitType
trait Metre[N <: Int] extends UnitType
trait Kilogram[N <: Int] extends UnitType
trait Second[N <: Int] extends UnitType

extension (d: Double)
  def *[U <: UnitType](quantity: Quantity[U]) = Quantity[U](quantity.value*d)
  def /[U <: UnitType](quantity: Quantity[U]) = Quantity[U](quantity.value/d)

case class Quantity[U <: UnitType](value: Double):
  def +(right: Quantity[U]): Quantity[U] = Quantity(value + right.value)
  def -(right: Quantity[U]): Quantity[U] = Quantity(value - right.value)
  
  transparent inline def *[V <: UnitType](right: Quantity[V]): Any =
    ${Quantity.multiply[U, V]('this, 'right, false)}
  
  transparent inline def /[V <: UnitType](right: Quantity[V]): Any =
    ${Quantity.multiply[U, V]('this, 'right, true)}

object Quantity:
  def multiply[U <: UnitType: Type, V <: UnitType: Type](left: Expr[Quantity[U]], right: Expr[Quantity[V]], divide: Boolean)(using Quotes): Expr[Any] =
    import quotes.reflect.*

    def typeToMap(tpe: TypeRepr, parts: Map[TypeRepr, Int] = Map()): Map[TypeRepr, Int] = tpe match
      case AppliedType(repr, List(ConstantType(IntConstant(n)))) =>
        Map(repr -> n)
      case AndType(left, right) =>
        typeToMap(left) ++ typeToMap(right)

    def add(left: Map[TypeRepr, Int], right: Map[TypeRepr, Int]): Map[TypeRepr, Int] =
      right.headOption match
        case None =>
          left
        case Some((repr, n)) =>
          add(left.updated(repr, left.getOrElse(repr, 0) + (if divide then -n else n)), right.tail)

    def mkType(parts: List[(TypeRepr, Int)], acc: TypeRepr = TypeRepr.of[UnitType]): TypeRepr =
      parts match
        case Nil =>
          acc
        case (repr, n) :: tail =>
          mkType(tail, AndType(acc, AppliedType(repr, List(ConstantType(IntConstant(n))))))

    val resultUnits = add(typeToMap(TypeRepr.of[U]), typeToMap(TypeRepr.of[V])).filter(_(1) != 0)
    val newValue = if divide then '{$left.value/$right.value} else '{$left.value*$right.value}
    
    if resultUnits.isEmpty then newValue else
      mkType(resultUnits.to(List)).asType match
        case '[ q ] => '{Quantity[q & UnitType]($newValue)}