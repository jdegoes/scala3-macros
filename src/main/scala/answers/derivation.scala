package genericDerivation

import scala.deriving.*
import compiletime.*

trait Show[-T]:
  def show(value: T): String

extension [T: Show](value: T) def show: String = summon[Show[T]].show(value)

object Show:
  given Show[Int] = _.toString
  given Show[String] = identity(_)
  
  private transparent inline def deriveProduct[Labels <: Tuple](tuple: Tuple): List[String] =
    inline tuple match
      case EmptyTuple => Nil
      case cons: (_ *: _) => cons match
        case (head *: tail) => inline erasedValue[Labels] match
          case _: (headLabel *: tailLabels) => inline valueOf[headLabel].asMatchable match
            case label: String =>
              val value = summonInline[Show[head.type]].show(head)
              s"$label=$value" :: deriveProduct[tailLabels](tail)

  private transparent inline def deriveSum[Elements <: Tuple, Labels <: Tuple](ordinal: Int, value: Matchable): String =
    inline erasedValue[Elements] match
      case _: (headType *: tailType) => inline erasedValue[Labels] match
        case _: (headLabel *: tailLabels) => inline valueOf[headLabel].asMatchable match
          case label: String =>
            if ordinal == 0 then
              value match
                case value: `headType` => summonInline[Show[headType]].show(value)
            else deriveSum[tailType, tailLabels](ordinal - 1, value)

  inline given derived[P](using mirror: Mirror.Of[P]): Show[P] = inline mirror match
    case given Mirror.ProductOf[P & Product] =>
      (value: P) => value.asMatchable match
        case value: Product =>
          val elements = deriveProduct[mirror.MirroredElemLabels](Tuple.fromProductTyped(value))
          val typeName = valueOf[mirror.MirroredLabel]
          typeName+elements.mkString("(", ", ", ")")
    
    case given Mirror.SumOf[P] =>
      (value: P) =>
        ???
        // val typeName = valueOf[mirror.MirroredLabel]
        // deriveSum[mirror.MirroredElemTypes, mirror.MirroredElemLabels](summon[Mirror.SumOf[P]].ordinal(value), value)