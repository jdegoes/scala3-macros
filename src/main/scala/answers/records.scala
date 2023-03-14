package recordTypes

import scala.quoted.*

case class Rec(map: Map[String, Any]) extends Selectable:
  def selectDynamic(name: String): Any = map(name)

type Person = Rec {
  def name: String
  def age: Int
}

val personRec = Rec(Map("name" -> "Jack Smith", "age" -> 71)).asInstanceOf[Person]

class Schema[RecType <: Rec](schema: Map[String, String]):
  def make(values: Map[String, Any]): Option[RecType] =
    if schema.keys.forall(values.contains) then Some(Rec(values).asInstanceOf[RecType]) else None

class SchemaType[+Name <: String & Singleton, Type]()
object SchemaType:
  given SchemaType["String", String]()
  given SchemaType["Int", Int]()

object Schema:
  def readSchema(schema: List[String], map: Map[String, String] = Map()): Map[String, String] =
    schema match
      case s"$key: $typ" :: tail => readSchema(tail, map.updated(key, typ))
      case _ :: tail             => readSchema(tail, map)
      case Nil                   => map

  transparent inline def parse(filename: String): Schema[?] = ${parseMacro('filename)}

  def parseMacro(filename: Expr[String])(using Quotes): Expr[Schema[?]] =
    import quotes.reflect.*
    val map = readSchema(scala.io.Source.fromFile(filename.valueOrAbort).getLines.to(List))

    def mkType(keyTypes: List[(String, String)], tpe: TypeRepr): TypeRepr =
      keyTypes match
        case Nil => tpe
        case (name, typeName) :: tail =>
          val typeNameRepr = ConstantType(StringConstant(typeName)).asType match
            case '[str] =>
              Expr.summon[SchemaType[str & String & scala.Singleton, ?]] match
                case Some('{ ${_}: schemaType }) => Type.of[schemaType] match
                  case '[ SchemaType[?, fieldType] ] => TypeRepr.of[fieldType]
                  case other => report.errorAndAbort(s"Unrecognized field type: ${other}")
                case other => report.errorAndAbort(s"Unrecognized field type: ${other}")

          mkType(tail, Refinement(tpe, name, typeNameRepr))
    
    mkType(map.to(List), TypeRepr.of[Rec]).asType match
      case '[rec] => '{Schema[rec & Rec](${Expr(map)})}