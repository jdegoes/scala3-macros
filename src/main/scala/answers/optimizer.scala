package reflectionAndTrees

import scala.quoted.*

object Optimizer:
  def optimizeMacro[T: Type](fn: Expr[PartialFunction[Any, T]])(using Quotes): Expr[Function[String, T]] =
    import quotes.*, reflect.*
    val (n, newPartialFunction) = fn.asTerm match
      case Inlined(_, _, Block(List(defDef), term)) => defDef match
        case DefDef(a, b, c, Some(Match(matchId, caseDefs))) =>
          val cases: Map[String, Term] =
            caseDefs.map:
              case CaseDef(Literal(StringConstant(str)), None, block) => str -> block
            .to(Map)
            
          val n: Int =
            LazyList.from(cases.size).find: i =>
              cases.map { c => math.abs(c(0).hashCode)%i }.to(Set).size == cases.size
            .get
          
          val newCaseDefs = cases.to(List).map: (str, block) =>
            CaseDef(Literal(IntConstant(math.abs(str.hashCode)%n)), None, block)
          
          val newDefDef = DefDef.copy(defDef)(a, b, c, Some(Match(matchId, newCaseDefs)))
          n -> Block(List(newDefDef), term)

    '{${newPartialFunction.asExprOf[PartialFunction[Any, T]]}.compose { s => math.abs(s.hashCode)%${Expr(n)} } }