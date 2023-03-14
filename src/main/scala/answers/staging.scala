package answers.multistageProgramming

import polynomials.*
import scala.quoted.*

given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)

enum Op:
  case Raise
  case Acc(k: Double)

extension (poly: Polynomial)
  def ops: List[Op] =
    import Op.*
    
    def recur(n: Int, terms: List[(Int, Double)], ops: List[Op]): List[Op] = terms match
      case Nil =>
        ops.reverse
      case (c, k) :: tail =>
        if n == c then recur(n + 1, tail, Raise :: Acc(k) :: ops)
        else recur(n + 1, terms, Raise :: ops)

    recur(0, poly.terms.to(List).sortBy(_(0)), Nil)

  def runtimeCalc: (Double => Double) = staging.run:
    '{
      (x: Double) =>
        var a: Double = 0.0
        var p: Double = 1.0
        ${
          poly.ops.foldLeft('{()}):
            case (insts, Op.Raise)  => '{ $insts; p = p * x }
            case (insts, Op.Acc(k)) => '{ $insts; a = a + ${Expr(k)}*p }
        }
        a
    }

  def *(double: Double): Polynomial = new Polynomial(poly.terms.mapValues(_*double).to(Map))

  def integrate(from: Double, to: Double, steps: Int): Double =
    val epsilon = (to - from)/steps
    val calc: (Double => Double) = (poly*epsilon).runtimeCalc
    //val calc: (Double => Double) = (poly*epsilon).apply(_) // Slow version, for comparison
    var sum: Double = 0.0
    var i = 0
    
    while i < steps do
      sum += calc(epsilon*i + from)
      i += 1
    
    sum

object Macro
  // inline def calc(inline ops: List[Op]): Double => Double = ${calcMacro('ops)}

  // given FromExpr[Op] with
  //   def unapply(expr: Expr[Op])(using Quotes): Option[Op] = expr match
  //     case '{ Op.Raise }   => Some(Op.Raise)
  //     case '{ Op.Acc($k) } => k.value.map(Op.Acc(_))
  //     case _               => None

  // def calcMacro(ops: Expr[List[Op]])(using Quotes): Expr[Double => Double] =
  //   '{
  //     (x: Double) =>
  //       var a: Double = 0.0
  //       var p: Double = 1.0
  //       ${
  //         ops.valueOrAbort.foldLeft('{()}):
  //           case (insts, Op.Raise)  => '{ $insts; p = p * x }
  //           case (insts, Op.Acc(k)) => '{ $insts; a = a + ${Expr(k)}*p }
  //       }
  //       a
  //   }
