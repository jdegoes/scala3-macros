package optimizingMatch

import reflectionAndTrees.*

@main
def optimize(): Unit =
  val partialFn: String => Int = exercise4.optimize:
    case "alpha"   => 1
    case "beta"    => 2
    case "gamma"   => 3
    case "delta"   => 4
    case "epsilon" => 5
    case "zeta"    => 6
    case "eta"     => 7
    case "theta"   => 8
  
  println(partialFn("zeta"))
