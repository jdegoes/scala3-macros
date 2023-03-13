package treeInspections

import reflectionAndTrees.*

@main
def test(): Unit =
  import exercise3.*
  var x = 10
  List(
    // inspect("hello"),
    // inspect(x),
    // inspect(42),
    // inspect("?"*3),
    // inspect(x*x),
    // inspect(()),
    // inspect(12: Double),
    // inspect("hello world".substring(6)),
    // inspect(List(1, 2, 3)),
    // inspect(List[Int](1, 2, 3)),
    // inspect(List[AnyVal](1, 2, 3)),
    // inspect { (x: Int) => x + 1 },
    // inspect { (x => x + 1): (Int => Int) },
    // inspect(Right[1, 2](2)),
    // inspect(for i <- 1 to 10 do println(i)),
    // inspect((1 to 10).foreach(println)),
    // inspect(x match { case 10 => true; case _ => false }),
    // inspect {
    //   object Y { final val x: Int = 42 }
    //   Y.x
    // }
  ).foreach(println(_))
