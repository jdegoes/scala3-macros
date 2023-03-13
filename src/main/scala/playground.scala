package playground
/**
 * This file is a place where we can experiment with macros defined elsewhere. This is necessary
 * because we can't invoke a macro in the file it is defined in.
 *
 * To try a method from `sbt`, call:
 * sbt> runMain playground.run
 */

@main
def run(): Unit =
  println("Hello world")

object experimental:
  val _ = ()