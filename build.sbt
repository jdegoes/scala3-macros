import sbt.Keys._ 
import sbt._ 

lazy val root = project
  .in(file(""))
  .settings(
    name := "scala3-macros",
    version := "0.1.0",
    scalacOptions ++= Seq(
      "-language:postfixOps",
      "-Ykind-projector",
      //"-Yexplicit-nulls",
      "-source", "future"
    ),
    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % "3.3.0-RC3",
    libraryDependencies += "org.scala-lang" %% "scala3-staging" % "3.3.0-RC3",
    scalaVersion := "3.3.0-RC3"
  )
