name := "ecology"

version := "0.1"

scalaVersion := "2.12.10"
// scalaVersion := "2.13.5"

idePackagePrefix := Some("nl.dgl.ecology")


libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.7"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.7" % "test"

// libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test  // FunSuite


libraryDependencies += "org.apache.spark" %% "spark-sql" % "3.1.2"