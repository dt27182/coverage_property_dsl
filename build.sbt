lazy val root = (project in file(".")).
  settings(
    name := "CoveragePropertyDSL",
    scalaVersion := "2.11.7",
    libraryDependencies += "edu.berkeley.cs" %% "chisel" % "2.3-SNAPSHOT"
  )
