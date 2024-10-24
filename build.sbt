scalaVersion := "3.3.1"

Global / scalacOptions ++= Seq(
  "-encoding",
  "utf8",
  "-feature",
  "-unchecked",
  "-Werror",
  "-Wunused:all",
  "-Wvalue-discard",
  "-deprecation",
  "-source:future-migration",
  "-language:strictEquality",
  "-Ycheck-all-patmat",
  "-Ycheck-mods",
  "-Ycook-comments",
  "-Yexplicit-nulls",
)

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.4" % "test"
libraryDependencies += "org.typelevel" %% "cats-parse" % "0.3.9"
