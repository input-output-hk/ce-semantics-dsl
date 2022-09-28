val scala3Version = "3.1.3"


lazy val root = project
  .in(file("."))
  .settings(
    name := "ce-semantics-dsl3",
    version := "0.9",

    scalaVersion := scala3Version,

    scalacOptions := Seq(
      "--encoding", "utf8",
      "--unchecked",
      "--deprecation",
      "--explain",
      "--explain-types",
      "-new-syntax",
    ),

    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-text" % "1.9",
      "com.lihaoyi" %% "sourcecode"  % "0.2.8", // https://github.com/com-lihaoyi/sourcecode
      "org.scalatestplus" %% "scalacheck-1-16" % "3.2.12.0", // https://www.scalatest.org/plus/scalacheck
    )
  )
