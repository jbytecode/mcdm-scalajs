val scala3Version = "3.10.1"


lazy val root = project
  .in(file("."))
  .settings(
    name := "mcdm",
    version := "0.1.2",

    scalaVersion := scala3Version,


    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "1.1.1" % Test,
      "org.scala-js" %%% "scalajs-dom" % "2.8.1")
  )


enablePlugins(ScalaJSPlugin)

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

// Single Threaded Tests
Test / parallelExecution := false
