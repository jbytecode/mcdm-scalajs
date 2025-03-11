val scala3Version = "3.6.4"


lazy val root = project
  .in(file("."))
  .settings(
    name := "mcdm",
    version := "0.1.0",

    scalaVersion := scala3Version,


    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "1.0.4" % Test,
      "org.scala-js" %%% "scalajs-dom" % "2.8.0")
  )


enablePlugins(ScalaJSPlugin)

// This is an application with a main method
scalaJSUseMainModuleInitializer := true
