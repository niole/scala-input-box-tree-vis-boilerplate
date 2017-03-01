enablePlugins(ScalaJSPlugin)

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"
libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.2"

jsDependencies += RuntimeDOM

lazy val commonSettings = Seq(
  scalaVersion := "2.12.1"
)

lazy val root = (project in file("."))
.settings(
  commonSettings,
  name := "Splay Tree"
)
