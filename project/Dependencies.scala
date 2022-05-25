import sbt._

object Dependencies {
  private lazy val cucumberVersion = "6.10.2"

  lazy val junit = "junit" % "junit" % "4.13.2"
  lazy val junitInterface = "com.novocode" % "junit-interface" % "0.11"
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.11"
  lazy val cucumberScala = "io.cucumber" %% "cucumber-scala" % cucumberVersion
  lazy val cucumberCore = "io.cucumber" % "cucumber-core" % cucumberVersion
  lazy val cucumberJunit = "io.cucumber" % "cucumber-junit" % cucumberVersion
  lazy val cucumberExpressions = "io.cucumber" % "cucumber-expressions" % "10.3.0"
}
