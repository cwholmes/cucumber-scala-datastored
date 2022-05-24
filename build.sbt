import Dependencies._

ThisBuild / scalaVersion     := "2.12.15"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.github.cwholmes"
ThisBuild / organizationName := "cwholmes"
ThisBuild / description := "cucumber-scala-datastored: A cucumber based library for data storage between steps."

ThisBuild / licenses := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.html"))
ThisBuild / scmInfo := Some(
  ScmInfo(
    browseUrl = url("https://github.com/cwholmes/cucumber-scala-datastored"),
    connection = "scm:git@github.com:cwholmes/cucumber-scala-datastored.git"
  )
)
ThisBuild / developers := List(
  Developer(id = "cwholmes", name = "Cody Holmes", email = "", url = url("https://github.com/cwholmes"))
)

// For building jars for JDK8
ThisBuild / javacOptions ++= Seq("-source", "1.8", "-target", "1.8")
Compile / compile / javacOptions ++= Seq("-encoding", "UTF-8", "-Xlint:unchecked", "-Xlint:deprecation")

doc / javacOptions := {
  val opts = Seq("-source", "1.8")
  if (scala.util.Properties.isJavaAtLeast("1.8"))
    opts ++ Seq("-Xdoclint:none")
  else
    opts
}

testOptions += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1")

libraryDependencies ++= Seq(
  "junit"               % "junit"              % "4.13.2" % "test"
)

// import ReleaseTransformations._

// releaseTagName := { (ThisBuild / version).value }

// releaseProcess := Seq[ReleaseStep](
//   checkSnapshotDependencies,
//   inquireVersions,
//   runClean,
//   runTest,
//   setReleaseVersion,
//   commitReleaseVersion,
//   tagRelease,
//   setNextVersion,
//   commitNextVersion,
//   pushChanges
// )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
