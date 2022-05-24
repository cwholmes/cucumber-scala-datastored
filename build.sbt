import Dependencies._

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

// scala versions

val scala212 = "2.12.15"
val scala213 = "2.13.6"
val scala3 = "3.0.2"

scalaVersion := scala213

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

val project = (projectMatrix in file("."))
  .settings(
    name := "cucumber-scala-datastored",
    libraryDependencies ++= Seq(
      scalaCucumber,
      "junit" % "junit" % "4.13.2" % "test"
    ),
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, n)) if n == 12 =>
          List("org.scala-lang.modules" %% "scala-collection-compat" % "2.7.0")
        case _ => Nil
      }
    },
    Compile / compile / javacOptions ++= Seq("-encoding", "UTF-8", "-Xlint:unchecked", "-Xlint:deprecation"),
    Compile / unmanagedSourceDirectories ++= {
      val sourceDir = (Compile / sourceDirectory).value
      CrossVersion.partialVersion(scalaVersion.value) match {
        // future versions can add directories specific to the scala version
        case _ => Seq()
      }
    },
    Test / unmanagedSourceDirectories ++= {
      val sourceDir = (Test / sourceDirectory).value
      CrossVersion.partialVersion(scalaVersion.value) match {
        // future versions can add directories specific to the scala version
        case _ => Seq()
      }
    }
  )
  .jvmPlatform(scalaVersions = Seq(scala3, scala213, scala212))

import ReleaseTransformations._

releaseTagName := { (ThisBuild / version).value }

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  setNextVersion,
  commitNextVersion,
  pushChanges
)

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
