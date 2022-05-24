
/*
 * Generates the evil looking apply methods in DataStored#DataStoreParameterTypeBody for Function1 to Function22
 * Partially copied from https://github.com/cucumber/cucumber-jvm-scala
 */
for (i <- 1 to 21) {
  val ts = (1 to i).map("T".+).mkString(", ")
  val output =
    s"""def apply[${ts}, R <: AnyRef](f: ($ts) => R)(implicit ${(1 to i).map(n => "m" + n + ": Manifest[T" + n + "]").mkString(", ")}, tag: ClassTag[R]): Unit = {
       |  processManifests(${(1 to i).map(n => "m" + n).mkString(", ")}) {
       |    case List(${(1 to i).map("pm" + _).mkString(", ")}) =>
       |      new StepBody(name, regex)((${(1 to i).map("value" + _ + ": Any").mkString(", ")}) => {
       |        process[R](${(1 to i).map("value" + _).mkString(", ")}) {
       |          case List(${(1 to i).map(n => "actual" + n + ": T" + n).mkString(", ")}) =>
       |            f(${(1 to i).map("actual" + _).mkString(", ")})
       |        }
       |      })(${(1 to i).map("pm" + _ + ".asInstanceOf[Manifest[Any]]").mkString(", ")})
       |    case List(${(1 to i + 1).map("pm" + _).mkString(", ")}) =>
       |      new StepBody(name, regex)((${(1 to i + 1).map("value" + _ + ": Any").mkString(", ")}) => {
       |        process[R](${(1 to i + 1).map("value" + _).mkString(", ")}) {
       |          case List(${(1 to i).map(n => "actual" + n + ": T" + n).mkString(", ")}) =>
       |            f(${(1 to i).map("actual" + _).mkString(", ")})
       |        }
       |      })(${(1 to i + 1).map("pm" + _ + ".asInstanceOf[Manifest[Any]]").mkString(", ")})
       |  }
       |}
       |""".stripMargin

  println(output + "\n")
}
