/*
 *
 */
for (i <- 1 to 21) {
    val ts = (1 to i).map("T".+).mkString(", ")
    val output =
      s"""def apply[$ts, R <: AnyRef](f: ($ts) => R)(implicit ${(1 to i).map(n => "m" + n + ": Maanifest[T" + n + "]").mkString(", ")}, tag: ClassTag[R]): Unit = {
      |  processManifests(${(1 to i).map("m".+).mkString(", ")}) {
      |    case List(${(1 to i).map("pm".+).mkString(", ")}) =>
      |      new StepBody(name, regex)((${(1 to i).map("value" + _ + ": Any").mkString(", ")}) => {
      |        process[R](${(1 to i)})
      |      })
      |  }
      |}
      |"""
}
