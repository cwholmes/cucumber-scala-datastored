package com.github.cwholmes.cucumber

import io.cucumber.scala.ScalaDsl

import scala.collection.mutable
import scala.reflect.{ClassTag, ManifestFactory}

trait DataStored {
  self: ScalaDsl =>
  import DataStored.registeredDataStoreTypes

  private case class StringReturn(string: String)

  private case class DataStoreCapture[T1](wrapped: T1, string: String)

  After {
    registeredDataStoreTypes.clear()
    DataStore.clear()
  }

  if (!registeredDataStoreTypes.contains(DataStored.basicReturn)) {
    ParameterType(DataStored.basicReturn, DataStored.dataStoreRegex)(StringReturn)
    registeredDataStoreTypes.add(DataStored.basicReturn)
  }

  /** Register data store based parameter type.
   */
  val DataStoreParameterType = new DataStoreParameterTypeBody

  final class DataStoreParameterTypeBody {

    def apply[R <: AnyRef](clazz: Class[R])(implicit tag: ClassTag[R]): Unit = {
      val dsName = s"DS-${tag.runtimeClass.getSimpleName}"
      if (!registeredDataStoreTypes.contains(dsName)) {
        new ParameterTypeBody(dsName, DataStored.dataStoreRegex)(fromDataStore[R])
        new ParameterTypeBody(s"$dsName-Returned", DataStored.dataStoreRegex)(fromDataStoreWrapped[R])
        registeredDataStoreTypes.add(dsName)
      }
    }

    private def fromDataStore[R <: AnyRef](implicit tag: ClassTag[R]): String => R = string => {
      val value = DataStore[R](string)
      assert(value != null)
      value
    }

    private def fromDataStoreWrapped[R <: AnyRef](implicit tag: ClassTag[R]): String => DataStoreCapture[R] = string => {
      val value = DataStore[R](string)
      assert(value != null)
      DataStoreCapture(value, string)
    }

  }

  implicit class Regex(sc: StringContext) {
    def r = new scala.util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  val GivenWithReturn = new DataStoreStep("Given")
  val ThenWithReturn = new DataStoreStep("Then")

  final class DataStoreStep(name: String) {
    def apply(regex: String): DataStoreStepBody = new DataStoreStepBody(name, regex)

    def apply(regex: String, returnedIndex: Int): DataStoreStepBody = new DataStoreStepBody(name, regex, returnedIndex)
  }

  final class DataStoreStepBody(name: String, regex: String, returnedIndex: Int = -1) {

    def processManifests(manifests: Manifest[_]*)(pf: PartialFunction[List[Manifest[_]], Unit]): Unit = {
      val foundReturns = DataStored.dataStoredRegexFull.r.findAllIn(regex).map {
        case r""".*DS-(${DataStored.dataStoreRegex})$tagName-Returned.*""" => tagName
        case DataStored.basicReturnFull => DataStored.basicReturn
      }.toSet
      val processedManifests = manifests.map { manifest =>
        if (foundReturns.contains(manifest.runtimeClass.getSimpleName)) { ManifestFactory.classType(classOf[DataStoreCapture[_]], manifest) }
        else manifest
      }.toList
      val finalManifests =
        if (foundReturns.contains(DataStored.basicReturn)) {
          val stringReturnManifest = List(ManifestFactory.classType(classOf[StringReturn]))
          if (returnedIndex == -1 || returnedIndex > manifests.size) { processedManifests ++ stringReturnManifest }
          else {
            val (start, end) = processedManifests.splitAt(returnedIndex - 1)
            start ++ stringReturnManifest ++ end
          }
        } else { processedManifests }
      pf(finalManifests)
    }

    def process[R <: AnyRef](args: Any*)(pf: PartialFunction[List[Any], R])(implicit tag: ClassTag[R]): R = {
      val finalArgs = args.map {
        case DataStoreCapture(wrapped, _) => wrapped
        case arg => arg
      }.filterNot(_.isInstanceOf[StringReturn]).toList
      val returned = pf(finalArgs)
      args.foreach {
        case DataStoreCapture(_, string) => processReturned(string, returned)
        case StringReturn(string) => processReturned(string, returned)
        case _ =>
      }
      returned
    }

    def processReturned[R <: AnyRef](string: String, returned: R)(implicit tag: ClassTag[R]): Unit = returned match {
      case TypedReturn(returns) => returns.foreach { case (innerReturn, tag) => DataStore(string, innerReturn)(tag) }
      case anyRef => DataStore[R](string, anyRef)
    }

    def apply[R <: AnyRef](f: => R)(implicit tag: ClassTag[R]): Unit = {
      if (regex.contains("{DS-Returned}")) {
        new StepBody(name, regex)((dataStoreString: StringReturn) => { DataStore[R](dataStoreString.string, f) })
      } else { new StepBody(name, regex)(() => f) }
    }

    /*
     * Generated apply1 to apply21 below
     */
    def apply[T1, R <: AnyRef](f: T1 => R)(implicit m1: Manifest[T1], tag: ClassTag[R]): Unit = {
      processManifests(m1) {
        case List(pm1) =>
          new StepBody(name, regex)((value1: Any) => { process[R](value1) { case List(actual1: T1) => f(actual1) } })(pm1.asInstanceOf[Manifest[Any]])
        case List(pm1, pm2) =>
          new StepBody(name, regex)((value1: Any, value2: Any) => { process[R](value1, value2) { case List(actual1: T1) => f(actual1) } })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]])
      }
    }

    def apply[T1, T2, R <: AnyRef](f: (T1, T2) => R)(implicit m1: Manifest[T1], m2: Manifest[T2], tag: ClassTag[R]): Unit = {
      processManifests(m1, m2) {
        case List(pm1, pm2) =>
          new StepBody(name, regex)((value1: Any, value2: Any) => {
            process[R](value1, value2) { case List(actual1: T1, actual2: T2) => f(actual1, actual2) }
          })(pm1.asInstanceOf[Manifest[Any]], pm2.asInstanceOf[Manifest[Any]])
        case List(pm1, pm2, pm3) =>
          new StepBody(name, regex)((value1: Any, value2: Any, value3: Any) => {
            process[R](value1, value2, value3) { case List(actual1: T1, actual2: T2) => f(actual1, actual2) }
          })(pm1.asInstanceOf[Manifest[Any]], pm2.asInstanceOf[Manifest[Any]], pm3.asInstanceOf[Manifest[Any]])
      }
    }

    def apply[T1, T2, T3, R <: AnyRef](
                                        f: (T1, T2, T3) => R)(implicit m1: Manifest[T1], m2: Manifest[T2], m3: Manifest[T3], tag: ClassTag[R]): Unit = {
      processManifests(m1, m2, m3) {
        case List(pm1, pm2, pm3) =>
          new StepBody(name, regex)((value1: Any, value2: Any, value3: Any) => {
            process[R](value1, value2, value3) { case List(actual1: T1, actual2: T2, actual3: T3) => f(actual1, actual2, actual3) }
          })(pm1.asInstanceOf[Manifest[Any]], pm2.asInstanceOf[Manifest[Any]], pm3.asInstanceOf[Manifest[Any]])
        case List(pm1, pm2, pm3, pm4) =>
          new StepBody(name, regex)((value1: Any, value2: Any, value3: Any, value4: Any) => {
            process[R](value1, value2, value3, value4) { case List(actual1: T1, actual2: T2, actual3: T3) => f(actual1, actual2, actual3) }
          })(pm1.asInstanceOf[Manifest[Any]], pm2.asInstanceOf[Manifest[Any]], pm3.asInstanceOf[Manifest[Any]], pm4.asInstanceOf[Manifest[Any]])
      }
    }

    def apply[T1, T2, T3, T4, R <: AnyRef](
                                            f: (T1, T2, T3, T4) => R)(implicit m1: Manifest[T1], m2: Manifest[T2], m3: Manifest[T3], m4: Manifest[T4], tag: ClassTag[R]): Unit = {
      processManifests(m1, m2, m3, m4) {
        case List(pm1, pm2, pm3, pm4) =>
          new StepBody(name, regex)((value1: Any, value2: Any, value3: Any, value4: Any) => {
            process[R](value1, value2, value3, value4) {
              case List(actual1: T1, actual2: T2, actual3: T3, actual4: T4) => f(actual1, actual2, actual3, actual4)
            }
          })(pm1.asInstanceOf[Manifest[Any]], pm2.asInstanceOf[Manifest[Any]], pm3.asInstanceOf[Manifest[Any]], pm4.asInstanceOf[Manifest[Any]])
        case List(pm1, pm2, pm3, pm4, pm5) =>
          new StepBody(name, regex)((value1: Any, value2: Any, value3: Any, value4: Any, value5: Any) => {
            process[R](value1, value2, value3, value4, value5) {
              case List(actual1: T1, actual2: T2, actual3: T3, actual4: T4) => f(actual1, actual2, actual3, actual4)
            }
          })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]])
      }
    }

    def apply[T1, T2, T3, T4, T5, R <: AnyRef](f: (T1, T2, T3, T4, T5) => R)(implicit
                                                                             m1: Manifest[T1],
                                                                             m2: Manifest[T2],
                                                                             m3: Manifest[T3],
                                                                             m4: Manifest[T4],
                                                                             m5: Manifest[T5],
                                                                             tag: ClassTag[R]): Unit = {
      processManifests(m1, m2, m3, m4, m5) {
        case List(pm1, pm2, pm3, pm4, pm5) =>
          new StepBody(name, regex)((value1: Any, value2: Any, value3: Any, value4: Any, value5: Any) => {
            process[R](value1, value2, value3, value4, value5) {
              case List(actual1: T1, actual2: T2, actual3: T3, actual4: T4, actual5: T5) => f(actual1, actual2, actual3, actual4, actual5)
            }
          })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]])
        case List(pm1, pm2, pm3, pm4, pm5, pm6) =>
          new StepBody(name, regex)((value1: Any, value2: Any, value3: Any, value4: Any, value5: Any, value6: Any) => {
            process[R](value1, value2, value3, value4, value5, value6) {
              case List(actual1: T1, actual2: T2, actual3: T3, actual4: T4, actual5: T5) => f(actual1, actual2, actual3, actual4, actual5)
            }
          })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]])
      }
    }

    def apply[T1, T2, T3, T4, T5, T6, R <: AnyRef](f: (T1, T2, T3, T4, T5, T6) => R)(implicit
                                                                                     m1: Manifest[T1],
                                                                                     m2: Manifest[T2],
                                                                                     m3: Manifest[T3],
                                                                                     m4: Manifest[T4],
                                                                                     m5: Manifest[T5],
                                                                                     m6: Manifest[T6],
                                                                                     tag: ClassTag[R]): Unit = {
      processManifests(m1, m2, m3, m4, m5, m6) {
        case List(pm1, pm2, pm3, pm4, pm5, pm6) =>
          new StepBody(name, regex)((value1: Any, value2: Any, value3: Any, value4: Any, value5: Any, value6: Any) => {
            process[R](value1, value2, value3, value4, value5, value6) {
              case List(actual1: T1, actual2: T2, actual3: T3, actual4: T4, actual5: T5, actual6: T6) =>
                f(actual1, actual2, actual3, actual4, actual5, actual6)
            }
          })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]])
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7) =>
          new StepBody(name, regex)((value1: Any, value2: Any, value3: Any, value4: Any, value5: Any, value6: Any, value7: Any) => {
            process[R](value1, value2, value3, value4, value5, value6, value7) {
              case List(actual1: T1, actual2: T2, actual3: T3, actual4: T4, actual5: T5, actual6: T6) =>
                f(actual1, actual2, actual3, actual4, actual5, actual6)
            }
          })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]])
      }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, R <: AnyRef](f: (T1, T2, T3, T4, T5, T6, T7) => R)(implicit
                                                                                             m1: Manifest[T1],
                                                                                             m2: Manifest[T2],
                                                                                             m3: Manifest[T3],
                                                                                             m4: Manifest[T4],
                                                                                             m5: Manifest[T5],
                                                                                             m6: Manifest[T6],
                                                                                             m7: Manifest[T7],
                                                                                             tag: ClassTag[R]): Unit = {
      processManifests(m1, m2, m3, m4, m5, m6, m7) {
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7) =>
          new StepBody(name, regex)((value1: Any, value2: Any, value3: Any, value4: Any, value5: Any, value6: Any, value7: Any) => {
            process[R](value1, value2, value3, value4, value5, value6, value7) {
              case List(actual1: T1, actual2: T2, actual3: T3, actual4: T4, actual5: T5, actual6: T6, actual7: T7) =>
                f(actual1, actual2, actual3, actual4, actual5, actual6, actual7)
            }
          })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]])
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8) =>
          new StepBody(name, regex)((value1: Any, value2: Any, value3: Any, value4: Any, value5: Any, value6: Any, value7: Any, value8: Any) => {
            process[R](value1, value2, value3, value4, value5, value6, value7, value8) {
              case List(actual1: T1, actual2: T2, actual3: T3, actual4: T4, actual5: T5, actual6: T6, actual7: T7) =>
                f(actual1, actual2, actual3, actual4, actual5, actual6, actual7)
            }
          })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]],
            pm8.asInstanceOf[Manifest[Any]])
      }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, R <: AnyRef](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R)(implicit
                                                                                                     m1: Manifest[T1],
                                                                                                     m2: Manifest[T2],
                                                                                                     m3: Manifest[T3],
                                                                                                     m4: Manifest[T4],
                                                                                                     m5: Manifest[T5],
                                                                                                     m6: Manifest[T6],
                                                                                                     m7: Manifest[T7],
                                                                                                     m8: Manifest[T8],
                                                                                                     tag: ClassTag[R]): Unit = {
      processManifests(m1, m2, m3, m4, m5, m6, m7, m8) {
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8) =>
          new StepBody(name, regex)((value1: Any, value2: Any, value3: Any, value4: Any, value5: Any, value6: Any, value7: Any, value8: Any) => {
            process[R](value1, value2, value3, value4, value5, value6, value7, value8) {
              case List(actual1: T1, actual2: T2, actual3: T3, actual4: T4, actual5: T5, actual6: T6, actual7: T7, actual8: T8) =>
                f(actual1, actual2, actual3, actual4, actual5, actual6, actual7, actual8)
            }
          })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]],
            pm8.asInstanceOf[Manifest[Any]])
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8, pm9) =>
          new StepBody(name, regex)(
            (value1: Any, value2: Any, value3: Any, value4: Any, value5: Any, value6: Any, value7: Any, value8: Any, value9: Any) => {
              process[R](value1, value2, value3, value4, value5, value6, value7, value8, value9) {
                case List(actual1: T1, actual2: T2, actual3: T3, actual4: T4, actual5: T5, actual6: T6, actual7: T7, actual8: T8) =>
                  f(actual1, actual2, actual3, actual4, actual5, actual6, actual7, actual8)
              }
            })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]],
            pm8.asInstanceOf[Manifest[Any]],
            pm9.asInstanceOf[Manifest[Any]])
      }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, R <: AnyRef](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R)(implicit
                                                                                                             m1: Manifest[T1],
                                                                                                             m2: Manifest[T2],
                                                                                                             m3: Manifest[T3],
                                                                                                             m4: Manifest[T4],
                                                                                                             m5: Manifest[T5],
                                                                                                             m6: Manifest[T6],
                                                                                                             m7: Manifest[T7],
                                                                                                             m8: Manifest[T8],
                                                                                                             m9: Manifest[T9],
                                                                                                             tag: ClassTag[R]): Unit = {
      processManifests(m1, m2, m3, m4, m5, m6, m7, m8, m9) {
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8, pm9) =>
          new StepBody(name, regex)(
            (value1: Any, value2: Any, value3: Any, value4: Any, value5: Any, value6: Any, value7: Any, value8: Any, value9: Any) => {
              process[R](value1, value2, value3, value4, value5, value6, value7, value8, value9) {
                case List(actual1: T1, actual2: T2, actual3: T3, actual4: T4, actual5: T5, actual6: T6, actual7: T7, actual8: T8, actual9: T9) =>
                  f(actual1, actual2, actual3, actual4, actual5, actual6, actual7, actual8, actual9)
              }
            })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]],
            pm8.asInstanceOf[Manifest[Any]],
            pm9.asInstanceOf[Manifest[Any]])
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8, pm9, pm10) =>
          new StepBody(name, regex)(
            (value1: Any, value2: Any, value3: Any, value4: Any, value5: Any, value6: Any, value7: Any, value8: Any, value9: Any, value10: Any) => {
              process[R](value1, value2, value3, value4, value5, value6, value7, value8, value9, value10) {
                case List(actual1: T1, actual2: T2, actual3: T3, actual4: T4, actual5: T5, actual6: T6, actual7: T7, actual8: T8, actual9: T9) =>
                  f(actual1, actual2, actual3, actual4, actual5, actual6, actual7, actual8, actual9)
              }
            })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]],
            pm8.asInstanceOf[Manifest[Any]],
            pm9.asInstanceOf[Manifest[Any]],
            pm10.asInstanceOf[Manifest[Any]])
      }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R <: AnyRef](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R)(implicit
                                                                                                                       m1: Manifest[T1],
                                                                                                                       m2: Manifest[T2],
                                                                                                                       m3: Manifest[T3],
                                                                                                                       m4: Manifest[T4],
                                                                                                                       m5: Manifest[T5],
                                                                                                                       m6: Manifest[T6],
                                                                                                                       m7: Manifest[T7],
                                                                                                                       m8: Manifest[T8],
                                                                                                                       m9: Manifest[T9],
                                                                                                                       m10: Manifest[T10],
                                                                                                                       tag: ClassTag[R]): Unit = {
      processManifests(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10) {
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8, pm9, pm10) =>
          new StepBody(name, regex)(
            (value1: Any, value2: Any, value3: Any, value4: Any, value5: Any, value6: Any, value7: Any, value8: Any, value9: Any, value10: Any) => {
              process[R](value1, value2, value3, value4, value5, value6, value7, value8, value9, value10) {
                case List(
                actual1: T1,
                actual2: T2,
                actual3: T3,
                actual4: T4,
                actual5: T5,
                actual6: T6,
                actual7: T7,
                actual8: T8,
                actual9: T9,
                actual10: T10) => f(actual1, actual2, actual3, actual4, actual5, actual6, actual7, actual8, actual9, actual10)
              }
            })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]],
            pm8.asInstanceOf[Manifest[Any]],
            pm9.asInstanceOf[Manifest[Any]],
            pm10.asInstanceOf[Manifest[Any]])
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8, pm9, pm10, pm11) =>
          new StepBody(name, regex)(
            (
              value1: Any,
              value2: Any,
              value3: Any,
              value4: Any,
              value5: Any,
              value6: Any,
              value7: Any,
              value8: Any,
              value9: Any,
              value10: Any,
              value11: Any) => {
              process[R](value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11) {
                case List(
                actual1: T1,
                actual2: T2,
                actual3: T3,
                actual4: T4,
                actual5: T5,
                actual6: T6,
                actual7: T7,
                actual8: T8,
                actual9: T9,
                actual10: T10) => f(actual1, actual2, actual3, actual4, actual5, actual6, actual7, actual8, actual9, actual10)
              }
            })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]],
            pm8.asInstanceOf[Manifest[Any]],
            pm9.asInstanceOf[Manifest[Any]],
            pm10.asInstanceOf[Manifest[Any]],
            pm11.asInstanceOf[Manifest[Any]])
      }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R <: AnyRef](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R)(implicit
                                                                                                                                 m1: Manifest[T1],
                                                                                                                                 m2: Manifest[T2],
                                                                                                                                 m3: Manifest[T3],
                                                                                                                                 m4: Manifest[T4],
                                                                                                                                 m5: Manifest[T5],
                                                                                                                                 m6: Manifest[T6],
                                                                                                                                 m7: Manifest[T7],
                                                                                                                                 m8: Manifest[T8],
                                                                                                                                 m9: Manifest[T9],
                                                                                                                                 m10: Manifest[T10],
                                                                                                                                 m11: Manifest[T11],
                                                                                                                                 tag: ClassTag[R]): Unit = {
      processManifests(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11) {
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8, pm9, pm10, pm11) =>
          new StepBody(name, regex)(
            (
              value1: Any,
              value2: Any,
              value3: Any,
              value4: Any,
              value5: Any,
              value6: Any,
              value7: Any,
              value8: Any,
              value9: Any,
              value10: Any,
              value11: Any) => {
              process[R](value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11) {
                case List(
                actual1: T1,
                actual2: T2,
                actual3: T3,
                actual4: T4,
                actual5: T5,
                actual6: T6,
                actual7: T7,
                actual8: T8,
                actual9: T9,
                actual10: T10,
                actual11: T11) => f(actual1, actual2, actual3, actual4, actual5, actual6, actual7, actual8, actual9, actual10, actual11)
              }
            })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]],
            pm8.asInstanceOf[Manifest[Any]],
            pm9.asInstanceOf[Manifest[Any]],
            pm10.asInstanceOf[Manifest[Any]],
            pm11.asInstanceOf[Manifest[Any]])
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8, pm9, pm10, pm11, pm12) =>
          new StepBody(name, regex)(
            (
              value1: Any,
              value2: Any,
              value3: Any,
              value4: Any,
              value5: Any,
              value6: Any,
              value7: Any,
              value8: Any,
              value9: Any,
              value10: Any,
              value11: Any,
              value12: Any) => {
              process[R](value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12) {
                case List(
                actual1: T1,
                actual2: T2,
                actual3: T3,
                actual4: T4,
                actual5: T5,
                actual6: T6,
                actual7: T7,
                actual8: T8,
                actual9: T9,
                actual10: T10,
                actual11: T11) => f(actual1, actual2, actual3, actual4, actual5, actual6, actual7, actual8, actual9, actual10, actual11)
              }
            })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]],
            pm8.asInstanceOf[Manifest[Any]],
            pm9.asInstanceOf[Manifest[Any]],
            pm10.asInstanceOf[Manifest[Any]],
            pm11.asInstanceOf[Manifest[Any]],
            pm12.asInstanceOf[Manifest[Any]])
      }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R <: AnyRef](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R)(implicit
                                                                                                                                           m1: Manifest[T1],
                                                                                                                                           m2: Manifest[T2],
                                                                                                                                           m3: Manifest[T3],
                                                                                                                                           m4: Manifest[T4],
                                                                                                                                           m5: Manifest[T5],
                                                                                                                                           m6: Manifest[T6],
                                                                                                                                           m7: Manifest[T7],
                                                                                                                                           m8: Manifest[T8],
                                                                                                                                           m9: Manifest[T9],
                                                                                                                                           m10: Manifest[T10],
                                                                                                                                           m11: Manifest[T11],
                                                                                                                                           m12: Manifest[T12],
                                                                                                                                           tag: ClassTag[R]): Unit = {
      processManifests(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12) {
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8, pm9, pm10, pm11, pm12) =>
          new StepBody(name, regex)(
            (
              value1: Any,
              value2: Any,
              value3: Any,
              value4: Any,
              value5: Any,
              value6: Any,
              value7: Any,
              value8: Any,
              value9: Any,
              value10: Any,
              value11: Any,
              value12: Any) => {
              process[R](value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12) {
                case List(
                actual1: T1,
                actual2: T2,
                actual3: T3,
                actual4: T4,
                actual5: T5,
                actual6: T6,
                actual7: T7,
                actual8: T8,
                actual9: T9,
                actual10: T10,
                actual11: T11,
                actual12: T12) =>
                  f(actual1, actual2, actual3, actual4, actual5, actual6, actual7, actual8, actual9, actual10, actual11, actual12)
              }
            })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]],
            pm8.asInstanceOf[Manifest[Any]],
            pm9.asInstanceOf[Manifest[Any]],
            pm10.asInstanceOf[Manifest[Any]],
            pm11.asInstanceOf[Manifest[Any]],
            pm12.asInstanceOf[Manifest[Any]])
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8, pm9, pm10, pm11, pm12, pm13) =>
          new StepBody(name, regex)(
            (
              value1: Any,
              value2: Any,
              value3: Any,
              value4: Any,
              value5: Any,
              value6: Any,
              value7: Any,
              value8: Any,
              value9: Any,
              value10: Any,
              value11: Any,
              value12: Any,
              value13: Any) => {
              process[R](value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13) {
                case List(
                actual1: T1,
                actual2: T2,
                actual3: T3,
                actual4: T4,
                actual5: T5,
                actual6: T6,
                actual7: T7,
                actual8: T8,
                actual9: T9,
                actual10: T10,
                actual11: T11,
                actual12: T12) =>
                  f(actual1, actual2, actual3, actual4, actual5, actual6, actual7, actual8, actual9, actual10, actual11, actual12)
              }
            })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]],
            pm8.asInstanceOf[Manifest[Any]],
            pm9.asInstanceOf[Manifest[Any]],
            pm10.asInstanceOf[Manifest[Any]],
            pm11.asInstanceOf[Manifest[Any]],
            pm12.asInstanceOf[Manifest[Any]],
            pm13.asInstanceOf[Manifest[Any]])
      }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R <: AnyRef](
                                                                                    f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R)(implicit
                                                                                                                                                      m1: Manifest[T1],
                                                                                                                                                      m2: Manifest[T2],
                                                                                                                                                      m3: Manifest[T3],
                                                                                                                                                      m4: Manifest[T4],
                                                                                                                                                      m5: Manifest[T5],
                                                                                                                                                      m6: Manifest[T6],
                                                                                                                                                      m7: Manifest[T7],
                                                                                                                                                      m8: Manifest[T8],
                                                                                                                                                      m9: Manifest[T9],
                                                                                                                                                      m10: Manifest[T10],
                                                                                                                                                      m11: Manifest[T11],
                                                                                                                                                      m12: Manifest[T12],
                                                                                                                                                      m13: Manifest[T13],
                                                                                                                                                      tag: ClassTag[R]): Unit = {
      processManifests(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13) {
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8, pm9, pm10, pm11, pm12, pm13) =>
          new StepBody(name, regex)(
            (
              value1: Any,
              value2: Any,
              value3: Any,
              value4: Any,
              value5: Any,
              value6: Any,
              value7: Any,
              value8: Any,
              value9: Any,
              value10: Any,
              value11: Any,
              value12: Any,
              value13: Any) => {
              process[R](value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13) {
                case List(
                actual1: T1,
                actual2: T2,
                actual3: T3,
                actual4: T4,
                actual5: T5,
                actual6: T6,
                actual7: T7,
                actual8: T8,
                actual9: T9,
                actual10: T10,
                actual11: T11,
                actual12: T12,
                actual13: T13) =>
                  f(actual1, actual2, actual3, actual4, actual5, actual6, actual7, actual8, actual9, actual10, actual11, actual12, actual13)
              }
            })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]],
            pm8.asInstanceOf[Manifest[Any]],
            pm9.asInstanceOf[Manifest[Any]],
            pm10.asInstanceOf[Manifest[Any]],
            pm11.asInstanceOf[Manifest[Any]],
            pm12.asInstanceOf[Manifest[Any]],
            pm13.asInstanceOf[Manifest[Any]])
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8, pm9, pm10, pm11, pm12, pm13, pm14) =>
          new StepBody(name, regex)(
            (
              value1: Any,
              value2: Any,
              value3: Any,
              value4: Any,
              value5: Any,
              value6: Any,
              value7: Any,
              value8: Any,
              value9: Any,
              value10: Any,
              value11: Any,
              value12: Any,
              value13: Any,
              value14: Any) => {
              process[R](value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13, value14) {
                case List(
                actual1: T1,
                actual2: T2,
                actual3: T3,
                actual4: T4,
                actual5: T5,
                actual6: T6,
                actual7: T7,
                actual8: T8,
                actual9: T9,
                actual10: T10,
                actual11: T11,
                actual12: T12,
                actual13: T13) =>
                  f(actual1, actual2, actual3, actual4, actual5, actual6, actual7, actual8, actual9, actual10, actual11, actual12, actual13)
              }
            })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]],
            pm8.asInstanceOf[Manifest[Any]],
            pm9.asInstanceOf[Manifest[Any]],
            pm10.asInstanceOf[Manifest[Any]],
            pm11.asInstanceOf[Manifest[Any]],
            pm12.asInstanceOf[Manifest[Any]],
            pm13.asInstanceOf[Manifest[Any]],
            pm14.asInstanceOf[Manifest[Any]])
      }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R <: AnyRef](
                                                                                         f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R)(implicit
                                                                                                                                                                m1: Manifest[T1],
                                                                                                                                                                m2: Manifest[T2],
                                                                                                                                                                m3: Manifest[T3],
                                                                                                                                                                m4: Manifest[T4],
                                                                                                                                                                m5: Manifest[T5],
                                                                                                                                                                m6: Manifest[T6],
                                                                                                                                                                m7: Manifest[T7],
                                                                                                                                                                m8: Manifest[T8],
                                                                                                                                                                m9: Manifest[T9],
                                                                                                                                                                m10: Manifest[T10],
                                                                                                                                                                m11: Manifest[T11],
                                                                                                                                                                m12: Manifest[T12],
                                                                                                                                                                m13: Manifest[T13],
                                                                                                                                                                m14: Manifest[T14],
                                                                                                                                                                tag: ClassTag[R]): Unit = {
      processManifests(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14) {
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8, pm9, pm10, pm11, pm12, pm13, pm14) =>
          new StepBody(name, regex)(
            (
              value1: Any,
              value2: Any,
              value3: Any,
              value4: Any,
              value5: Any,
              value6: Any,
              value7: Any,
              value8: Any,
              value9: Any,
              value10: Any,
              value11: Any,
              value12: Any,
              value13: Any,
              value14: Any) => {
              process[R](value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13, value14) {
                case List(
                actual1: T1,
                actual2: T2,
                actual3: T3,
                actual4: T4,
                actual5: T5,
                actual6: T6,
                actual7: T7,
                actual8: T8,
                actual9: T9,
                actual10: T10,
                actual11: T11,
                actual12: T12,
                actual13: T13,
                actual14: T14) =>
                  f(actual1, actual2, actual3, actual4, actual5, actual6, actual7, actual8, actual9, actual10, actual11, actual12, actual13, actual14)
              }
            })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]],
            pm8.asInstanceOf[Manifest[Any]],
            pm9.asInstanceOf[Manifest[Any]],
            pm10.asInstanceOf[Manifest[Any]],
            pm11.asInstanceOf[Manifest[Any]],
            pm12.asInstanceOf[Manifest[Any]],
            pm13.asInstanceOf[Manifest[Any]],
            pm14.asInstanceOf[Manifest[Any]])
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8, pm9, pm10, pm11, pm12, pm13, pm14, pm15) =>
          new StepBody(name, regex)(
            (
              value1: Any,
              value2: Any,
              value3: Any,
              value4: Any,
              value5: Any,
              value6: Any,
              value7: Any,
              value8: Any,
              value9: Any,
              value10: Any,
              value11: Any,
              value12: Any,
              value13: Any,
              value14: Any,
              value15: Any) => {
              process[R](
                value1,
                value2,
                value3,
                value4,
                value5,
                value6,
                value7,
                value8,
                value9,
                value10,
                value11,
                value12,
                value13,
                value14,
                value15) {
                case List(
                actual1: T1,
                actual2: T2,
                actual3: T3,
                actual4: T4,
                actual5: T5,
                actual6: T6,
                actual7: T7,
                actual8: T8,
                actual9: T9,
                actual10: T10,
                actual11: T11,
                actual12: T12,
                actual13: T13,
                actual14: T14) =>
                  f(actual1, actual2, actual3, actual4, actual5, actual6, actual7, actual8, actual9, actual10, actual11, actual12, actual13, actual14)
              }
            })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]],
            pm8.asInstanceOf[Manifest[Any]],
            pm9.asInstanceOf[Manifest[Any]],
            pm10.asInstanceOf[Manifest[Any]],
            pm11.asInstanceOf[Manifest[Any]],
            pm12.asInstanceOf[Manifest[Any]],
            pm13.asInstanceOf[Manifest[Any]],
            pm14.asInstanceOf[Manifest[Any]],
            pm15.asInstanceOf[Manifest[Any]])
      }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R <: AnyRef](
                                                                                              f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R)(implicit
                                                                                                                                                                          m1: Manifest[T1],
                                                                                                                                                                          m2: Manifest[T2],
                                                                                                                                                                          m3: Manifest[T3],
                                                                                                                                                                          m4: Manifest[T4],
                                                                                                                                                                          m5: Manifest[T5],
                                                                                                                                                                          m6: Manifest[T6],
                                                                                                                                                                          m7: Manifest[T7],
                                                                                                                                                                          m8: Manifest[T8],
                                                                                                                                                                          m9: Manifest[T9],
                                                                                                                                                                          m10: Manifest[T10],
                                                                                                                                                                          m11: Manifest[T11],
                                                                                                                                                                          m12: Manifest[T12],
                                                                                                                                                                          m13: Manifest[T13],
                                                                                                                                                                          m14: Manifest[T14],
                                                                                                                                                                          m15: Manifest[T15],
                                                                                                                                                                          tag: ClassTag[R]): Unit = {
      processManifests(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15) {
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8, pm9, pm10, pm11, pm12, pm13, pm14, pm15) =>
          new StepBody(name, regex)(
            (
              value1: Any,
              value2: Any,
              value3: Any,
              value4: Any,
              value5: Any,
              value6: Any,
              value7: Any,
              value8: Any,
              value9: Any,
              value10: Any,
              value11: Any,
              value12: Any,
              value13: Any,
              value14: Any,
              value15: Any) => {
              process[R](
                value1,
                value2,
                value3,
                value4,
                value5,
                value6,
                value7,
                value8,
                value9,
                value10,
                value11,
                value12,
                value13,
                value14,
                value15) {
                case List(
                actual1: T1,
                actual2: T2,
                actual3: T3,
                actual4: T4,
                actual5: T5,
                actual6: T6,
                actual7: T7,
                actual8: T8,
                actual9: T9,
                actual10: T10,
                actual11: T11,
                actual12: T12,
                actual13: T13,
                actual14: T14,
                actual15: T15) => f(
                  actual1,
                  actual2,
                  actual3,
                  actual4,
                  actual5,
                  actual6,
                  actual7,
                  actual8,
                  actual9,
                  actual10,
                  actual11,
                  actual12,
                  actual13,
                  actual14,
                  actual15)
              }
            })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]],
            pm8.asInstanceOf[Manifest[Any]],
            pm9.asInstanceOf[Manifest[Any]],
            pm10.asInstanceOf[Manifest[Any]],
            pm11.asInstanceOf[Manifest[Any]],
            pm12.asInstanceOf[Manifest[Any]],
            pm13.asInstanceOf[Manifest[Any]],
            pm14.asInstanceOf[Manifest[Any]],
            pm15.asInstanceOf[Manifest[Any]])
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8, pm9, pm10, pm11, pm12, pm13, pm14, pm15, pm16) =>
          new StepBody(name, regex)(
            (
              value1: Any,
              value2: Any,
              value3: Any,
              value4: Any,
              value5: Any,
              value6: Any,
              value7: Any,
              value8: Any,
              value9: Any,
              value10: Any,
              value11: Any,
              value12: Any,
              value13: Any,
              value14: Any,
              value15: Any,
              value16: Any) => {
              process[R](
                value1,
                value2,
                value3,
                value4,
                value5,
                value6,
                value7,
                value8,
                value9,
                value10,
                value11,
                value12,
                value13,
                value14,
                value15,
                value16) {
                case List(
                actual1: T1,
                actual2: T2,
                actual3: T3,
                actual4: T4,
                actual5: T5,
                actual6: T6,
                actual7: T7,
                actual8: T8,
                actual9: T9,
                actual10: T10,
                actual11: T11,
                actual12: T12,
                actual13: T13,
                actual14: T14,
                actual15: T15) => f(
                  actual1,
                  actual2,
                  actual3,
                  actual4,
                  actual5,
                  actual6,
                  actual7,
                  actual8,
                  actual9,
                  actual10,
                  actual11,
                  actual12,
                  actual13,
                  actual14,
                  actual15)
              }
            })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]],
            pm8.asInstanceOf[Manifest[Any]],
            pm9.asInstanceOf[Manifest[Any]],
            pm10.asInstanceOf[Manifest[Any]],
            pm11.asInstanceOf[Manifest[Any]],
            pm12.asInstanceOf[Manifest[Any]],
            pm13.asInstanceOf[Manifest[Any]],
            pm14.asInstanceOf[Manifest[Any]],
            pm15.asInstanceOf[Manifest[Any]],
            pm16.asInstanceOf[Manifest[Any]])
      }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R <: AnyRef](
                                                                                                   f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R)(implicit
                                                                                                                                                                                    m1: Manifest[T1],
                                                                                                                                                                                    m2: Manifest[T2],
                                                                                                                                                                                    m3: Manifest[T3],
                                                                                                                                                                                    m4: Manifest[T4],
                                                                                                                                                                                    m5: Manifest[T5],
                                                                                                                                                                                    m6: Manifest[T6],
                                                                                                                                                                                    m7: Manifest[T7],
                                                                                                                                                                                    m8: Manifest[T8],
                                                                                                                                                                                    m9: Manifest[T9],
                                                                                                                                                                                    m10: Manifest[T10],
                                                                                                                                                                                    m11: Manifest[T11],
                                                                                                                                                                                    m12: Manifest[T12],
                                                                                                                                                                                    m13: Manifest[T13],
                                                                                                                                                                                    m14: Manifest[T14],
                                                                                                                                                                                    m15: Manifest[T15],
                                                                                                                                                                                    m16: Manifest[T16],
                                                                                                                                                                                    tag: ClassTag[R]): Unit = {
      processManifests(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16) {
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8, pm9, pm10, pm11, pm12, pm13, pm14, pm15, pm16) =>
          new StepBody(name, regex)(
            (
              value1: Any,
              value2: Any,
              value3: Any,
              value4: Any,
              value5: Any,
              value6: Any,
              value7: Any,
              value8: Any,
              value9: Any,
              value10: Any,
              value11: Any,
              value12: Any,
              value13: Any,
              value14: Any,
              value15: Any,
              value16: Any) => {
              process[R](
                value1,
                value2,
                value3,
                value4,
                value5,
                value6,
                value7,
                value8,
                value9,
                value10,
                value11,
                value12,
                value13,
                value14,
                value15,
                value16) {
                case List(
                actual1: T1,
                actual2: T2,
                actual3: T3,
                actual4: T4,
                actual5: T5,
                actual6: T6,
                actual7: T7,
                actual8: T8,
                actual9: T9,
                actual10: T10,
                actual11: T11,
                actual12: T12,
                actual13: T13,
                actual14: T14,
                actual15: T15,
                actual16: T16) => f(
                  actual1,
                  actual2,
                  actual3,
                  actual4,
                  actual5,
                  actual6,
                  actual7,
                  actual8,
                  actual9,
                  actual10,
                  actual11,
                  actual12,
                  actual13,
                  actual14,
                  actual15,
                  actual16)
              }
            })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]],
            pm8.asInstanceOf[Manifest[Any]],
            pm9.asInstanceOf[Manifest[Any]],
            pm10.asInstanceOf[Manifest[Any]],
            pm11.asInstanceOf[Manifest[Any]],
            pm12.asInstanceOf[Manifest[Any]],
            pm13.asInstanceOf[Manifest[Any]],
            pm14.asInstanceOf[Manifest[Any]],
            pm15.asInstanceOf[Manifest[Any]],
            pm16.asInstanceOf[Manifest[Any]])
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8, pm9, pm10, pm11, pm12, pm13, pm14, pm15, pm16, pm17) =>
          new StepBody(name, regex)(
            (
              value1: Any,
              value2: Any,
              value3: Any,
              value4: Any,
              value5: Any,
              value6: Any,
              value7: Any,
              value8: Any,
              value9: Any,
              value10: Any,
              value11: Any,
              value12: Any,
              value13: Any,
              value14: Any,
              value15: Any,
              value16: Any,
              value17: Any) => {
              process[R](
                value1,
                value2,
                value3,
                value4,
                value5,
                value6,
                value7,
                value8,
                value9,
                value10,
                value11,
                value12,
                value13,
                value14,
                value15,
                value16,
                value17) {
                case List(
                actual1: T1,
                actual2: T2,
                actual3: T3,
                actual4: T4,
                actual5: T5,
                actual6: T6,
                actual7: T7,
                actual8: T8,
                actual9: T9,
                actual10: T10,
                actual11: T11,
                actual12: T12,
                actual13: T13,
                actual14: T14,
                actual15: T15,
                actual16: T16) => f(
                  actual1,
                  actual2,
                  actual3,
                  actual4,
                  actual5,
                  actual6,
                  actual7,
                  actual8,
                  actual9,
                  actual10,
                  actual11,
                  actual12,
                  actual13,
                  actual14,
                  actual15,
                  actual16)
              }
            })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]],
            pm8.asInstanceOf[Manifest[Any]],
            pm9.asInstanceOf[Manifest[Any]],
            pm10.asInstanceOf[Manifest[Any]],
            pm11.asInstanceOf[Manifest[Any]],
            pm12.asInstanceOf[Manifest[Any]],
            pm13.asInstanceOf[Manifest[Any]],
            pm14.asInstanceOf[Manifest[Any]],
            pm15.asInstanceOf[Manifest[Any]],
            pm16.asInstanceOf[Manifest[Any]],
            pm17.asInstanceOf[Manifest[Any]])
      }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R <: AnyRef](
                                                                                                        f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R)(implicit
                                                                                                                                                                                              m1: Manifest[T1],
                                                                                                                                                                                              m2: Manifest[T2],
                                                                                                                                                                                              m3: Manifest[T3],
                                                                                                                                                                                              m4: Manifest[T4],
                                                                                                                                                                                              m5: Manifest[T5],
                                                                                                                                                                                              m6: Manifest[T6],
                                                                                                                                                                                              m7: Manifest[T7],
                                                                                                                                                                                              m8: Manifest[T8],
                                                                                                                                                                                              m9: Manifest[T9],
                                                                                                                                                                                              m10: Manifest[T10],
                                                                                                                                                                                              m11: Manifest[T11],
                                                                                                                                                                                              m12: Manifest[T12],
                                                                                                                                                                                              m13: Manifest[T13],
                                                                                                                                                                                              m14: Manifest[T14],
                                                                                                                                                                                              m15: Manifest[T15],
                                                                                                                                                                                              m16: Manifest[T16],
                                                                                                                                                                                              m17: Manifest[T17],
                                                                                                                                                                                              tag: ClassTag[R]): Unit = {
      processManifests(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17) {
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8, pm9, pm10, pm11, pm12, pm13, pm14, pm15, pm16, pm17) =>
          new StepBody(name, regex)(
            (
              value1: Any,
              value2: Any,
              value3: Any,
              value4: Any,
              value5: Any,
              value6: Any,
              value7: Any,
              value8: Any,
              value9: Any,
              value10: Any,
              value11: Any,
              value12: Any,
              value13: Any,
              value14: Any,
              value15: Any,
              value16: Any,
              value17: Any) => {
              process[R](
                value1,
                value2,
                value3,
                value4,
                value5,
                value6,
                value7,
                value8,
                value9,
                value10,
                value11,
                value12,
                value13,
                value14,
                value15,
                value16,
                value17) {
                case List(
                actual1: T1,
                actual2: T2,
                actual3: T3,
                actual4: T4,
                actual5: T5,
                actual6: T6,
                actual7: T7,
                actual8: T8,
                actual9: T9,
                actual10: T10,
                actual11: T11,
                actual12: T12,
                actual13: T13,
                actual14: T14,
                actual15: T15,
                actual16: T16,
                actual17: T17) => f(
                  actual1,
                  actual2,
                  actual3,
                  actual4,
                  actual5,
                  actual6,
                  actual7,
                  actual8,
                  actual9,
                  actual10,
                  actual11,
                  actual12,
                  actual13,
                  actual14,
                  actual15,
                  actual16,
                  actual17)
              }
            })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]],
            pm8.asInstanceOf[Manifest[Any]],
            pm9.asInstanceOf[Manifest[Any]],
            pm10.asInstanceOf[Manifest[Any]],
            pm11.asInstanceOf[Manifest[Any]],
            pm12.asInstanceOf[Manifest[Any]],
            pm13.asInstanceOf[Manifest[Any]],
            pm14.asInstanceOf[Manifest[Any]],
            pm15.asInstanceOf[Manifest[Any]],
            pm16.asInstanceOf[Manifest[Any]],
            pm17.asInstanceOf[Manifest[Any]])
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8, pm9, pm10, pm11, pm12, pm13, pm14, pm15, pm16, pm17, pm18) =>
          new StepBody(name, regex)(
            (
              value1: Any,
              value2: Any,
              value3: Any,
              value4: Any,
              value5: Any,
              value6: Any,
              value7: Any,
              value8: Any,
              value9: Any,
              value10: Any,
              value11: Any,
              value12: Any,
              value13: Any,
              value14: Any,
              value15: Any,
              value16: Any,
              value17: Any,
              value18: Any) => {
              process[R](
                value1,
                value2,
                value3,
                value4,
                value5,
                value6,
                value7,
                value8,
                value9,
                value10,
                value11,
                value12,
                value13,
                value14,
                value15,
                value16,
                value17,
                value18) {
                case List(
                actual1: T1,
                actual2: T2,
                actual3: T3,
                actual4: T4,
                actual5: T5,
                actual6: T6,
                actual7: T7,
                actual8: T8,
                actual9: T9,
                actual10: T10,
                actual11: T11,
                actual12: T12,
                actual13: T13,
                actual14: T14,
                actual15: T15,
                actual16: T16,
                actual17: T17) => f(
                  actual1,
                  actual2,
                  actual3,
                  actual4,
                  actual5,
                  actual6,
                  actual7,
                  actual8,
                  actual9,
                  actual10,
                  actual11,
                  actual12,
                  actual13,
                  actual14,
                  actual15,
                  actual16,
                  actual17)
              }
            })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]],
            pm8.asInstanceOf[Manifest[Any]],
            pm9.asInstanceOf[Manifest[Any]],
            pm10.asInstanceOf[Manifest[Any]],
            pm11.asInstanceOf[Manifest[Any]],
            pm12.asInstanceOf[Manifest[Any]],
            pm13.asInstanceOf[Manifest[Any]],
            pm14.asInstanceOf[Manifest[Any]],
            pm15.asInstanceOf[Manifest[Any]],
            pm16.asInstanceOf[Manifest[Any]],
            pm17.asInstanceOf[Manifest[Any]],
            pm18.asInstanceOf[Manifest[Any]])
      }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R <: AnyRef](
                                                                                                             f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R)(implicit
                                                                                                                                                                                                        m1: Manifest[T1],
                                                                                                                                                                                                        m2: Manifest[T2],
                                                                                                                                                                                                        m3: Manifest[T3],
                                                                                                                                                                                                        m4: Manifest[T4],
                                                                                                                                                                                                        m5: Manifest[T5],
                                                                                                                                                                                                        m6: Manifest[T6],
                                                                                                                                                                                                        m7: Manifest[T7],
                                                                                                                                                                                                        m8: Manifest[T8],
                                                                                                                                                                                                        m9: Manifest[T9],
                                                                                                                                                                                                        m10: Manifest[T10],
                                                                                                                                                                                                        m11: Manifest[T11],
                                                                                                                                                                                                        m12: Manifest[T12],
                                                                                                                                                                                                        m13: Manifest[T13],
                                                                                                                                                                                                        m14: Manifest[T14],
                                                                                                                                                                                                        m15: Manifest[T15],
                                                                                                                                                                                                        m16: Manifest[T16],
                                                                                                                                                                                                        m17: Manifest[T17],
                                                                                                                                                                                                        m18: Manifest[T18],
                                                                                                                                                                                                        tag: ClassTag[R]): Unit = {
      processManifests(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18) {
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8, pm9, pm10, pm11, pm12, pm13, pm14, pm15, pm16, pm17, pm18) =>
          new StepBody(name, regex)(
            (
              value1: Any,
              value2: Any,
              value3: Any,
              value4: Any,
              value5: Any,
              value6: Any,
              value7: Any,
              value8: Any,
              value9: Any,
              value10: Any,
              value11: Any,
              value12: Any,
              value13: Any,
              value14: Any,
              value15: Any,
              value16: Any,
              value17: Any,
              value18: Any) => {
              process[R](
                value1,
                value2,
                value3,
                value4,
                value5,
                value6,
                value7,
                value8,
                value9,
                value10,
                value11,
                value12,
                value13,
                value14,
                value15,
                value16,
                value17,
                value18) {
                case List(
                actual1: T1,
                actual2: T2,
                actual3: T3,
                actual4: T4,
                actual5: T5,
                actual6: T6,
                actual7: T7,
                actual8: T8,
                actual9: T9,
                actual10: T10,
                actual11: T11,
                actual12: T12,
                actual13: T13,
                actual14: T14,
                actual15: T15,
                actual16: T16,
                actual17: T17,
                actual18: T18) => f(
                  actual1,
                  actual2,
                  actual3,
                  actual4,
                  actual5,
                  actual6,
                  actual7,
                  actual8,
                  actual9,
                  actual10,
                  actual11,
                  actual12,
                  actual13,
                  actual14,
                  actual15,
                  actual16,
                  actual17,
                  actual18
                )
              }
            })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]],
            pm8.asInstanceOf[Manifest[Any]],
            pm9.asInstanceOf[Manifest[Any]],
            pm10.asInstanceOf[Manifest[Any]],
            pm11.asInstanceOf[Manifest[Any]],
            pm12.asInstanceOf[Manifest[Any]],
            pm13.asInstanceOf[Manifest[Any]],
            pm14.asInstanceOf[Manifest[Any]],
            pm15.asInstanceOf[Manifest[Any]],
            pm16.asInstanceOf[Manifest[Any]],
            pm17.asInstanceOf[Manifest[Any]],
            pm18.asInstanceOf[Manifest[Any]])
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8, pm9, pm10, pm11, pm12, pm13, pm14, pm15, pm16, pm17, pm18, pm19) =>
          new StepBody(name, regex)(
            (
              value1: Any,
              value2: Any,
              value3: Any,
              value4: Any,
              value5: Any,
              value6: Any,
              value7: Any,
              value8: Any,
              value9: Any,
              value10: Any,
              value11: Any,
              value12: Any,
              value13: Any,
              value14: Any,
              value15: Any,
              value16: Any,
              value17: Any,
              value18: Any,
              value19: Any) => {
              process[R](
                value1,
                value2,
                value3,
                value4,
                value5,
                value6,
                value7,
                value8,
                value9,
                value10,
                value11,
                value12,
                value13,
                value14,
                value15,
                value16,
                value17,
                value18,
                value19) {
                case List(
                actual1: T1,
                actual2: T2,
                actual3: T3,
                actual4: T4,
                actual5: T5,
                actual6: T6,
                actual7: T7,
                actual8: T8,
                actual9: T9,
                actual10: T10,
                actual11: T11,
                actual12: T12,
                actual13: T13,
                actual14: T14,
                actual15: T15,
                actual16: T16,
                actual17: T17,
                actual18: T18) => f(
                  actual1,
                  actual2,
                  actual3,
                  actual4,
                  actual5,
                  actual6,
                  actual7,
                  actual8,
                  actual9,
                  actual10,
                  actual11,
                  actual12,
                  actual13,
                  actual14,
                  actual15,
                  actual16,
                  actual17,
                  actual18
                )
              }
            })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]],
            pm8.asInstanceOf[Manifest[Any]],
            pm9.asInstanceOf[Manifest[Any]],
            pm10.asInstanceOf[Manifest[Any]],
            pm11.asInstanceOf[Manifest[Any]],
            pm12.asInstanceOf[Manifest[Any]],
            pm13.asInstanceOf[Manifest[Any]],
            pm14.asInstanceOf[Manifest[Any]],
            pm15.asInstanceOf[Manifest[Any]],
            pm16.asInstanceOf[Manifest[Any]],
            pm17.asInstanceOf[Manifest[Any]],
            pm18.asInstanceOf[Manifest[Any]],
            pm19.asInstanceOf[Manifest[Any]])
      }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R <: AnyRef](
                                                                                                                  f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R)(implicit
                                                                                                                                                                                                                  m1: Manifest[T1],
                                                                                                                                                                                                                  m2: Manifest[T2],
                                                                                                                                                                                                                  m3: Manifest[T3],
                                                                                                                                                                                                                  m4: Manifest[T4],
                                                                                                                                                                                                                  m5: Manifest[T5],
                                                                                                                                                                                                                  m6: Manifest[T6],
                                                                                                                                                                                                                  m7: Manifest[T7],
                                                                                                                                                                                                                  m8: Manifest[T8],
                                                                                                                                                                                                                  m9: Manifest[T9],
                                                                                                                                                                                                                  m10: Manifest[T10],
                                                                                                                                                                                                                  m11: Manifest[T11],
                                                                                                                                                                                                                  m12: Manifest[T12],
                                                                                                                                                                                                                  m13: Manifest[T13],
                                                                                                                                                                                                                  m14: Manifest[T14],
                                                                                                                                                                                                                  m15: Manifest[T15],
                                                                                                                                                                                                                  m16: Manifest[T16],
                                                                                                                                                                                                                  m17: Manifest[T17],
                                                                                                                                                                                                                  m18: Manifest[T18],
                                                                                                                                                                                                                  m19: Manifest[T19],
                                                                                                                                                                                                                  tag: ClassTag[R]): Unit = {
      processManifests(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19) {
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8, pm9, pm10, pm11, pm12, pm13, pm14, pm15, pm16, pm17, pm18, pm19) =>
          new StepBody(name, regex)(
            (
              value1: Any,
              value2: Any,
              value3: Any,
              value4: Any,
              value5: Any,
              value6: Any,
              value7: Any,
              value8: Any,
              value9: Any,
              value10: Any,
              value11: Any,
              value12: Any,
              value13: Any,
              value14: Any,
              value15: Any,
              value16: Any,
              value17: Any,
              value18: Any,
              value19: Any) => {
              process[R](
                value1,
                value2,
                value3,
                value4,
                value5,
                value6,
                value7,
                value8,
                value9,
                value10,
                value11,
                value12,
                value13,
                value14,
                value15,
                value16,
                value17,
                value18,
                value19) {
                case List(
                actual1: T1,
                actual2: T2,
                actual3: T3,
                actual4: T4,
                actual5: T5,
                actual6: T6,
                actual7: T7,
                actual8: T8,
                actual9: T9,
                actual10: T10,
                actual11: T11,
                actual12: T12,
                actual13: T13,
                actual14: T14,
                actual15: T15,
                actual16: T16,
                actual17: T17,
                actual18: T18,
                actual19: T19) => f(
                  actual1,
                  actual2,
                  actual3,
                  actual4,
                  actual5,
                  actual6,
                  actual7,
                  actual8,
                  actual9,
                  actual10,
                  actual11,
                  actual12,
                  actual13,
                  actual14,
                  actual15,
                  actual16,
                  actual17,
                  actual18,
                  actual19
                )
              }
            })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]],
            pm8.asInstanceOf[Manifest[Any]],
            pm9.asInstanceOf[Manifest[Any]],
            pm10.asInstanceOf[Manifest[Any]],
            pm11.asInstanceOf[Manifest[Any]],
            pm12.asInstanceOf[Manifest[Any]],
            pm13.asInstanceOf[Manifest[Any]],
            pm14.asInstanceOf[Manifest[Any]],
            pm15.asInstanceOf[Manifest[Any]],
            pm16.asInstanceOf[Manifest[Any]],
            pm17.asInstanceOf[Manifest[Any]],
            pm18.asInstanceOf[Manifest[Any]],
            pm19.asInstanceOf[Manifest[Any]])
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8, pm9, pm10, pm11, pm12, pm13, pm14, pm15, pm16, pm17, pm18, pm19, pm20) =>
          new StepBody(name, regex)(
            (
              value1: Any,
              value2: Any,
              value3: Any,
              value4: Any,
              value5: Any,
              value6: Any,
              value7: Any,
              value8: Any,
              value9: Any,
              value10: Any,
              value11: Any,
              value12: Any,
              value13: Any,
              value14: Any,
              value15: Any,
              value16: Any,
              value17: Any,
              value18: Any,
              value19: Any,
              value20: Any) => {
              process[R](
                value1,
                value2,
                value3,
                value4,
                value5,
                value6,
                value7,
                value8,
                value9,
                value10,
                value11,
                value12,
                value13,
                value14,
                value15,
                value16,
                value17,
                value18,
                value19,
                value20
              ) {
                case List(
                actual1: T1,
                actual2: T2,
                actual3: T3,
                actual4: T4,
                actual5: T5,
                actual6: T6,
                actual7: T7,
                actual8: T8,
                actual9: T9,
                actual10: T10,
                actual11: T11,
                actual12: T12,
                actual13: T13,
                actual14: T14,
                actual15: T15,
                actual16: T16,
                actual17: T17,
                actual18: T18,
                actual19: T19) => f(
                  actual1,
                  actual2,
                  actual3,
                  actual4,
                  actual5,
                  actual6,
                  actual7,
                  actual8,
                  actual9,
                  actual10,
                  actual11,
                  actual12,
                  actual13,
                  actual14,
                  actual15,
                  actual16,
                  actual17,
                  actual18,
                  actual19
                )
              }
            })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]],
            pm8.asInstanceOf[Manifest[Any]],
            pm9.asInstanceOf[Manifest[Any]],
            pm10.asInstanceOf[Manifest[Any]],
            pm11.asInstanceOf[Manifest[Any]],
            pm12.asInstanceOf[Manifest[Any]],
            pm13.asInstanceOf[Manifest[Any]],
            pm14.asInstanceOf[Manifest[Any]],
            pm15.asInstanceOf[Manifest[Any]],
            pm16.asInstanceOf[Manifest[Any]],
            pm17.asInstanceOf[Manifest[Any]],
            pm18.asInstanceOf[Manifest[Any]],
            pm19.asInstanceOf[Manifest[Any]],
            pm20.asInstanceOf[Manifest[Any]])
      }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R <: AnyRef](
                                                                                                                       f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R)(implicit
                                                                                                                                                                                                                            m1: Manifest[T1],
                                                                                                                                                                                                                            m2: Manifest[T2],
                                                                                                                                                                                                                            m3: Manifest[T3],
                                                                                                                                                                                                                            m4: Manifest[T4],
                                                                                                                                                                                                                            m5: Manifest[T5],
                                                                                                                                                                                                                            m6: Manifest[T6],
                                                                                                                                                                                                                            m7: Manifest[T7],
                                                                                                                                                                                                                            m8: Manifest[T8],
                                                                                                                                                                                                                            m9: Manifest[T9],
                                                                                                                                                                                                                            m10: Manifest[T10],
                                                                                                                                                                                                                            m11: Manifest[T11],
                                                                                                                                                                                                                            m12: Manifest[T12],
                                                                                                                                                                                                                            m13: Manifest[T13],
                                                                                                                                                                                                                            m14: Manifest[T14],
                                                                                                                                                                                                                            m15: Manifest[T15],
                                                                                                                                                                                                                            m16: Manifest[T16],
                                                                                                                                                                                                                            m17: Manifest[T17],
                                                                                                                                                                                                                            m18: Manifest[T18],
                                                                                                                                                                                                                            m19: Manifest[T19],
                                                                                                                                                                                                                            m20: Manifest[T20],
                                                                                                                                                                                                                            tag: ClassTag[R]): Unit = {
      processManifests(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20) {
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8, pm9, pm10, pm11, pm12, pm13, pm14, pm15, pm16, pm17, pm18, pm19, pm20) =>
          new StepBody(name, regex)(
            (
              value1: Any,
              value2: Any,
              value3: Any,
              value4: Any,
              value5: Any,
              value6: Any,
              value7: Any,
              value8: Any,
              value9: Any,
              value10: Any,
              value11: Any,
              value12: Any,
              value13: Any,
              value14: Any,
              value15: Any,
              value16: Any,
              value17: Any,
              value18: Any,
              value19: Any,
              value20: Any) => {
              process[R](
                value1,
                value2,
                value3,
                value4,
                value5,
                value6,
                value7,
                value8,
                value9,
                value10,
                value11,
                value12,
                value13,
                value14,
                value15,
                value16,
                value17,
                value18,
                value19,
                value20
              ) {
                case List(
                actual1: T1,
                actual2: T2,
                actual3: T3,
                actual4: T4,
                actual5: T5,
                actual6: T6,
                actual7: T7,
                actual8: T8,
                actual9: T9,
                actual10: T10,
                actual11: T11,
                actual12: T12,
                actual13: T13,
                actual14: T14,
                actual15: T15,
                actual16: T16,
                actual17: T17,
                actual18: T18,
                actual19: T19,
                actual20: T20) => f(
                  actual1,
                  actual2,
                  actual3,
                  actual4,
                  actual5,
                  actual6,
                  actual7,
                  actual8,
                  actual9,
                  actual10,
                  actual11,
                  actual12,
                  actual13,
                  actual14,
                  actual15,
                  actual16,
                  actual17,
                  actual18,
                  actual19,
                  actual20
                )
              }
            })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]],
            pm8.asInstanceOf[Manifest[Any]],
            pm9.asInstanceOf[Manifest[Any]],
            pm10.asInstanceOf[Manifest[Any]],
            pm11.asInstanceOf[Manifest[Any]],
            pm12.asInstanceOf[Manifest[Any]],
            pm13.asInstanceOf[Manifest[Any]],
            pm14.asInstanceOf[Manifest[Any]],
            pm15.asInstanceOf[Manifest[Any]],
            pm16.asInstanceOf[Manifest[Any]],
            pm17.asInstanceOf[Manifest[Any]],
            pm18.asInstanceOf[Manifest[Any]],
            pm19.asInstanceOf[Manifest[Any]],
            pm20.asInstanceOf[Manifest[Any]])
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8, pm9, pm10, pm11, pm12, pm13, pm14, pm15, pm16, pm17, pm18, pm19, pm20, pm21) =>
          new StepBody(name, regex)(
            (
              value1: Any,
              value2: Any,
              value3: Any,
              value4: Any,
              value5: Any,
              value6: Any,
              value7: Any,
              value8: Any,
              value9: Any,
              value10: Any,
              value11: Any,
              value12: Any,
              value13: Any,
              value14: Any,
              value15: Any,
              value16: Any,
              value17: Any,
              value18: Any,
              value19: Any,
              value20: Any,
              value21: Any) => {
              process[R](
                value1,
                value2,
                value3,
                value4,
                value5,
                value6,
                value7,
                value8,
                value9,
                value10,
                value11,
                value12,
                value13,
                value14,
                value15,
                value16,
                value17,
                value18,
                value19,
                value20,
                value21
              ) {
                case List(
                actual1: T1,
                actual2: T2,
                actual3: T3,
                actual4: T4,
                actual5: T5,
                actual6: T6,
                actual7: T7,
                actual8: T8,
                actual9: T9,
                actual10: T10,
                actual11: T11,
                actual12: T12,
                actual13: T13,
                actual14: T14,
                actual15: T15,
                actual16: T16,
                actual17: T17,
                actual18: T18,
                actual19: T19,
                actual20: T20) => f(
                  actual1,
                  actual2,
                  actual3,
                  actual4,
                  actual5,
                  actual6,
                  actual7,
                  actual8,
                  actual9,
                  actual10,
                  actual11,
                  actual12,
                  actual13,
                  actual14,
                  actual15,
                  actual16,
                  actual17,
                  actual18,
                  actual19,
                  actual20
                )
              }
            })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]],
            pm8.asInstanceOf[Manifest[Any]],
            pm9.asInstanceOf[Manifest[Any]],
            pm10.asInstanceOf[Manifest[Any]],
            pm11.asInstanceOf[Manifest[Any]],
            pm12.asInstanceOf[Manifest[Any]],
            pm13.asInstanceOf[Manifest[Any]],
            pm14.asInstanceOf[Manifest[Any]],
            pm15.asInstanceOf[Manifest[Any]],
            pm16.asInstanceOf[Manifest[Any]],
            pm17.asInstanceOf[Manifest[Any]],
            pm18.asInstanceOf[Manifest[Any]],
            pm19.asInstanceOf[Manifest[Any]],
            pm20.asInstanceOf[Manifest[Any]],
            pm21.asInstanceOf[Manifest[Any]])
      }
    }

    def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R <: AnyRef](
                                                                                                                            f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R)(implicit
                                                                                                                                                                                                                                      m1: Manifest[T1],
                                                                                                                                                                                                                                      m2: Manifest[T2],
                                                                                                                                                                                                                                      m3: Manifest[T3],
                                                                                                                                                                                                                                      m4: Manifest[T4],
                                                                                                                                                                                                                                      m5: Manifest[T5],
                                                                                                                                                                                                                                      m6: Manifest[T6],
                                                                                                                                                                                                                                      m7: Manifest[T7],
                                                                                                                                                                                                                                      m8: Manifest[T8],
                                                                                                                                                                                                                                      m9: Manifest[T9],
                                                                                                                                                                                                                                      m10: Manifest[T10],
                                                                                                                                                                                                                                      m11: Manifest[T11],
                                                                                                                                                                                                                                      m12: Manifest[T12],
                                                                                                                                                                                                                                      m13: Manifest[T13],
                                                                                                                                                                                                                                      m14: Manifest[T14],
                                                                                                                                                                                                                                      m15: Manifest[T15],
                                                                                                                                                                                                                                      m16: Manifest[T16],
                                                                                                                                                                                                                                      m17: Manifest[T17],
                                                                                                                                                                                                                                      m18: Manifest[T18],
                                                                                                                                                                                                                                      m19: Manifest[T19],
                                                                                                                                                                                                                                      m20: Manifest[T20],
                                                                                                                                                                                                                                      m21: Manifest[T21],
                                                                                                                                                                                                                                      tag: ClassTag[R]): Unit = {
      processManifests(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20, m21) {
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8, pm9, pm10, pm11, pm12, pm13, pm14, pm15, pm16, pm17, pm18, pm19, pm20, pm21) =>
          new StepBody(name, regex)(
            (
              value1: Any,
              value2: Any,
              value3: Any,
              value4: Any,
              value5: Any,
              value6: Any,
              value7: Any,
              value8: Any,
              value9: Any,
              value10: Any,
              value11: Any,
              value12: Any,
              value13: Any,
              value14: Any,
              value15: Any,
              value16: Any,
              value17: Any,
              value18: Any,
              value19: Any,
              value20: Any,
              value21: Any) => {
              process[R](
                value1,
                value2,
                value3,
                value4,
                value5,
                value6,
                value7,
                value8,
                value9,
                value10,
                value11,
                value12,
                value13,
                value14,
                value15,
                value16,
                value17,
                value18,
                value19,
                value20,
                value21
              ) {
                case List(
                actual1: T1,
                actual2: T2,
                actual3: T3,
                actual4: T4,
                actual5: T5,
                actual6: T6,
                actual7: T7,
                actual8: T8,
                actual9: T9,
                actual10: T10,
                actual11: T11,
                actual12: T12,
                actual13: T13,
                actual14: T14,
                actual15: T15,
                actual16: T16,
                actual17: T17,
                actual18: T18,
                actual19: T19,
                actual20: T20,
                actual21: T21) => f(
                  actual1,
                  actual2,
                  actual3,
                  actual4,
                  actual5,
                  actual6,
                  actual7,
                  actual8,
                  actual9,
                  actual10,
                  actual11,
                  actual12,
                  actual13,
                  actual14,
                  actual15,
                  actual16,
                  actual17,
                  actual18,
                  actual19,
                  actual20,
                  actual21
                )
              }
            })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]],
            pm8.asInstanceOf[Manifest[Any]],
            pm9.asInstanceOf[Manifest[Any]],
            pm10.asInstanceOf[Manifest[Any]],
            pm11.asInstanceOf[Manifest[Any]],
            pm12.asInstanceOf[Manifest[Any]],
            pm13.asInstanceOf[Manifest[Any]],
            pm14.asInstanceOf[Manifest[Any]],
            pm15.asInstanceOf[Manifest[Any]],
            pm16.asInstanceOf[Manifest[Any]],
            pm17.asInstanceOf[Manifest[Any]],
            pm18.asInstanceOf[Manifest[Any]],
            pm19.asInstanceOf[Manifest[Any]],
            pm20.asInstanceOf[Manifest[Any]],
            pm21.asInstanceOf[Manifest[Any]])
        case List(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8, pm9, pm10, pm11, pm12, pm13, pm14, pm15, pm16, pm17, pm18, pm19, pm20, pm21, pm22) =>
          new StepBody(name, regex)(
            (
              value1: Any,
              value2: Any,
              value3: Any,
              value4: Any,
              value5: Any,
              value6: Any,
              value7: Any,
              value8: Any,
              value9: Any,
              value10: Any,
              value11: Any,
              value12: Any,
              value13: Any,
              value14: Any,
              value15: Any,
              value16: Any,
              value17: Any,
              value18: Any,
              value19: Any,
              value20: Any,
              value21: Any,
              value22: Any) => {
              process[R](
                value1,
                value2,
                value3,
                value4,
                value5,
                value6,
                value7,
                value8,
                value9,
                value10,
                value11,
                value12,
                value13,
                value14,
                value15,
                value16,
                value17,
                value18,
                value19,
                value20,
                value21,
                value22
              ) {
                case List(
                actual1: T1,
                actual2: T2,
                actual3: T3,
                actual4: T4,
                actual5: T5,
                actual6: T6,
                actual7: T7,
                actual8: T8,
                actual9: T9,
                actual10: T10,
                actual11: T11,
                actual12: T12,
                actual13: T13,
                actual14: T14,
                actual15: T15,
                actual16: T16,
                actual17: T17,
                actual18: T18,
                actual19: T19,
                actual20: T20,
                actual21: T21) => f(
                  actual1,
                  actual2,
                  actual3,
                  actual4,
                  actual5,
                  actual6,
                  actual7,
                  actual8,
                  actual9,
                  actual10,
                  actual11,
                  actual12,
                  actual13,
                  actual14,
                  actual15,
                  actual16,
                  actual17,
                  actual18,
                  actual19,
                  actual20,
                  actual21
                )
              }
            })(
            pm1.asInstanceOf[Manifest[Any]],
            pm2.asInstanceOf[Manifest[Any]],
            pm3.asInstanceOf[Manifest[Any]],
            pm4.asInstanceOf[Manifest[Any]],
            pm5.asInstanceOf[Manifest[Any]],
            pm6.asInstanceOf[Manifest[Any]],
            pm7.asInstanceOf[Manifest[Any]],
            pm8.asInstanceOf[Manifest[Any]],
            pm9.asInstanceOf[Manifest[Any]],
            pm10.asInstanceOf[Manifest[Any]],
            pm11.asInstanceOf[Manifest[Any]],
            pm12.asInstanceOf[Manifest[Any]],
            pm13.asInstanceOf[Manifest[Any]],
            pm14.asInstanceOf[Manifest[Any]],
            pm15.asInstanceOf[Manifest[Any]],
            pm16.asInstanceOf[Manifest[Any]],
            pm17.asInstanceOf[Manifest[Any]],
            pm18.asInstanceOf[Manifest[Any]],
            pm19.asInstanceOf[Manifest[Any]],
            pm20.asInstanceOf[Manifest[Any]],
            pm21.asInstanceOf[Manifest[Any]],
            pm22.asInstanceOf[Manifest[Any]])
      }
    }

  }

}

object DataStored {

  // Only allow uppercase letters, digits and underscores
  private val dataStoreRegex = "[A-Z\\d_]+"
  private val dataStoredRegexFull = s"\\{DS-($dataStoreRegex-)?Returned}"
  private val basicReturn = "DS-Returned"
  private val basicReturnFull = "{DS-Returned}"

  private val registeredDataStoreTypes = new mutable.HashSet[String]()
}
