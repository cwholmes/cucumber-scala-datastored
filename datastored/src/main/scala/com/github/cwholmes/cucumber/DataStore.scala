package com.github.cwholmes.cucumber

import scala.reflect.ClassTag
import scala.collection.mutable

/** Stores test data used throughout a cucumber testing scenario.
 */
object DataStore {

  /** A map of classes to a Map of names to elements.
   *
   * <p>The name is the representation of the object given by the consumer.
   *
   * <p>Ex. DataStore("MyReportData", reportData) -- myObject is type ReportDataUtil
   * reportData will be stored in the classOf[ReportDataUtil] map
   * with key "MyReportData" and value reportData.
   * then DataStore[ReportDataUtil]("MyReportData") will return reportData.
   */
  private val dataStore: mutable.Map[Class[_], mutable.Map[String, AnyRef]] = mutable.Map()

  /** Retrieve an element stored in this storage object with the requested name and class.
   *
   * @param key      the name of the element.
   * @param classTag the class tag of the element.
   * @tparam V the type of the element.
   * @return the element if it exists; otherwise, null.
   */
  def apply[V](key: String)(implicit classTag: ClassTag[V]): V = {
    val classObjects = dataStore.getOrElseUpdate(classTag.runtimeClass, mutable.Map())

    val storedObject = classObjects.get(key).orNull

    storedObject.asInstanceOf[V]
  }

  /** Store an element in this storage object with the given name and class.
   *
   * @param key      the name of the value to store.
   * @param value    the value to store.
   * @param classTag the class tag of the value to store.
   * @tparam V the type of the value to store.
   * @return an option of the pre-existing element with that name and class.
   */
  def apply[V <: AnyRef](key: String, value: V)(implicit classTag: ClassTag[V]): Option[V] = {
    dataStore.getOrElseUpdate(classTag.runtimeClass, mutable.Map()).put(key, value).asInstanceOf[Option[V]]
  }

  /** Clear all elemnts of the given type from this storage object.
   *
   * @param clazz the class to be cleared.
   * @tparam V the type of the cleared class.
   * @return an imutable option of the map that was removed.
   */
  def clear[V](clazz: Class[V]): Option[Map[String, V]] = dataStore.remove(clazz).asInstanceOf[Option[mutable.Map[String, V]]].map(_.toMap)

  /**
   * Clear all elements in this storage object.
   */
  def clear(): Unit = dataStore.clear()
}
