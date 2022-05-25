package com.github.cwholmes.cucumber

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DataStoreSpec extends AnyFlatSpec with Matchers {

  "The DataStore" should "allow the storage of an object" in {
    DataStore("MyString", "This is my string.")

    DataStore[String]("MyString") should equal("This is my string.")
  }

  "The DataStore" should "not fail for a null object" in {
      DataStore[String]("NotInStore") should equal(null)
  }

  "The DataStore" should "clear the given class" in {
      DataStore("MyString", "This is my string.")
      DataStore("ThisJavaBoolean", java.lang.Boolean.TRUE)

      DataStore[String]("MyString") should equal("This is my string.")
      DataStore.clear(classOf[String])
      // The store will still contain the non string element
      DataStore[String]("MyString") should equal(null)
      DataStore[java.lang.Boolean]("ThisJavaBoolean") should equal(java.lang.Boolean.TRUE)
  }

  "The DataStore" should "clear all classes" in {
      DataStore("MyString", "This is my string.")
      DataStore("ThisJavaBoolean", java.lang.Boolean.TRUE)

      DataStore[String]("MyString") should equal("This is my string.")
      DataStore.clear()
      // The store will still contain the non string element
      DataStore[String]("MyString") should equal(null)
      DataStore[java.lang.Boolean]("ThisJavaBoolean") should equal(null)
  }

  "The DataStore" should "not remove the object unless cleared" in {
    DataStore("MyString", "This is my string.")

    DataStore[String]("MyString") should equal("This is my string.")
    // the store will still contain the element
    DataStore[String]("MyString") should equal("This is my string.")
  }
}
