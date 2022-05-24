package com.github.cwholmes.cucumber

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DataStoredSpec extends AnyFlatSpec with Matchers {
  "The DataStored trait" should "do things" in {
    "hello" shouldEqual "hello"
  }
}
