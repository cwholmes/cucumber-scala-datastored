package com.github.cwholmes.cucumber.examples

import org.scalatest.matchers.should.Matchers
import com.github.cwholmes.cucumber.DataStored
import io.cucumber.scala.{EN, ScalaDsl}

class BasicSteps extends ScalaDsl with EN with DataStored with Matchers {

    // prepare the DS-String and DS-String-Returned captures
    DataStoreParameterType(classOf[String])

    Given("""a normal step.""") {
        // do nothing
    }

    Given("""a normal step with capture {word}.""") { word: String =>
        word should not be empty
    }

    GivenWithReturn("""a step that updates {word} with 'world' and returns {DS-Returned}.""") { word: String =>
        word should not be empty

        // value is returned and stored with the name from the defined step
        word + " world"
    }

    WhenWithReturn("""{word} is stored as {DS-Returned}.""") { word: String =>
      word
    }

    ThenWithReturn("""a step that updates and returns {DS-String-Returned} with 'world'.""") { word: String =>
        word should not be empty

        // value is returned and stored with the name from the defined step
        word + " world"
    }

    Then("""string {DS-String} should equal string {DS-String}.""") { (string1: String, string2: String) =>
      string1 should equal(string2)
    }
}
