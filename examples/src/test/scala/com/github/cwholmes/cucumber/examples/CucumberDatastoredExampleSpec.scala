package com.github.cwholmes.cucumber.examples

import org.junit.runner.RunWith
import io.cucumber.junit.{Cucumber, CucumberOptions}

@RunWith(classOf[Cucumber])
@CucumberOptions(
    features = Array("classpath:/features/basic.feature"),
    glue = Array("com/github/cwholmes/cucumber/examples"),
    plugin = Array("json:target/cucumber-report/datastore-example.json"))
class DatastoredExampleSpec {}
