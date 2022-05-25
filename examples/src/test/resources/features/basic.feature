Feature: Example of using data stored.

  Scenario: Use the return.
    Given a normal step.
    Given a normal step with capture hello.
    Given a step that updates hello with 'world' and returns HELLO1.
    When hello is stored as HELLO2.
    Then a step that updates and returns HELLO2 with 'world'.
    Then string HELLO1 should equal string HELLO2.
