#import "ligo-breathalyzer/lib/lib.mligo" "Breath"

let case_mock_test =
  Breath.Model.case
    "mock test"
    "need to implement"
    (fun (_level: Breath.Logger.level) ->
      Breath.Result.reduce [
        Breath.Assert.is_equal "test" true true
      ])

let test_suite =
  Breath.Model.suite "Suite for App" [
    case_mock_test
  ]
