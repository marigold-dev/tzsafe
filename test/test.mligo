#import "ligo-breathalyzer/lib/lib.mligo" "Breath"

#import "./test_app.mligo" "App"

let () =
  Breath.Model.run_suites Void [
    App.test_suite
  ]
