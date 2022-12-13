(* MIT License
   Copyright (c) 2022 Marigold <contact@marigold.dev>
   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal in
   the Software without restriction, including without limitation the rights to
   use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
   the Software, and to permit persons to whom the Software is furnished to do so,
   subject to the following conditions:
   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.
   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

#import "ligo-breathalyzer/lib/lib.mligo" "Breath"

#import "./test_receiving_tez.mligo" "Tez"
#import "./test_basic_proposal.mligo" "Basic_proposal"
#import "./test_sign.mligo" "Sign"
#import "./test_setting.mligo" "Setting"
#import "./test_adjust_threshold_proposal.mligo" "Adjust_threshold_proposal"
#import "./test_change_signer_proposal.mligo" "Change_signer_proposal"
#import "./test_lambda_proposal.mligo" "Lambda_proposal"
#import "./test_emit_events.mligo" "Emit_events"

let () =
  Breath.Model.run_suites Void
  [ Tez.test_suite
  ; Basic_proposal.test_suite
  ; Sign.test_suite
  ; Setting.test_suite
  ; Adjust_threshold_proposal.test_suite
  ; Change_signer_proposal.test_suite
  ; Lambda_proposal.test_suite
  ; Emit_events.test_suite
  ]
