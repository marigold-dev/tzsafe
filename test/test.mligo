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
#import "./test_setting.mligo" "Setting"
#import "./test_adjust_threshold_proposal.mligo" "Adjust_threshold_proposal"
#import "./test_change_owner_proposal.mligo" "Change_owner_proposal"
#import "./test_lambda_proposal.mligo" "Lambda_proposal"
#import "./test_emit_events.mligo" "Emit_events"
#import "./test_sign_only_entrypoint.mligo" "Sign_ony"
#import "./test_resolve_proposal_entrypoint.mligo" "Exe"
#import "./test_disapproval.mligo" "Disapproval"
#import "./test_expiration_time.mligo" "Exp_date"
#import "./test_update_metadata.mligo" "Update_metadata"
#import "./test_tzip27.mligo" "Tzip27"

#include "../src/internal/contract.mligo"
#import "../app/main.mligo" "App"

#import "../src/internal/proposal_content.mligo" "Proposal_content"
#import "../src/internal/parameter.mligo" "Param"

type proposal_content = Proposal_content.Types.t
type sign_proposal = Param.Types.sign_proposal

let () =
  Breath.Model.run_suites Void 
  [ Tez.test_suite
  ; Basic_proposal.test_suite
  ; Setting.test_suite
  ; Adjust_threshold_proposal.test_suite
  ; Change_owner_proposal.test_suite
  ; Lambda_proposal.test_suite
  ; Emit_events.test_suite
  ; Sign_ony.test_suite
  ; Exe.test_suite
  ; Disapproval.test_suite
  ; Exp_date.test_suite
  ; Update_metadata.test_suite
  ; Tzip27.test_suite
  ]
