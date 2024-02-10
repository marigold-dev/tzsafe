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
#import "./common/helper.mligo" "Helper"
#import "./common/assert.mligo" "Assert"
#import "./common/util.mligo" "Util"
#import "./common/mock_contract.mligo" "Mock_contract"
#import "../src/wallet/proposal_content.mligo" "Proposal_content"
#import "../src/wallet/event.mligo" "Event"
#import "../src/tzsafe.mligo" "App"

type proposal_content = Proposal_content.Types.t

let case_emit_poe =
  Breath.Model.case
  "test emit poe"
  "successuful emit poe"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let multisig_contract = Helper.originate level init_storage 0tez in
 
      let payload = 0x01 in
      let param = ([Proof_of_event {payload}] : proposal_content list) in

      let action = Breath.Context.act_as alice (Helper.proof_of_event_challenge 0tez multisig_contract payload) in
      let sign_action = Breath.Context.act_as bob (Helper.sign_proposal multisig_contract 1n true param) in
      let resolve_action = Breath.Context.act_as bob (Helper.resolve_proposal multisig_contract 1n param) in
      let multisig_address = multisig_contract.originated_address in

      let events = (Util.get_last_events_from multisig_address "proof_of_event" : Event.Types.proof_of_event list) in

      let { payload = expected_payload }
          = Option.unopt (List.head_opt events) in

      Breath.Result.reduce [
        action
      ; sign_action
      ; resolve_action
      ; Breath.Assert.is_equal "payload of poe" expected_payload payload
      ])

let case_process_poe_challenge_with_non_zero_amount =
  Breath.Model.case
  "test process poe challange with 1tez"
  "fail to process proof_of_event_challenge"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let multisig_contract = Helper.originate level init_storage 0tez in
 
      let payload = 0x01 in

      let action = Breath.Context.act_as alice (Helper.proof_of_event_challenge 1tez multisig_contract payload) in
      Breath.Result.reduce [
        Breath.Expect.fail_with_message "You must not send tez to the smart contract" action
      ])

let test_suite =
  Breath.Model.suite "Suite for tzip17" [
    case_emit_poe
  ; case_process_poe_challenge_with_non_zero_amount
  ]
