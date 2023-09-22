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
#import "./common/mock_contract.mligo" "Mock_contract"
#import "../src/internal/proposal_content.mligo" "Proposal_content"
#import "../app/main.mligo" "App"

type proposal_content = Proposal_content.Types.t

let case_invalidated_threshold =
  Breath.Model.case
  "invalidated_threshold "
  "fail to perform any operation"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, _bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address] in
      let init_storage = Helper.init_storage (owners, 0n) in
      let multisig_contract = Helper.originate level App.main init_storage 0tez in
      let param = ([] : proposal_content list) in

      let param1 = (Transfer { target = alice.address; amount = 10tez;} :: param) in
      let action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in

      Breath.Result.reduce [
        Breath.Expect.fail_with_message "Threshold must be greater than 1" action1
      ])

let case_number_of_owner_less_than_threshold =
  Breath.Model.case
  "number of owner less than threshold"
  "fail to perform any operation"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, _bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig_contract = Helper.originate level App.main init_storage 0tez in
      let param = ([] : proposal_content list) in

      let param1 = (Transfer { target = alice.address; amount = 10tez;} :: param) in
      let action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in

      Breath.Result.reduce [
        Breath.Expect.fail_with_message "Number of owner should be greater than threshold" action1
      ])

let test_suite =
  Breath.Model.suite "Suite for setting" [
    case_invalidated_threshold
  ; case_number_of_owner_less_than_threshold
  ]


