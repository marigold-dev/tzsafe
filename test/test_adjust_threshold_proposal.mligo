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
#import "../src/internal/proposal_content.mligo" "Proposal_content"

type proposal_content = Proposal_content.Types.t

let case_execute_adjust_threshold_proposal =
  Breath.Model.case
  "test adjust threshold proposal"
  "successuful adjust"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in

      let param = ([] : (nat proposal_content) list) in

      (* create proposal *)
      let param = Adjust_threshold 2n :: param in
      let action = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param) in
      let sign_action = Breath.Context.act_as bob (Helper.sign_proposal multisig_contract 1n true param) in
      let resolve_action = Breath.Context.act_as bob (Helper.resolve_proposal multisig_contract 1n param) in

      let storage = Breath.Contract.storage_of multisig_contract in

      Breath.Result.reduce [
        action
      ; sign_action
      ; resolve_action
      ; Breath.Assert.is_equal "storage threshold" storage.threshold 2n
      ])

let case_adjust_invalid_threshold_proposal =
  Breath.Model.case
  "test create adjusting invalid threshold proposal"
  "failed to create"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in

      let param = ([] : (nat proposal_content) list) in

      (* create proposal *)
      let param = Adjust_threshold 0n :: param in
      let action = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param) in

      Breath.Result.reduce [
        Breath.Expect.fail_with_message "Threshold must be greater than 1" action
      ])

let case_fail_to_remove_all_owners_proposal =
  Breath.Model.case
  "test case fail to remove all owners proposal"
  "failed to execute proposal"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in

      let param = ([] : (nat proposal_content) list) in

      (* create proposal *)
      let param = Remove_owners owners :: param in
      let create_action = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param) in
      let sign_action = Breath.Context.act_as alice (Helper.sign_proposal multisig_contract 1n true param) in
      let exe_action = Breath.Context.act_as alice (Helper.resolve_proposal multisig_contract 1n param) in

      Breath.Result.reduce [
        create_action
      ; sign_action
      ; Breath.Expect.fail_with_message "Require at least one owner in the contract" exe_action
      ])

let case_fail_to_reduce_number_of_owners_below_threshold_proposal =
  Breath.Model.case
  "test case fail to reduce the number of owners below the threshold"
  "failed to execute proposal"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in

      let param = ([] : (nat proposal_content) list) in

      (* create proposal *)
      let param = Remove_owners (Set.literal [alice.address; ]) :: param in
      let create_action = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param) in
      let sign_action = Breath.Context.act_as alice (Helper.sign_proposal multisig_contract 1n true param) in
      let exe_action = Breath.Context.act_as alice (Helper.resolve_proposal multisig_contract 1n param) in

      Breath.Result.reduce [
        create_action
      ; sign_action
      ; Breath.Expect.fail_with_message "No enough signature to resolve the proposal" exe_action
      ])

let case_fail_to_set_invalid_effective_proposal =
  Breath.Model.case
  "test case fail to set invalid effective proposal"
  "failed to execute proposal"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in

      let param = ([] : (nat proposal_content) list) in

      (* create proposal *)
      let param = Adjust_effective_period (-1) :: param in
      let create_action = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param) in

      Breath.Result.reduce [
        Breath.Expect.fail_with_message "Effective period should be greater than 0" create_action
      ])

let test_suite =
  Breath.Model.suite "Suite for reset threshold proposal" [
    case_execute_adjust_threshold_proposal
  ; case_adjust_invalid_threshold_proposal
  ; case_fail_to_remove_all_owners_proposal
  ; case_fail_to_reduce_number_of_owners_below_threshold_proposal
  ; case_fail_to_set_invalid_effective_proposal
  ]

