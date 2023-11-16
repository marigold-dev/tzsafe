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
#import "./common/mock_contract.mligo" "Mock_contract"
#import "./common/util.mligo" "Util"
#import "../src/internal/proposal_content.mligo" "Proposal_content"
#import "../app/main.mligo" "App"

type proposal_content = Proposal_content.Types.t

let case_sign_proposal_passing_expiration_time =
  Breath.Model.case
  "test sign proposal passing expiration time"
  "fail to sign"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address; carol.address] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let init_storage = { init_storage with effective_period = 1_800 } in
      let multisig_contract = Helper.originate level App.main init_storage 100tez in
      let param = ([] : proposal_content list) in

      (* create proposal 1 *)
      let param1 = (Transfer { target = alice.address; amount = 10tez;} :: param) in
      let create_action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in
      let _ = Test.bake_until_n_cycle_end 100n in
      let sign_action1 = Breath.Context.act_as bob (Helper.sign_proposal multisig_contract 1n true param1) in

      Breath.Result.reduce [
        create_action1
      ; Breath.Expect.fail_with_message "The proposal has passed its expiration time" sign_action1
      ])

let case_resolve_proposal_passing_expiration_time =
  Breath.Model.case
  "test resolve proposal passing expiration time"
  "proposal should be expired"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address; carol.address] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let init_storage = { init_storage with effective_period = 1_800 } in
      let multisig_contract = Helper.originate level App.main init_storage 100tez in
      let param = ([] : proposal_content list) in

      (* create proposal 1 *)
      let param1 = (Transfer { target = alice.address; amount = 10tez;} :: param) in
      let create_action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in
      let sign_action1 = Breath.Context.act_as bob (Helper.sign_proposal multisig_contract 1n true param1) in
      let _ = Test.bake_until_n_cycle_end 100n in
      let exe_action1 = Breath.Context.act_as bob (Helper.resolve_proposal multisig_contract 1n param1) in

      let storage = Breath.Contract.storage_of multisig_contract in
      let proposal1 = Util.unopt (Big_map.find_opt 1n storage.archives) "proposal 1 doesn't exist" in

      Breath.Result.reduce [
        create_action1
      ; sign_action1
      ; exe_action1
      ; Breath.Assert.is_equal "#1 proposal" proposal1 Expired
      ])

let case_resolve_executed_proposal_passing_expiration_time =
  Breath.Model.case
  "test resolve executed proposal passing expiration time"
  "the state of proposal shouldn't change"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address; carol.address] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let init_storage = { init_storage with effective_period = 1_800 } in
      let multisig_contract = Helper.originate level App.main init_storage 100tez in
      let param = ([] : proposal_content list) in

      (* create proposal 1 *)
      let param1 = (Transfer { target = alice.address; amount = 10tez;} :: param) in
      let create_action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in
      let sign_action1 = Breath.Context.act_as bob (Helper.sign_proposal multisig_contract 1n true param1) in
      let exe_action1 = Breath.Context.act_as bob (Helper.resolve_proposal multisig_contract 1n param1) in
      let _ = Test.bake_until_n_cycle_end 100n in
      let exe_action2 = Breath.Context.act_as bob (Helper.resolve_proposal multisig_contract 1n param1) in

      let storage = Breath.Contract.storage_of multisig_contract in
      let proposal1 = Util.unopt (Big_map.find_opt 1n storage.archives) "proposal 1 doesn't exist" in

      Breath.Result.reduce [
        create_action1
      ; sign_action1
      ; exe_action1
      ; Breath.Expect.fail_with_message "This proposal has been resolved" exe_action2
      ; Breath.Assert.is_equal "#1 proposal" proposal1 Executed
      ])

let case_resolve_rejected_proposal_passing_expiration_time=
  Breath.Model.case
  "test resolve rejected proposal passing expiration time"
  "the state of proposal shouldn't change"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address; carol.address] in
      let init_storage = Helper.init_storage (owners, 3n) in
      let init_storage = { init_storage with effective_period = 1_800 } in
      let multisig_contract = Helper.originate level App.main init_storage 100tez in
      let param = ([] : proposal_content list) in

      (* create proposal 1 *)
      let param1 = (Transfer { target = alice.address; amount = 10tez;} :: param) in
      let create_action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in
      let sign_action1 = Breath.Context.act_as bob (Helper.sign_proposal multisig_contract 1n false param1) in
      let exe_action1 = Breath.Context.act_as bob (Helper.resolve_proposal multisig_contract 1n param1) in
      let _ = Test.bake_until_n_cycle_end 100n in
      let exe_action2 = Breath.Context.act_as bob (Helper.resolve_proposal multisig_contract 1n param1) in

      let storage = Breath.Contract.storage_of multisig_contract in
      let proposal1 = Util.unopt (Big_map.find_opt 1n storage.archives) "proposal 1 doesn't exist" in

      Breath.Result.reduce [
        create_action1
      ; sign_action1
      ; exe_action1
      ; Breath.Expect.fail_with_message "This proposal has been resolved" exe_action2
      ; Breath.Assert.is_equal "#1 proposal" proposal1 Rejected
      ])

let case_adjust_effective_time =
  Breath.Model.case
  "test adjust effective period"
  "effective period should be updated"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address; carol.address] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let init_storage = { init_storage with effective_period = 1_800 } in
      let multisig_contract = Helper.originate level App.main init_storage 100tez in
      let param = ([] : proposal_content list) in

      (* create proposal 1 *)
      let param1 = (Adjust_effective_period 200) :: param in
      let create_action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in
      let sign_action1 = Breath.Context.act_as bob (Helper.sign_proposal multisig_contract 1n true param1) in
      let exe_action1 = Breath.Context.act_as bob (Helper.resolve_proposal multisig_contract 1n param1) in

      let storage = Breath.Contract.storage_of multisig_contract in

      Breath.Result.reduce [
        create_action1
      ; sign_action1
      ; exe_action1
      ; Breath.Assert.is_equal  "adjust effective period" 200 storage.effective_period
      ])

let case_adjust_effective_time_with_invalid_value =
  Breath.Model.case
  "test adjust invalide effective period"
  "effective period shouldn't be updated"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address; carol.address] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let init_storage = { init_storage with effective_period = 1_800 } in
      let multisig_contract = Helper.originate level App.main init_storage 100tez in
      let param = ([] : proposal_content list) in

      (* create proposal 1 *)
      let param1 = (Adjust_effective_period 0) :: param in
      let create_action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in

      let storage = Breath.Contract.storage_of multisig_contract in

      Breath.Result.reduce [
      Breath.Expect.fail_with_message "Effective period should be greater than 0" create_action1
      ; Breath.Assert.is_equal  "adjust effective period" 1_800 storage.effective_period
      ])

let test_suite =
  Breath.Model.suite "Suite for expiration time " [
    case_sign_proposal_passing_expiration_time
  ; case_resolve_proposal_passing_expiration_time
  ; case_resolve_executed_proposal_passing_expiration_time
  ; case_resolve_rejected_proposal_passing_expiration_time
  ; case_adjust_effective_time
  ; case_adjust_effective_time_with_invalid_value
  ]
