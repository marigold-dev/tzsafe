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

let case_gathering_signatures =
  Breath.Model.case
  "test gathering signatures"
  "successuful gathering proposal but not executing"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address; carol.address] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let multisig_contract = Helper.originate level App.main init_storage 10tez in
      let param = ([] : proposal_content list) in

      (* create proposal 1 *)
      let param1 = (Transfer { target = alice.address; amount = 10tez;} :: param) in
      let create_action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in
      let sign_action1_1 = Breath.Context.act_as bob (Helper.sign_proposal multisig_contract 1n true param1) in
      let sign_action1_2 = Breath.Context.act_as carol (Helper.sign_proposal multisig_contract 1n true param1) in

      (* create proposal 2 *)
      let param2 = (Transfer { target = bob.address; amount = 20tez;} :: param) in
      let create_action2 = Breath.Context.act_as bob (Helper.create_proposal multisig_contract param2) in
      let sign_action2_1 = Breath.Context.act_as carol (Helper.sign_proposal multisig_contract 2n true param2) in
      let sign_action2_2 = Breath.Context.act_as alice (Helper.sign_proposal multisig_contract 2n true param2) in

      let balance = Breath.Contract.balance_of multisig_contract in
      let storage = Breath.Contract.storage_of multisig_contract in

      let proposal1 = Util.unopt (Big_map.find_opt 1n storage.proposals) "proposal 1 doesn't exist" in
      let proposal2 = Util.unopt (Big_map.find_opt 2n storage.proposals) "proposal 2 doesn't exist" in

      Breath.Result.reduce [
        create_action1
      ; sign_action1_1
      ; sign_action1_2
      ; create_action2
      ; sign_action2_1
      ; sign_action2_2
      ; Breath.Assert.is_equal "balance" balance 10tez
      ; Breath.Assert.is_equal "the counter of proposal" storage.proposal_counter 2n
      ; Assert.is_proposal_equal "#1 proposal" proposal1
        ({
          state            = Proposing;
          signatures       = Map.literal [(bob.address, true); (carol.address, true)];
          proposer         = { actor = alice.address; timestamp = Tezos.get_now () };
          resolver         = None;
          contents         = [ Transfer {
            amount           = 10tez;
            target           = alice.address;
          }]
        })
      ; Assert.is_proposal_equal "#2 proposal" proposal2
        ({
          state            = Proposing;
          signatures       = Map.literal [(carol.address, true); (alice.address, true)];
          proposer         = { actor = bob.address; timestamp = Tezos.get_now () };
          resolver         = None;
          contents         = [ Transfer {
            target           = bob.address;
            amount           = 20tez;
          }]
        })
      ])

let case_fail_to_sign_proposal_with_nonzero_amount =
  Breath.Model.case
  "sign the proposal with nonzero amount"
  "fail to sign"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig_contract = Helper.originate level App.main init_storage 0tez in
      let param = ([] : proposal_content list) in

      let param1 = (Transfer { target = alice.address; amount = 10tez;} :: param) in
      let action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in
      let sign_action1 = Breath.Context.act_as bob (Helper.sign_proposal_with_amount 1tez multisig_contract 1n true param1) in

      Breath.Result.reduce [
        action1
      ; Breath.Expect.fail_with_message "You must not send tez to the smart contract" sign_action1
      ])

let case_fail_double_sign =
  Breath.Model.case
  "sign the same proposal twice"
  "fail to sign"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig_contract = Helper.originate level App.main init_storage 0tez in
      let param = ([] : proposal_content list) in

      let param1 = (Transfer { target = alice.address; amount = 10tez;} :: param) in
      let action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in
      let sign_action1 = Breath.Context.act_as bob (Helper.sign_proposal multisig_contract 1n true param1) in
      let sign_action2 = Breath.Context.act_as bob (Helper.sign_proposal multisig_contract 1n true param1) in

      Breath.Result.reduce [
        action1
      ; sign_action1
      ; Breath.Expect.fail_with_message "You have already signed this proposal" sign_action2
      ])

let case_unauthorized_user_fail_to_sign =
  Breath.Model.case
  "unauthorized user sign proposal"
  "fail to sign"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig_contract = Helper.originate level App.main init_storage 0tez in
      let param = ([] : proposal_content list) in

      let param1 = (Transfer { target = alice.address;  amount = 10tez;} :: param) in
      let action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in
      let sign_action1 = Breath.Context.act_as carol (Helper.sign_proposal multisig_contract 1n true param1) in

      Breath.Result.reduce [
        action1
      ; Breath.Expect.fail_with_message "Only the contract owners can perform this operation" sign_action1
      ])

let case_sign_nonexisted_proposal =
  Breath.Model.case
  "sign nonexisted proposal"
  "fail to sign"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig_contract = Helper.originate level App.main init_storage 0tez in
      let param = ([] : proposal_content list) in

      let param1 = (Transfer { target = alice.address;  amount = 10tez;} :: param) in
      let action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in
      let sign_action1 = Breath.Context.act_as carol (Helper.sign_proposal multisig_contract 2n true param1) in

      Breath.Result.reduce [
        action1
      ; Breath.Expect.fail_with_message "Only the contract owners can perform this operation" sign_action1
      ])

let case_no_owner =
  Breath.Model.case
  "no owner"
  "fail to perform any operation"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, _bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig_contract = Helper.originate level App.main init_storage 0tez in
      let param = ([] : proposal_content list) in

      let param1 = (Transfer { target = alice.address;  amount = 10tez;} :: param) in
      let action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in

      Breath.Result.reduce [
        Breath.Expect.fail_with_message "Only the contract owners can perform this operation" action1
      ])

let case_fail_to_sign_after_proposal_is_resolved =
  Breath.Model.case
  "test fail to sign after proposal is resolved"
  "fail to sign"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address; carol.address] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let multisig_contract = Helper.originate level App.main init_storage 10tez in
      let param = ([] : proposal_content list) in

      let param = (Transfer { target = alice.address;  amount = 10tez;} :: param) in
      let create_action = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param) in
      let sign_action1 = Breath.Context.act_as bob (Helper.sign_proposal multisig_contract 1n true param) in
      let exe_action = Breath.Context.act_as bob (Helper.resolve_proposal multisig_contract 1n param) in
      let sign_action2 = Breath.Context.act_as carol (Helper.sign_proposal multisig_contract 1n true param) in

      Breath.Result.reduce [
        create_action
      ; sign_action1
      ; exe_action
      ; Breath.Expect.fail_with_message "This proposal has been resolved" sign_action2
      ])

let case_wrong_content_bytes =
  Breath.Model.case
  "test proide wrong content bytes"
  "fail to sign proposal"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address; carol.address] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let multisig_contract = Helper.originate level App.main init_storage 10tez in
      let param = ([] : proposal_content list) in

      (* create proposal 1 *)
      let param1 = (Transfer { target = alice.address;  amount = 10tez;} :: param) in
      let create_action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in

      let param2 = (Transfer { target = bob.address;  amount = 10tez;} :: param) in
      let sign_action1 = Breath.Context.act_as bob (Helper.sign_proposal multisig_contract 1n true param2) in


      Breath.Result.reduce [
        create_action1
      ; Breath.Expect.fail_with_message "The proposal content doesn't match" sign_action1
      ])

let test_suite =
  Breath.Model.suite "Suite for sign proposal only" [
    case_gathering_signatures
  ; case_fail_to_sign_proposal_with_nonzero_amount
  ; case_fail_double_sign
  ; case_unauthorized_user_fail_to_sign
  ; case_sign_nonexisted_proposal
  ; case_no_owner
  ; case_fail_to_sign_after_proposal_is_resolved
  ; case_wrong_content_bytes
  ]


