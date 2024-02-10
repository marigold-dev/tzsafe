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
#import "../src/wallet/proposal_content.mligo" "Proposal_content"
#import "../src/tzsafe.mligo" "App"
#include "../src/wallet/contract.mligo"
#include "./common/util.mligo"

type proposal_content = Proposal_content.Types.t


let case_create_proposal =
  Breath.Model.case
  "test create proposal"
  "successuful create proposal"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig_contract = Helper.originate level init_storage 0tez in
      let param = ([] : proposal_content list) in

      (* create proposal 1 *)
      let param1 = (Transfer { target = alice.address; amount = 10tez;} :: param) in
      let action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in

      (* create proposal 2 *)
      let param2 = Adjust_effective_period 100 :: param in
      let action2 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param2) in

      (* create proposal 3 *)
      let param3 = Adjust_threshold 2n :: param in
      let action3 = Breath.Context.act_as bob (Helper.create_proposal multisig_contract param3) in

      (* create proposal 4 *)
      let param4 = Add_owners (Set.literal [bob.address; carol.address]) :: param in
      let action4 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param4) in

      (* create proposal 5 *)
      let param5 = Remove_owners (Set.literal [bob.address; carol.address]) :: param in
      let action5 = Breath.Context.act_as bob (Helper.create_proposal multisig_contract param5) in

      (* create proposal 7 *)
      let param6 =
        [ Transfer { target = alice.address; amount = 10tez;}
        ; Transfer { target = bob.address;amount = 20tez;}
        ; Adjust_threshold 2n
        ; Add_owners (Set.literal [bob.address; carol.address])
        ; Remove_owners (Set.literal [bob.address; carol.address])
        ; Adjust_effective_period 100
        ] in
      let action6 = Breath.Context.act_as bob (Helper.create_proposal multisig_contract param6) in

      let balance = Breath.Contract.balance_of multisig_contract in
      let storage = Breath.Contract.storage_of multisig_contract in

      let proposal1  = unopt (Big_map.find_opt 1n storage.proposals) "proposal 1 doesn't exist" in
      let proposal2  = unopt (Big_map.find_opt 2n storage.proposals) "proposal 2 doesn't exist" in
      let proposal3  = unopt (Big_map.find_opt 3n storage.proposals) "proposal 3 doesn't exist" in
      let proposal4  = unopt (Big_map.find_opt 4n storage.proposals) "proposal 4 doesn't exist" in
      let proposal5  = unopt (Big_map.find_opt 5n storage.proposals) "proposal 5 doesn't exist" in
      let proposal6  = unopt (Big_map.find_opt 6n storage.proposals) "proposal 6 doesn't exist" in

      Breath.Result.reduce [
        action1
      ; action2
      ; action3
      ; action4
      ; action5
      ; action6
      ; Breath.Assert.is_equal "balance" balance 0tez
      ; Breath.Assert.is_equal "the counter of proposal" storage.proposal_counter 6n
      ; Assert.is_proposal_equal "#1 proposal" proposal1
        ({
          state            = Proposing;
          signatures       = Map.empty;
          proposer         = { actor = alice.address; timestamp = Tezos.get_now () };
          resolver         = None;
          contents         = param1
        })
      ; Assert.is_proposal_equal "#2 proposal" proposal2
        ({
          state            = Proposing;
          signatures       = Map.empty;
          proposer         = { actor = alice.address; timestamp = Tezos.get_now () };
          resolver         = None;
          contents         = param2
        })
      ; Assert.is_proposal_equal "#3 proposal" proposal3
        ({
          state            = Proposing;
          signatures       = Map.empty;
          proposer         = { actor = bob.address; timestamp = Tezos.get_now () };
          resolver         = None;
          contents         = param3
        })
      ; Assert.is_proposal_equal "#4 proposal" proposal4
        ({
          state            = Proposing;
          signatures       = Map.empty;
          proposer         = { actor = alice.address; timestamp = Tezos.get_now () };
          resolver         = None;
          contents         = param4
        })
      ; Assert.is_proposal_equal "#5 proposal" proposal5
        ({
          state            = Proposing;
          signatures       = Map.empty;
          proposer         = { actor = bob.address; timestamp = Tezos.get_now () };
          resolver         = None;
          contents         = param5
        })
      ; Assert.is_proposal_equal "#6 proposal" proposal6
        ({
          state            = Proposing;
          signatures       = Map.empty;
          proposer         = { actor = bob.address; timestamp = Tezos.get_now () };
          resolver         = None;
          contents         = param6
        })
      ])

let case_fail_to_create_empty_proposal =
  Breath.Model.case
  "test create empty proposal"
  "failed to create proposal"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig_contract = Helper.originate level init_storage 0tez in
      let param = ([] : proposal_content list) in

      let action = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param) in

      Breath.Result.reduce [
        Breath.Expect.fail_with_message "There is no content in proposal" action
      ])

let case_fail_to_create_transfer_0_amount_proposal =
  Breath.Model.case
  "test create transfer 0 amount proposal"
  "failed to create proposal"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig_contract = Helper.originate level init_storage 0tez in
      let param = ([] : proposal_content list) in
      let param = (Transfer { target = alice.address; amount = 0tez;} :: param) in

      let action = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param) in

      Breath.Result.reduce [
        Breath.Expect.fail_with_message "Amount should be greater than zero" action
      ])

let case_fail_to_create_proposal_with_empty_owner_adjustment =
  Breath.Model.case
  "test create proposal with empty owner adjustment"
  "failed to create proposal"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig_contract = Helper.originate level init_storage 0tez in
      let param = ([] : proposal_content list) in

      let param1 = Remove_owners (Set.literal []) :: param in
      let action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in

      let param2 = Add_owners (Set.literal []) :: param in
      let action2 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param2) in

      Breath.Result.reduce [
        Breath.Expect.fail_with_message "No owner to be added or removed" action1
      ; Breath.Expect.fail_with_message "No owner to be added or removed" action2
      ])

let case_unauthorized_user_fail_to_create_proposal =
  Breath.Model.case
  "unauthorized user creates proposal"
  "fail to create proposal"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig_contract = Helper.originate level init_storage 0tez in

      (* create proposal 1 *)
      let param1 = [Transfer { target = alice.address; amount = 0tez;}] in
      let action1 = Breath.Context.act_as carol (Helper.create_proposal multisig_contract param1) in

      let balance = Breath.Contract.balance_of multisig_contract in
      let storage = Breath.Contract.storage_of multisig_contract in

      Breath.Result.reduce [
        Breath.Expect.fail_with_message "Only the contract owners can perform this operation" action1
      ; Breath.Assert.is_equal "balance" balance 0tez
      ; Breath.Assert.is_equal "the counter of proposal" storage.proposal_counter 0n
      ])

let case_fail_to_create_proposal_with_nonzero_amount =
  Breath.Model.case
  "create proposal with nonzero amount"
  "fail to create proposal"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig_contract = Helper.originate level init_storage 0tez in

      (* create proposal 1 *)
      let param1 = [Transfer { target = alice.address; amount = 0tez;}] in
      let action1 = Breath.Context.act_as alice (Helper.create_proposal_with_amount 1tez multisig_contract param1) in

      Breath.Result.reduce [
        Breath.Expect.fail_with_message "You must not send tez to the smart contract" action1
      ])

let test_suite =
  Breath.Model.suite "Suite for create proposal" [
    case_create_proposal
  ; case_fail_to_create_empty_proposal
  ; case_unauthorized_user_fail_to_create_proposal
  ; case_fail_to_create_transfer_0_amount_proposal
  ; case_fail_to_create_proposal_with_empty_owner_adjustment
  ; case_fail_to_create_proposal_with_nonzero_amount
  ]

