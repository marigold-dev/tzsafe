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

let case_execute_add_owner_proposal =
  Breath.Model.case
  "test add owners proposal"
  "successuful add owners"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in

      let param = ([] : (nat proposal_content) list) in

      (* create proposal *)
      let param = Add_owners (Set.literal [bob.address; carol.address]) :: param in
      let action = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param) in
      let sign_action = Breath.Context.act_as alice (Helper.sign_proposal multisig_contract 1n true param) in
      let resolve_action = Breath.Context.act_as alice (Helper.resolve_proposal multisig_contract 1n param) in

      let storage = Breath.Contract.storage_of multisig_contract in

      Breath.Result.reduce [
        action
      ; sign_action
      ; resolve_action
      ; Breath.Assert.is_equal "storage threshold" storage.wallet.owners
        (Set.literal [alice.address; bob.address; carol.address])
      ])

let case_execute_add_existed_owner_proposal =
  Breath.Model.case
  "test add existed owners proposal"
  "nothing happen"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, _bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in

      let param = ([] : (nat proposal_content) list) in

      (* create proposal *)
      let param = Add_owners (Set.literal [alice.address;]) :: param in
      let action = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param) in
      let sign_action = Breath.Context.act_as alice (Helper.sign_proposal multisig_contract 1n true param) in
      let resolve_action = Breath.Context.act_as alice (Helper.resolve_proposal multisig_contract 1n param) in

      let storage = Breath.Contract.storage_of multisig_contract in

      Breath.Result.reduce [
        action
      ; sign_action
      ; resolve_action
      ; Breath.Assert.is_equal "storage threshold" storage.wallet.owners
        (Set.literal [alice.address;])
      ])

let case_execute_remove_owner_proposal =
  Breath.Model.case
  "test remove owners proposal"
  "successuful remove owners"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address; carol.address] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in

      let param = ([] : (nat proposal_content) list) in

      (* create proposal *)
      let param = Remove_owners (Set.literal [bob.address; carol.address]) :: param in
      let action = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param) in
      let sign_action = Breath.Context.act_as alice (Helper.sign_proposal multisig_contract 1n true param) in
      let resolve_action = Breath.Context.act_as alice (Helper.resolve_proposal multisig_contract 1n param) in

      let storage = Breath.Contract.storage_of multisig_contract in

      Breath.Result.reduce [
        action
      ; sign_action
      ; resolve_action
      ; Breath.Assert.is_equal "storage threshold" storage.wallet.owners
        (Set.literal [alice.address;])
      ])

let case_execute_remove_nonexisted_owner_proposal =
  Breath.Model.case
  "test remove nonexisted owners proposal"
  "nothing happen"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address;] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in

      let param = ([] : (nat proposal_content) list) in

      (* create proposal *)
      let param = Remove_owners (Set.literal [bob.address; carol.address]) :: param in
      let action = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param) in
      let sign_action = Breath.Context.act_as alice (Helper.sign_proposal multisig_contract 1n true param) in
      let resolve_action = Breath.Context.act_as alice (Helper.resolve_proposal multisig_contract 1n param) in

      let storage = Breath.Contract.storage_of multisig_contract in

      Breath.Result.reduce [
        action
      ; sign_action
      ; resolve_action
      ; Breath.Assert.is_equal "storage owner" storage.wallet.owners
        (Set.literal [alice.address;])
      ])

let case_resolve_transfer_proposal_after_owner_changed =
  Breath.Model.case
  "test resolve transfer proposal after owner changed"
  "proposal shouldn't be solved if signers don't match owners"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 100tez in

      let param = ([] : (nat proposal_content) list) in

      (* 1. create transfer proposal *)
      let param1 = Transfer { target = alice.address; parameter = (); amount = 10tez;} :: param in
      let action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in
      let sign_action1 = Breath.Context.act_as alice (Helper.sign_proposal multisig_contract 1n true param1) in

      (* 2. create proposal of changing owner and resolve *)
      let param2 = Remove_owners (Set.literal [alice.address]) :: param in
      let action2 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param2) in
      let sign_action2 = Breath.Context.act_as alice (Helper.sign_proposal multisig_contract 2n true param2) in
      let resolve_action2 = Breath.Context.act_as alice (Helper.resolve_proposal multisig_contract 2n param2) in

      (* 3. try to resolve *)
      let resolve_action3_1 = Breath.Context.act_as alice (Helper.resolve_proposal multisig_contract 1n param1) in
      let resolve_action3_2 = Breath.Context.act_as bob (Helper.resolve_proposal multisig_contract 1n param1) in

      (* 4. sign and resolve *)
      let sign_action4 = Breath.Context.act_as bob (Helper.sign_proposal multisig_contract 1n true param1) in
      let resolve_action4 = Breath.Context.act_as bob (Helper.resolve_proposal multisig_contract 1n param1) in

      let storage = Breath.Contract.storage_of multisig_contract in

      let proposal1 = Util.unopt (Big_map.find_opt 1n storage.wallet.proposals) "proposal 1 doesn't exist" in

      Breath.Result.reduce [
        action1
      ; sign_action1
      ; action2
      ; sign_action2
      ; resolve_action2
      ; Breath.Expect.fail_with_message "Only the contract owners can perform this operation" resolve_action3_1
      ; Breath.Expect.fail_with_message "No enough signature to resolve the proposal" resolve_action3_2
      ; sign_action4
      ; resolve_action4
      ; Breath.Assert.is_equal "signature" proposal1.signatures (Map.literal [(bob.address, true)])
      ])

let test_suite =
  Breath.Model.suite "Suite for changing owner proposal" [
    case_execute_add_owner_proposal
  ; case_execute_add_existed_owner_proposal
  ; case_execute_remove_owner_proposal
  ; case_execute_remove_nonexisted_owner_proposal
  ; case_resolve_transfer_proposal_after_owner_changed
  ]

