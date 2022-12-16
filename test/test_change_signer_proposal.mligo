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

let case_execute_add_signer_proposal =
  Breath.Model.case
  "test add signers proposal"
  "successuful add signers"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, carol)) = Breath.Context.init_default () in
      let signers : address set = Set.literal [alice.address] in
      let init_storage = Helper.init_storage (signers, 1n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in

      let param = ([] : (nat proposal_content) list) in

      (* create proposal *)
      let param = Add_signers (Set.literal [bob.address; carol.address]) :: param in
      let action = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param) in
      let sign_action = Breath.Context.act_as alice (Helper.sign_and_execute_proposal multisig_contract 1n true) in

      let storage = Breath.Contract.storage_of multisig_contract in

      Breath.Result.reduce [
        action
      ; sign_action
      ; Breath.Assert.is_equal "storage threshold" storage.signers
        (Set.literal [alice.address; bob.address; carol.address])
      ])

let case_execute_add_existed_signer_proposal =
  Breath.Model.case
  "test add existed signers proposal"
  "nothing happen"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, _bob, _carol)) = Breath.Context.init_default () in
      let signers : address set = Set.literal [alice.address] in
      let init_storage = Helper.init_storage (signers, 1n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in

      let param = ([] : (nat proposal_content) list) in

      (* create proposal *)
      let param = Add_signers (Set.literal [alice.address;]) :: param in
      let action = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param) in
      let sign_action = Breath.Context.act_as alice (Helper.sign_and_execute_proposal multisig_contract 1n true) in

      let storage = Breath.Contract.storage_of multisig_contract in

      Breath.Result.reduce [
        action
      ; sign_action
      ; Breath.Assert.is_equal "storage threshold" storage.signers
        (Set.literal [alice.address;])
      ])

let case_execute_remove_signer_proposal =
  Breath.Model.case
  "test remove signers proposal"
  "successuful remove signers"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, carol)) = Breath.Context.init_default () in
      let signers : address set = Set.literal [alice.address; bob.address; carol.address] in
      let init_storage = Helper.init_storage (signers, 1n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in

      let param = ([] : (nat proposal_content) list) in

      (* create proposal *)
      let param = Remove_signers (Set.literal [bob.address; carol.address]) :: param in
      let action = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param) in
      let sign_action = Breath.Context.act_as alice (Helper.sign_and_execute_proposal multisig_contract 1n true) in

      let storage = Breath.Contract.storage_of multisig_contract in

      Breath.Result.reduce [
        action
      ; sign_action
      ; Breath.Assert.is_equal "storage threshold" storage.signers
        (Set.literal [alice.address;])
      ])

let case_execute_remove_nonexisted_signer_proposal =
  Breath.Model.case
  "test remove nonexisted signers proposal"
  "nothing happen"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, carol)) = Breath.Context.init_default () in
      let signers : address set = Set.literal [alice.address;] in
      let init_storage = Helper.init_storage (signers, 1n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in

      let param = ([] : (nat proposal_content) list) in

      (* create proposal *)
      let param = Remove_signers (Set.literal [bob.address; carol.address]) :: param in
      let action = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param) in
      let sign_action = Breath.Context.act_as alice (Helper.sign_and_execute_proposal multisig_contract 1n true) in

      let storage = Breath.Contract.storage_of multisig_contract in

      Breath.Result.reduce [
        action
      ; sign_action
      ; Breath.Assert.is_equal "storage threshold" storage.signers
        (Set.literal [alice.address;])
      ])

let test_suite =
  Breath.Model.suite "Suite for changing signer proposal" [
    case_execute_add_signer_proposal
  ; case_execute_add_existed_signer_proposal
  ; case_execute_remove_signer_proposal
  ; case_execute_remove_nonexisted_signer_proposal
  ]

