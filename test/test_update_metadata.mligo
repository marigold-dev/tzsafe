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
#import "../app/main.mligo" "App"

type proposal_content = Proposal_content.Types.t

let case_update_metadata =
  Breath.Model.case
  "test update element metadata"
  "successuful update element metadata"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let multisig_contract = Helper.originate level init_storage 0tez in

      let param = ([] : proposal_content list) in

      let param = Add_or_update_metadata {key=""; value=0x12} :: param in
      let action = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param) in
      let sign_action = Breath.Context.act_as bob (Helper.sign_proposal multisig_contract 1n true param) in
      let resolve_action = Breath.Context.act_as bob (Helper.resolve_proposal multisig_contract 1n param) in

      let storage = Breath.Contract.storage_of multisig_contract in

      Breath.Result.reduce [
        action
      ; sign_action
      ; resolve_action
      ; Breath.Assert.is_equal "metadata with key \"\"" (Big_map.find_opt "" storage.metadata) (Some 0x12)
      ])

let case_add_metadata =
  Breath.Model.case
  "test add more element to metadata"
  "successuful add more element to metadata"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let multisig_contract = Helper.originate level init_storage 0tez in

      let param = ([] : proposal_content list) in

      let param = Add_or_update_metadata {key="1"; value=0x12} :: param in
      let action = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param) in
      let sign_action = Breath.Context.act_as bob (Helper.sign_proposal multisig_contract 1n true param) in
      let resolve_action = Breath.Context.act_as bob (Helper.resolve_proposal multisig_contract 1n param) in

      let storage = Breath.Contract.storage_of multisig_contract in

      Breath.Result.reduce [
        action
      ; sign_action
      ; resolve_action
      ; Breath.Assert.is_equal "metadata with key \"\"" (Big_map.find_opt "" storage.metadata) (Some 0x01)
      ; Breath.Assert.is_equal "metadata with key \"1\"" (Big_map.find_opt "1" storage.metadata) (Some 0x12)
      ])

let case_remove_metadata =
  Breath.Model.case
  "test remove element from metadata"
  "successuful remove element from metadata"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let multisig_contract = Helper.originate level init_storage 0tez in

      let param = ([] : proposal_content list) in

      let param = Remove_metadata {key=""} :: param in
      let action = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param) in
      let sign_action = Breath.Context.act_as bob (Helper.sign_proposal multisig_contract 1n true param) in
      let resolve_action = Breath.Context.act_as bob (Helper.resolve_proposal multisig_contract 1n param) in

      let storage = Breath.Contract.storage_of multisig_contract in

      Breath.Result.reduce [
        action
      ; sign_action
      ; resolve_action
      ; Breath.Assert.is_equal "metadata with key \"\"" (Big_map.find_opt "" storage.metadata) None
      ])

let test_suite =
  Breath.Model.suite "Suite for update metadata" [
    case_update_metadata
  ; case_add_metadata
  ; case_remove_metadata
  ]

