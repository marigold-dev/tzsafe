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
#import "./common/util.mligo" "Util"
#import "./common/assert.mligo" "Assert"

#import "../src/internal/storage.mligo" "Storage"
#import "../app/main_bytes.mligo" "App"

type proposal_content = Storage.Types.proposal_content
type storage_types_proposal = Storage.Types.proposal
type storage_types_view_proposal = Storage.Types.view_proposal

let case_view_setting =
  Breath.Model.case
  "test create proposal and view setting"
  "view successufully return setting"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig = Helper.originate level App.main init_storage 0tez in

      let storage = Breath.Contract.storage_of multisig in
      let view_owners = App.owners ((), storage) in
      let view_threshold = App.threshold ((), storage) in

      Breath.Result.reduce [
        Breath.Assert.is_equal "owner" view_owners owners
      ; Breath.Assert.is_equal "threshold" view_threshold 2n
      ])

let case_view_proposal =
  Breath.Model.case
  "test view proposal"
  "successuful view proposal"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig = Helper.originate level App.main init_storage 0tez in
      let param = ([] : (bytes proposal_content) list) in

      let param = (Execute { target = multisig.originated_address; parameter = 0x00; amount = 0tez;} :: param) in
      let action = Breath.Context.act_as alice (Helper.create_proposal multisig param) in

      let storage = Breath.Contract.storage_of multisig in

      let expected_proposal = Util.unopt (Big_map.find_opt 1n storage.proposal_map) "proposal 1 doesn't exist" in
      let view_proposal = App.proposal (1n, storage) in

      Breath.Result.reduce [
        action
      ; Assert.is_view_proposal_equal "#1 view proposal" view_proposal (App.Views.to_view_proposal expected_proposal)
      ])

let case_view_proposals =
  Breath.Model.case
  "test view proposals"
  "successuful view proposals"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig = Helper.originate level App.main init_storage 0tez in
      let param = ([] : (bytes proposal_content) list) in

      let param1 = (Execute { target = multisig.originated_address; parameter = 0x00; amount = 0tez;} :: param) in
      let action1 = Breath.Context.act_as alice (Helper.create_proposal multisig param1) in

      let param2 = (Execute { target = multisig.originated_address; parameter = 0x01; amount = 0tez;} :: param) in
      let action2 = Breath.Context.act_as alice (Helper.create_proposal multisig param2) in

      let param3 = (Execute { target = multisig.originated_address; parameter = 0x02; amount = 0tez;} :: param) in
      let action3 = Breath.Context.act_as alice (Helper.create_proposal multisig param3) in

      let param4 = (Execute { target = multisig.originated_address; parameter = 0x03; amount = 0tez;} :: param) in
      let action4 = Breath.Context.act_as alice (Helper.create_proposal multisig param4) in

      let storage = Breath.Contract.storage_of multisig in
      let view_proposals = App.proposals ((2n, 1n), storage) in

      let expected_proposal2 = Util.unopt (Big_map.find_opt 2n storage.proposal_map) "proposal 2 doesn't exist" in
      let expected_proposal3 = Util.unopt (Big_map.find_opt 3n storage.proposal_map) "proposal 3 doesn't exist" in

      Breath.Result.reduce [
        action1
      ; action2
      ; action3
      ; action4
      ; Assert.is_view_proposals_equal "view proposals" view_proposals
         (Map.map
            (fun (_i, p: nat * bytes storage_types_proposal) : bytes storage_types_view_proposal -> App.Views.to_view_proposal p)
            (Map.literal [(2n, expected_proposal2); (3n, expected_proposal3)]))
      ])

let test_suite =
  Breath.Model.suite "Suite for views" [
    case_view_setting
  ; case_view_proposal
  ; case_view_proposals
  ]
