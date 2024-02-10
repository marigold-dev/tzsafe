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
#import "./common/mock_contract.mligo" "Mock_contract"
#import "./common/helper.mligo" "Helper"
#import "./common/assert.mligo" "Assert"
#import "./common/util.mligo" "Util"
#import "../src/wallet/proposal_content.mligo" "Proposal_content"
#import "../src/wallet/storage.mligo" "Storage"
#import "../src/wallet/event.mligo" "Event"
#import "../src/tzsafe.mligo" "App"

type proposal_content = Proposal_content.Types.t
type storage_types_proposal = Storage.Types.proposal
type storage_types_proposal_state = Storage.Types.proposal_state

let case_emitted_create_proposal =
  Breath.Model.case
  "test emitted event for creating proposal"
  "successuful emit event"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig_contract = Helper.originate level init_storage 0tez in
      let param = ([] : proposal_content list) in

      let param1 = (Transfer { target = alice.address; amount = 10tez;} :: param) in
      let action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in
      let multisig_address = multisig_contract.originated_address in

      let events = (Util.get_last_events_from multisig_address "create_proposal" : Event.Types.create_proposal list) in

      let { proposal_id = emitted_id }
          = Option.unopt (List.head_opt events) in

      Breath.Result.reduce [
        action1
      ; Breath.Assert.is_equal "proposal id" emitted_id 1n
      ])

let case_emitted_sign_proposal =
  Breath.Model.case
  "test emitted event for signing proposal"
  "successuful emit event"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig_contract = Helper.originate level init_storage 0tez in
      let param = ([] : proposal_content list) in

      let param1 = (Transfer { target = bob.address; amount = 20tez;} :: param) in
      let action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in
      let sign_action1 = Breath.Context.act_as bob (Helper.sign_proposal multisig_contract 1n true param1) in
      let multisig_address = multisig_contract.originated_address in

      let events = (Util.get_last_events_from multisig_address "sign_proposal" : Event.Types.sign_proposal list) in

      let {proposal_id = emitted_proposal_id; signer = emitted_addr; agreement = emitted_agreement} = Option.unopt (List.head_opt events) in

      Breath.Result.reduce [
        action1
      ; sign_action1
      ; Breath.Assert.is_equal "proposal id" emitted_proposal_id 1n
      ; Breath.Assert.is_equal "owner" emitted_addr bob.address
      ; Breath.Assert.is_equal "agreement" emitted_agreement true
      ])

let case_emitted_exe_proposal =
  Breath.Model.case
  "test emitted event for executing proposal"
  "successuful emit event"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let multisig_contract = Helper.originate level init_storage 10tez in
      let param = ([] : proposal_content list) in

      let param1 = (Transfer { target = bob.address; amount = 1tez;} :: param) in
      let action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in
      let sign_action1 = Breath.Context.act_as bob (Helper.sign_proposal multisig_contract 1n true param1) in

      let exe_action1 = Breath.Context.act_as bob (Helper.resolve_proposal multisig_contract 1n param1) in

      let multisig_address = multisig_contract.originated_address in

      let events = (Util.get_last_events_from multisig_address "resolve_proposal" : Event.Types.resolve_proposal list) in
      let {proposal_id = emitted_proposal_id1; proposal_state = emitted_state} = Option.unopt (List.head_opt events) in

      let events = (Util.get_last_events_from multisig_address "archive_proposal" : Event.Types.archive_proposal list) in
      let {proposal_id = emitted_proposal_id2; proposal = emitted_proposal2} = Option.unopt (List.head_opt events) in

      let unpack_proposal_opt2 = (Bytes.unpack emitted_proposal2 : storage_types_proposal option ) in
      let unpack_proposal2 = Option.unopt unpack_proposal_opt2 in

      Breath.Result.reduce [
        action1
      ; sign_action1
      ; exe_action1
      ; Breath.Assert.is_equal "proposal id" emitted_proposal_id1 1n
      ; Breath.Assert.is_equal "state" emitted_state Executed
      ; Breath.Assert.is_equal "proposal id" emitted_proposal_id2 1n
      ; Assert.is_proposal_equal "proposal" unpack_proposal2
          { contents = param1
          ; proposer = { actor = alice.address; timestamp = Tezos.get_now () }
          ; resolver = (Some {actor = bob.address; timestamp = Tezos.get_now ()})
          ; signatures = Map.literal [ (bob.address, true) ]
          ; state = Executed
          }
      ])

let case_emitted_receiving_amount =
  Breath.Model.case
  "test emitted event for receiving amount"
  "successuful emit event"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig_contract = Helper.originate level init_storage 0tez in
      let contract = Mock_contract.originate_transfer_only_contract level multisig_contract.originated_address in

      let action = Breath.Context.act_as carol (fun (_u:unit) -> (Breath.Contract.transfer_with_entrypoint_to contract "default" ()) 0tez) in

      let events = (Util.get_last_events_from multisig_contract.originated_address "receiving_tez" : Event.Types.receiving_tez list) in

      let {from = emitted_sender; amount = emitted_amount} = Option.unopt (List.head_opt events) in

      Breath.Result.reduce [
        action
      ; Breath.Assert.is_equal "sender" emitted_sender contract.originated_address
      ; Breath.Assert.is_equal "amount" emitted_amount 10tez
      ])

let test_suite =
  Breath.Model.suite "Suite for emitted events" [
    case_emitted_create_proposal
  ; case_emitted_sign_proposal
  ; case_emitted_exe_proposal
  //; case_emitted_receiving_amount
  ]

