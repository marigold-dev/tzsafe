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


#import "../common/constants.mligo" "Constants"
#import "../common/util.mligo" "Util"
#import "proposal_content.mligo" "Proposal_content"
#import "parameter.mligo" "Parameter"
#import "storage.mligo" "Storage"
#import "conditions.mligo" "Conditions"
#import "execution.mligo" "Execution"
#import "event.mligo" "Event"

type parameter_types = Parameter.Types.t
type proposal_id = Parameter.Types.proposal_id
type storage_types = Storage.Types.t
type storage_types_proposal = Storage.Types.proposal
type storage_types_proposal_state = Storage.Types.proposal_state
type effective_period = Storage.Types.effective_period
type proposal_content = Proposal_content.Types.t

type request = parameter_types * storage_types
type result = operation list * storage_types


(**
 * Default entrypoint
 *)
let default (_, s : unit * storage_types) : result =
    let event = Tezos.emit "%receiving_tez" ({ from = Tezos.get_sender (); amount = Tezos.get_amount (); } : Event.Types.receiving_tez) in
    ([event], s)

(**
 * Proposal creation
 *)

let create_proposal (proposal_contents, storage : proposal_content list * storage_types) : result =
    let () = Conditions.only_owner storage in
    let () = Conditions.amount_must_be_zero_tez (Tezos.get_amount ()) in
    let () = Conditions.not_empty_content proposal_contents in
    let proposal = Storage.Op.create_proposal proposal_contents in
    let storage = Storage.Op.register_proposal(proposal, storage) in
    let event = Tezos.emit "%create_proposal" ({ proposal_id = storage.proposal_counter } : Event.Types.create_proposal) in
    ([event], storage)

(**
 * Proposal Signing
 *)

let sign_proposal
  ( proposal_id, proposal_contents, agreement, storage
      : proposal_id
      * proposal_content list
      * Parameter.Types.agreement
      * storage_types)
  : result =
    let () = Conditions.only_owner storage in
    let () = Conditions.amount_must_be_zero_tez (Tezos.get_amount ()) in
    let proposal = Storage.Op.retrieve_proposal (proposal_id, storage) in
    let () = Conditions.unsigned proposal in
    let () = Conditions.within_expiration_time proposal.proposer.timestamp storage.effective_period in
    let () = Conditions.check_proposals_content proposal_contents proposal.contents in
    let owner = Tezos.get_sender () in
    let proposal = Storage.Op.update_signature (proposal, owner, agreement) in
    let storage = Storage.Op.update_proposal(proposal_id, proposal, storage) in
    let event = Tezos.emit "%sign_proposal" ({proposal_id; signer = owner; agreement } : Event.Types.sign_proposal) in
    ([event], storage)

(**
 * Proposal Execution
 *)

let resolve_proposal
  ( proposal_id, proposal_contents, storage
      : proposal_id
      * proposal_content list
      * storage_types)
  : result =
    let () = Conditions.only_owner storage in
    let () = Conditions.amount_must_be_zero_tez (Tezos.get_amount ()) in
    let proposal = Storage.Op.retrieve_proposal(proposal_id, storage) in
    let () = Conditions.check_proposals_content proposal_contents proposal.contents in
    let expiration_time = proposal.proposer.timestamp + storage.effective_period in
    let proposal = Storage.Op.update_proposal_state (proposal, storage.owners, storage.threshold, expiration_time) in
    let () = Conditions.ready_to_execute proposal.state in
    let storage = Storage.Op.update_proposal(proposal_id, proposal, storage) in
    let ops, storage = Execution.perform_operations proposal_id proposal storage in
    let event = Tezos.emit "%resolve_proposal" ({ proposal_id = proposal_id ; proposal_state = proposal.state } : Event.Types.resolve_proposal) in
    (event::ops, storage)

let contract (action, storage : request) : result =
    let ops, storage =
      match action with
      | Default u -> default (u, storage)
      | Create_proposal { proposal_contents } ->
          create_proposal (proposal_contents, storage)
      | Sign_proposal { proposal_id; proposal_contents; agreement } ->
          sign_proposal (proposal_id, proposal_contents, agreement, storage)
      | Resolve_proposal { proposal_id; proposal_contents } ->
          resolve_proposal (proposal_id, proposal_contents, storage)
    in
    let () = Conditions.check_setting storage in
    (ops, storage)
