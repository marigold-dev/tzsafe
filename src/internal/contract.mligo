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

type parameter_types = Parameter.Types.t
type storage_types = Storage.Types.t
type storage_types_proposal = Storage.Types.proposal
type storage_types_proposal_state = Storage.Types.proposal_state
type effective_period = Storage.Types.effective_period
type proposal_content = Proposal_content.Types.t

type 'a request = 'a parameter_types * 'a storage_types
type 'a result = operation list * 'a storage_types


(**
 * Default entrypoint
 *)
let default (type a) (_, s : unit * a storage_types) : a result =
    let event = Tezos.emit "%receiving_tez" (Tezos.get_sender (), Tezos.get_amount ()) in
    ([event], s)

(**
 * Proposal creation
 *)
let create_proposal (type a) (proposal_content, storage : (a proposal_content) list * a storage_types) : a result =
    let () = Conditions.only_owner storage in
    let () = Conditions.amount_must_be_zero_tez (Tezos.get_amount ()) in
    let () = Conditions.not_empty_content proposal_content in
    let proposal = Storage.Op.create_proposal proposal_content in
    let storage = Storage.Op.register_proposal(proposal, storage) in
    let event = Tezos.emit "%create_proposal" (bytes storage.proposal_counter, proposal) in
    ([event], storage)

(**
 * Proposal signature only
 *)

let sign_proposal (type a)
  ( proposal_id, proposal_content, agreement, storage
    : Parameter.Types.proposal_id
      * (a proposal_content) list
      * Parameter.Types.agreement
      * a storage_types)
  : a result =
    let () = Conditions.only_owner storage in
    let () = Conditions.amount_must_be_zero_tez (Tezos.get_amount ()) in
    let proposal = Storage.Op.retrieve_proposal(proposal_id, storage) in
    let () = Conditions.unresolved proposal.state in
    let () = Conditions.unsigned proposal in
    let () = Conditions.within_expiration_time proposal.proposer.timestamp storage.effective_period in
    let packed_proposal_content = Bytes.pack proposal_content in
    let () = Conditions.check_proposals_content packed_proposal_content proposal.contents in
    let owner = Tezos.get_sender () in
    let proposal = Storage.Op.update_signature (proposal, owner, agreement) in
    let storage = Storage.Op.update_proposal(proposal_id, proposal, storage) in
    let event = Tezos.emit "%sign_proposal" (proposal_id, owner, agreement) in
    ([event], storage)

(**
 * Proposal Execution
 *)

let resolve_proposal (type a)
  ( proposal_id, packed_proposal_content, storage
      : Parameter.Types.proposal_id
      * bytes
      * a storage_types)
  : a result =
    let () = Conditions.only_owner storage in
    let () = Conditions.amount_must_be_zero_tez (Tezos.get_amount ()) in
    let proposal = Storage.Op.retrieve_proposal(proposal_id, storage) in
    let () = Conditions.unresolved proposal.state in
    let () = Conditions.check_proposals_content packed_proposal_content proposal.contents in
    let owner = Tezos.get_sender () in
    let expiration_time = proposal.proposer.timestamp + storage.effective_period in
    let proposal = Storage.Op.update_proposal_state (proposal, storage.owners, storage.threshold, expiration_time) in
    let () = Conditions.ready_to_execute proposal.state in
    let storage = Storage.Op.update_proposal(proposal_id, proposal, storage) in
    let ops, proposal, storage = Execution.perform_operations proposal storage in
    let storage = Storage.Op.update_proposal(proposal_id, proposal, storage) in
    let event = Tezos.emit "%resolve_proposal" (proposal_id, owner) in
    let poe = Tezos.emit "%proof_of_event" (proposal_id, packed_proposal_content) in
    (poe::event::ops, storage)

let contract (type a) (action, storage : a request) : a result =
    let ops, storage =
      match action with
      | Default u -> default (u, storage)
      | Create_proposal (proposal_params) ->
          create_proposal (proposal_params, storage)
      | Sign_proposal (proposal_id, proposal_content, agreement) ->
          sign_proposal (proposal_id, proposal_content, agreement, storage)
      | Proof_of_event_challenge { challenge_id; payload; } ->
          resolve_proposal (challenge_id, payload, storage)
    in
    let _ = Conditions.check_setting storage in
    (ops, storage)
