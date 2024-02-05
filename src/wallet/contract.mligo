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
type payload = Parameter.Types.payload
type storage_types = Storage.Types.t
type storage_types_proposal = Storage.Types.proposal
type storage_types_proposal_state = Storage.Types.proposal_state
type proposal_content = Proposal_content.Types.t
type voting_option = Parameter.Types.voting_option

type request = parameter_types -> storage_types
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
    let (addr, token_id) = storage.nft in
    let () = assert_some (Tezos.call_view "get_balance" (Tezos.get_sender(), token_id) addr : nat option) in
    let () = Conditions.amount_must_be_zero_tez (Tezos.get_amount ()) in
    let () = Conditions.not_empty_content proposal_contents in
    let proposal = Storage.Op.create_proposal proposal_contents in
    let storage = Storage.Op.register_proposal(proposal, storage) in
    let proposal_id = storage.proposal_counter in
    let event = Tezos.emit "%create_proposal" ({ proposal_id } : Event.Types.create_proposal) in
    // Not implement yet
    let contr_opt = Tezos.get_entrypoint_opt "%active_lock" addr in
    let contr = Option.unopt contr_opt in
    let op = Tezos.transaction proposal_id 0tez contr in
    ([event; op], storage)

(**
 * Proposal Signing
 *)

let sign_proposal
  ( proposal_id, proposal_contents, voting, storage
      : proposal_id
      * proposal_content list
      * (voting_option * nat) 
      * storage_types)
  : result =
    //let () = Conditions.only_owner storage in
    //let () = Conditions.amount_must_be_zero_tez (Tezos.get_amount ()) in
    //let proposal = Storage.Op.retrieve_proposal (proposal_id, storage) in
    //let () = Conditions.unsigned proposal in
    //let () = Conditions.within_expiration_time proposal.proposer.timestamp storage.effective_period in
    //let () = Conditions.check_proposals_content proposal_contents proposal.contents in
    //let owner = Tezos.get_sender () in
    //let proposal = Storage.Op.update_signature (proposal, owner, agreement) in
    //let storage = Storage.Op.update_proposal(proposal_id, proposal, storage) in
    //let event = Tezos.emit "%sign_proposal" ({proposal_id; signer = owner; agreement } : Event.Types.sign_proposal) in
    //([event], storage)
    ([],storage)

(**
 * Proposal Execution
 *)

let resolve_proposal
  ( proposal_id, proposal_contents, storage
      : proposal_id
      * proposal_content list
      * storage_types)
  : result =
    //let () = Conditions.only_owner storage in
    //let () = Conditions.amount_must_be_zero_tez (Tezos.get_amount ()) in
    //let proposal = Storage.Op.retrieve_proposal(proposal_id, storage) in
    //let () = Conditions.check_proposals_content proposal_contents proposal.contents in
    //let expiration_time = proposal.proposer.timestamp + storage.effective_period in
    //let proposal = Storage.Op.update_proposal_state (proposal, storage.owners, storage.threshold, expiration_time) in
    //let () = Conditions.ready_to_execute proposal.state in
    //let storage = Storage.Op.update_proposal(proposal_id, proposal, storage) in
    //let ops, storage = Execution.perform_operations proposal_id proposal storage in
    //let event = Tezos.emit "%resolve_proposal" ({ proposal_id ; proposal_state = proposal.state } : Event.Types.resolve_proposal) in
    //let archive = Tezos.emit "%archive_proposal" ({ proposal_id ; proposal = Bytes.pack proposal }: Event.Types.archive_proposal ) in
    //(event::archive::ops, storage)
    ([],storage)

let proof_of_event_challenge (payload, storage : payload * storage_types) : result =
  //create_proposal ([Proof_of_event {payload}], storage)
  ([],storage)

let contract (action : parameter_types) (storage : storage_types) : result =
    let ops, storage =
      match action with
      | Default u -> default (u, storage)
      | Create_proposal { proposal_contents } ->
          create_proposal (proposal_contents, storage)
      | Sign_proposal { proposal_id; proposal_contents; voting } ->
          sign_proposal (proposal_id, proposal_contents, voting, storage)
      | Resolve_proposal { proposal_id; proposal_contents } ->
          resolve_proposal (proposal_id, proposal_contents, storage)
      | Proof_of_event_challenge { payload } ->
          proof_of_event_challenge (payload, storage)
    in
    //let () = Conditions.check_setting storage in
    (ops, storage)
