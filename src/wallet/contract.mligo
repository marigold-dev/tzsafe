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
#import "fa2_interactor.mligo" "FA2"
#import "../storage.mligo" "GStorage"

type g_storage = GStorage.t

type parameter_types = Parameter.Types.t
type proposal_id = Parameter.Types.proposal_id
type payload = Parameter.Types.payload
type storage_types = Storage.Types.t
type storage_types_proposal = Storage.Types.proposal
type storage_types_proposal_state = Storage.Types.proposal_state
type proposal_content = Proposal_content.Types.t
type votes = Parameter.Types.votes

type request = parameter_types -> g_storage 
type result = operation list * g_storage 


(**
 * Default entrypoint
 *)
let default (_, s : unit * g_storage) : result =
    let event = Tezos.emit "%receiving_tez" ({ from = Tezos.get_sender (); amount = Tezos.get_amount (); } : Event.Types.receiving_tez) in
    ([event], s)

(**
 * Proposal creation
 *)
[@inline]
let create_proposal (proposal_contents, storage : proposal_content list * g_storage) : result =
    let {wallet; fa2;} = storage in
    let {token_id} = wallet.token in
    let _tokens = Conditions.check_ownership token_id (Tezos.get_sender ()) fa2 in
    let () = Conditions.amount_must_be_zero_tez (Tezos.get_amount ()) in
    let () = Conditions.not_empty_content proposal_contents in
    let proposal = Storage.Op.create_proposal (proposal_contents) in
    let wallet = Storage.Op.register_proposal(proposal, wallet) in
    let proposal_id = wallet.proposal_counter in
    let event = Tezos.emit "%create_proposal" ({ proposal_id } : Event.Types.create_proposal) in
    let fa2 = FA2.call_register_lock_key fa2 proposal_id in
    ([event], {wallet; fa2})

(**
 * Proposal Signing
 *)

let sign_proposal
  ( proposal_id, proposal_contents, votes, storage
      : proposal_id
      * proposal_content list
      * votes
      * g_storage)
  : result =
    //let { token_id} = storage.nft in
    //let owner = Tezos.get_sender () in
    //let tokens = Conditions.check_ownership token_id addr in
    //let () = Conditions.amount_must_be_zero_tez (Tezos.get_amount ()) in
    //let {vote = _; quantity} = votes in
    //let () = Conditions.sufficient_token tokens quantity in
    //let proposal = Storage.Op.retrieve_proposal (proposal_id, storage) in
    //let () = Conditions.within_voting_time proposal.proposer.timestamp storage.voting_duration in
    //let () = Conditions.check_proposals_content proposal_contents proposal.contents in
    //let ops, proposal =
    //    if Storage.Op.in_voting_history (proposal_id, addr, storage.voting_history) then
    //        let history = Storage.Op.get_voting_history (proposal_id, addr, storage.voting_history) in
    //        let {vote = _; quantity = h_quantity} = history in
    //        let proposal =  Storage.Op.adjust_votes (proposal, votes, (Some history)) in

    //        let op1 = FA2.call_unlock addr proposal_id token_id owner h_quantity in
    //        let op2 = FA2.call_lock addr proposal_id token_id owner quantity in
    //        [op1; op2], proposal 
    //    else 
    //        let op = FA2.call_lock addr proposal_id token_id owner quantity in
    //        let proposal = Storage.Op.adjust_votes (proposal, votes, None) in
    //        [op], proposal
    //in
    //let storage = Storage.Op.update_voting_history(proposal_id, owner, votes, storage) in
    //let storage = Storage.Op.update_proposal(proposal_id, proposal, storage) in
    //let event = Tezos.emit "%sign_proposal" ({proposal_id; signer = owner} : Event.Types.sign_proposal) in
    //(event::ops, storage)
    ([], storage)

(**
 * Proposal Execution
 *)

let resolve_proposal
  ( proposal_id, proposal_contents, storage
      : proposal_id
      * proposal_content list
      * g_storage)
  : result =
    //let { token_id} = storage.nft in
    //let _tokens = Conditions.check_ownership token_id addr in
    //let () = Conditions.amount_must_be_zero_tez (Tezos.get_amount ()) in
    //let proposal = Storage.Op.retrieve_proposal(proposal_id, storage) in
    //let () = Conditions.check_proposals_content proposal_contents proposal.contents in
    //let () = Conditions.pass_voting_time proposal.proposer.timestamp storage.voting_duration in
    //let expiration_time = proposal.proposer.timestamp + storage.voting_duration + storage.execution_duration in
    //let supply = FA2.get_total_supply token_id addr in
    //let proposal = Storage.Op.update_proposal_state (proposal, storage.quorum, storage.supermajority, supply, expiration_time) in
    //let () = Conditions.ready_to_execute proposal.state in
    //let storage = Storage.Op.update_proposal(proposal_id, proposal, storage) in
    //let ops, storage = Execution.perform_operations proposal_id proposal storage in
    //let op = FA2.call_register_lock_key addr proposal_id in
    //let event = Tezos.emit "%resolve_proposal" ({ proposal_id ; proposal_state = proposal.state } : Event.Types.resolve_proposal) in
    //let archive = Tezos.emit "%archive_proposal" ({ proposal_id ; proposal = Bytes.pack proposal }: Event.Types.archive_proposal ) in
    //(event::archive::op::ops, storage)
    ([], storage)

// PoE is one special type of proposal
let proof_of_event_challenge (payload, storage : payload * g_storage) : result =
  create_proposal ([Proof_of_event {payload}], storage)

let contract (action : parameter_types) (storage : g_storage) : result =
    let ops, storage =
      match action with
      | Default u -> default (u, storage)
      | Create_proposal { proposal_contents; } ->
          create_proposal (proposal_contents, storage)
      | Sign_proposal { proposal_id; proposal_contents; votes } ->
          sign_proposal (proposal_id, proposal_contents, votes, storage)
      | Resolve_proposal { proposal_id; proposal_contents } ->
          resolve_proposal (proposal_id, proposal_contents, storage)
      | Proof_of_event_challenge { payload } ->
          proof_of_event_challenge (payload, storage)
    in
    let () = Conditions.check_setting storage.wallet in
    (ops, storage)
