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
type storage_wallet = Storage.Types.wallet
type storage_tickets = Storage.Types.tickets
type storage_types_proposal = Storage.Types.proposal
type storage_types_proposal_state = Storage.Types.proposal_state
type effective_period = Storage.Types.effective_period
type proposal_content = Proposal_content.Types.t

type 'a request = 'a parameter_types * 'a storage_types
type 'a result = operation list * 'a storage_types


(**
 * Default entrypoint
 *)
let default (type a) (_, w, t : unit * a storage_wallet * a storage_tickets) : a result =
    let event = Tezos.emit "%receiving_tez" (Tezos.get_sender (), Tezos.get_amount ()) in
    ([event], ({wallet = w; tickets = t} : a storage_types))

(**
 * Ticket entrypoint
 *)
let ticket (type a) (t, w, ts : a ticket * a storage_wallet * a storage_tickets) : a result =
    let ticket_info,t  = Tezos.read_ticket t in
    let ts = Storage.Op.store_ticket (ts, t) in
    let event = Tezos.emit "%receiving_ticket" (Tezos.get_sender (), ticket_info) in
    ([event], ({wallet = w; tickets = ts} : a storage_types))

(**
 * Proposal creation
 *)
let create_proposal (type a) (proposal_content, wallet, tickets: (a proposal_content) list * a storage_wallet * a storage_tickets) : a result =
    let () = Conditions.only_owner wallet in
    let () = Conditions.amount_must_be_zero_tez (Tezos.get_amount ()) in
    let () = Conditions.not_empty_content proposal_content in
    let proposal = Storage.Op.create_proposal proposal_content in
    let wallet = Storage.Op.register_proposal(proposal, wallet) in
    let event = Tezos.emit "%create_proposal" (wallet.proposal_counter, proposal) in
    ([event], ({ wallet = wallet; tickets = tickets } : a storage_types))

(**
 * Proposal signature only
 *)

let sign_proposal (type a)
  ( proposal_id, proposal_content, agreement, wallet, tickets
    : Parameter.Types.proposal_id
      * (a proposal_content) list
      * Parameter.Types.agreement
      * a storage_wallet
      * a storage_tickets)
  : a result =
    let () = Conditions.only_owner wallet in
    let proposal = Storage.Op.retrieve_proposal(proposal_id, wallet) in
    let () = Conditions.unresolved proposal.state in
    let () = Conditions.unsigned proposal in
    let () = Conditions.within_expiration_time proposal.proposer.timestamp wallet.effective_period in
    let () = Conditions.check_proposals_content proposal_content proposal.contents in
    let owner = Tezos.get_sender () in
    let proposal = Storage.Op.update_signature (proposal, owner, agreement) in
    let wallet = Storage.Op.update_proposal(proposal_id, proposal, wallet) in
    let event = Tezos.emit "%sign_proposal" (proposal_id, owner, agreement) in
    ([event], ({ wallet = wallet; tickets = tickets } : a storage_types))

(**
 * Proposal Execution
 *)

let resolve_proposal (type a)
  ( proposal_id, proposal_content, wallet, tickets
      : Parameter.Types.proposal_id
      * (a proposal_content) list
      * a storage_wallet
      * a storage_tickets)
  : a result =
    let () = Conditions.only_owner wallet in
    let proposal = Storage.Op.retrieve_proposal(proposal_id, wallet) in
    let () = Conditions.unresolved proposal.state in
    let () = Conditions.check_proposals_content proposal_content proposal.contents in
    let owner = Tezos.get_sender () in
    let expiration_time = proposal.proposer.timestamp + wallet.effective_period in
    let proposal = Storage.Op.update_proposal_state (proposal, wallet.owners, wallet.threshold, expiration_time) in
    let () = Conditions.ready_to_execute proposal.state in
    let wallet = Storage.Op.update_proposal(proposal_id, proposal, wallet) in
    let ops, proposal, wallet, tickets = Execution.perform_operations proposal wallet tickets in
    let wallet = Storage.Op.update_proposal(proposal_id, proposal, wallet) in
    let event = Tezos.emit "%resolve_proposal" (proposal_id, owner) in
    (event::ops, ({ wallet = wallet; tickets = tickets } : a storage_types))

let contract (type a) (action, storage : a request) : a result =
    let { wallet; tickets } = storage in
    let ops, { wallet; tickets } =
      match action with
      | Default u ->
          default (u, wallet, tickets)
      | Ticket t ->
          ticket (t, wallet, tickets)
      | Create_proposal (proposal_params) ->
          create_proposal (proposal_params, wallet, tickets)
      | Sign_proposal (proposal_id, proposal_content, agreement) ->
          sign_proposal (proposal_id, proposal_content, agreement, wallet, tickets)
      | Resolve_proposal (proposal_id, proposal_content) ->
          resolve_proposal (proposal_id, proposal_content, wallet, tickets)
    in
    let storage = Conditions.check_setting (wallet, tickets) in
    (ops, storage)
