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
    let () = Conditions.only_signer storage in
    let () = Conditions.amount_must_be_zero_tez (Tezos.get_amount ()) in
    let () = Conditions.check_proposals_content proposal_content in
    let proposal = Storage.Op.create_proposal proposal_content in
    let storage = Storage.Op.register_proposal(proposal, storage) in
    let event = Tezos.emit "%create_proposal" (storage.proposal_counter, proposal) in
    ([event], storage)

(**
 * Proposal signature
 *)

let sign_and_execute_proposal (type a) (proposal_id, agreement, storage : Parameter.Types.proposal_id * Parameter.Types.agreement * a storage_types) : a result =
    let () = Conditions.only_signer storage in
    let proposal = Storage.Op.retrieve_proposal(proposal_id, storage) in
    let () = Conditions.not_execute_yet proposal.executed in
    let () = Conditions.not_sign_yet proposal in
    let signer = Tezos.get_sender () in
    let proposal = Storage.Op.update_signature (proposal, signer, agreement) in
    let proposal = Storage.Op.update_proposal_state (proposal, Set.cardinal storage.signers,storage.threshold) in
    let storage = Storage.Op.update_proposal(proposal_id, proposal, storage) in
    let ops, storage = Execution.perform_operations proposal storage in
    let ops = Tezos.emit "%sign_proposal" (proposal_id, signer)::ops in
    if Util.is_some proposal.executed
    then (Tezos.emit "%execute_proposal" (proposal_id, signer)::ops, storage)
    else (ops, storage)

(**
 * Proposal signature only
 *)

let sign_proposal_only (type a) (proposal_id, agreement, storage : Parameter.Types.proposal_id * Parameter.Types.agreement * a storage_types) : a result =
    let () = Conditions.only_signer storage in
    let proposal = Storage.Op.retrieve_proposal(proposal_id, storage) in
    let () = Conditions.not_execute_yet proposal.executed in
    let () = Conditions.not_sign_yet proposal in
    let signer = Tezos.get_sender () in
    let proposal = Storage.Op.update_signature (proposal, signer, agreement) in
    let storage = Storage.Op.update_proposal(proposal_id, proposal, storage) in
    let event = Tezos.emit "%sign_proposal" (proposal_id, signer) in
    ([event], storage)

(**
 * Proposal Execution
 *)

let execute_proposal (type a) (proposal_id, storage : Parameter.Types.proposal_id * a storage_types) : a result =
    let () = Conditions.only_signer storage in
    let proposal = Storage.Op.retrieve_proposal(proposal_id, storage) in
    let () = Conditions.not_execute_yet proposal.executed in
    let signer = Tezos.get_sender () in
    let proposal = Storage.Op.update_proposal_state (proposal, Set.cardinal storage.signers, storage.threshold) in
    let () = Conditions.ready_to_execute proposal.executed in
    let storage = Storage.Op.update_proposal(proposal_id, proposal, storage) in
    let ops, s = Execution.perform_operations proposal storage in
    let event = Tezos.emit "%execute_proposal" (proposal_id, signer) in
    (event::ops, s)

let contract (type a) (action, storage : a request) : a result =
    let _ = Conditions.check_setting storage in
    match action with
    | Default u -> default (u, storage)
    | Create_proposal proposal_params ->
        create_proposal (proposal_params, storage)
    | Sign_and_execute_proposal (proposal_id, agreement) ->
        sign_and_execute_proposal (proposal_id, agreement, storage)
    | Sign_proposal_only (proposal_id, agreement) ->
        sign_proposal_only (proposal_id, agreement, storage)
    | Execute_proposal proposal_id ->
        execute_proposal (proposal_id, storage)
