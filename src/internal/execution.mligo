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

#import "../common/errors.mligo" "Errors"
#import "../common/constants.mligo" "Constants"
#import "../common/util.mligo" "Util"
#import "proposal_content.mligo" "Proposal_content"
#import "./storage.mligo" "Storage"
#import "conditions.mligo" "Conditions"
#import "event.mligo" "Event"

type storage_types = Storage.Types.t
type storage_types_proposal_id = Storage.Types.proposal_id
type storage_types_proposal = Storage.Types.proposal
type storage_types_proposal_state = Storage.Types.proposal_state
type proposal_content = Proposal_content.Types.t

let send_by (target : address) (amount : tez) : operation =
    let contract_opt : unit contract option = Tezos.get_contract_opt target in
    let contract = Option.unopt_with_error contract_opt Errors.unknown_contract in
    Tezos.transaction () amount contract

let send (content : proposal_content) (storage : storage_types)
  : (operation list  * storage_types) =
    match content with
    | Transfer tx -> ([send_by tx.target tx.amount], storage)
    | Execute_lambda e -> (e.lambda (), storage)
    | Adjust_threshold t -> ([], Storage.Op.adjust_threshold t storage)
    | Add_owners s -> ([], Storage.Op.add_owners s storage)
    | Remove_owners s -> ([], Storage.Op.remove_owners s storage)
    | Adjust_effective_period i -> ([], Storage.Op.adjust_effective_period i storage)
    | Add_or_update_metadata { key; value } -> ([], Storage.Op.update_metadata (key, (Some value), storage))
    | Remove_metadata { key } -> ([], Storage.Op.update_metadata (key, None, storage))

let perform_operations
  (proposal_id: storage_types_proposal_id)
  (proposal: storage_types_proposal)
  (storage : storage_types)
  : operation list * storage_types =
    let batch ((ops, s), c : (operation list * storage_types) * proposal_content) : (operation list * storage_types) =
      let (new_ops, new_s) = send c s in
      let acc (x,ys : (operation * operation list)) : operation list = x :: ys in
      List.fold_right acc ops new_ops, new_s
    in
    match proposal.state with
    | Executed ->
      let (ops, s) = List.fold_left batch (Constants.no_operation, storage) proposal.contents in
      let new_s = Storage.Op.archive_proposal (proposal_id, Executed, s) in
      (ops, new_s)
    | Proposing ->
      (Constants.no_operation, storage)
    | Rejected ->
      let new_s = Storage.Op.archive_proposal (proposal_id, Rejected, storage) in
      (Constants.no_operation, new_s)
    | Expired ->
      let new_s = Storage.Op.archive_proposal (proposal_id, Expired, storage) in
      (Constants.no_operation, new_s)

