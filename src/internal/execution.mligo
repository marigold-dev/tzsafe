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

type storage_types = Storage.Types.t
type storage_wallet = Storage.Types.wallet
type storage_types_proposal = Storage.Types.proposal
type storage_types_proposal_state = Storage.Types.proposal_state
type proposal_content = Proposal_content.Types.t

let send_by (type a) (parameter: a) (target : address) (amount : tez) : operation =
    [@no_mutation]
    let contract_opt : a contract option = Tezos.get_contract_opt target in
    let contract = Option.unopt_with_error contract_opt Errors.unknown_contract in
    Tezos.transaction parameter amount contract

let send (type a) (content : a proposal_content) (wallet: a storage_wallet)
  : (operation option * a proposal_content * a storage_wallet) =
    match content with
    | Transfer tx -> (Some (send_by tx.parameter tx.target tx.amount), content, wallet)
    | Execute tx -> (Some (send_by tx.parameter tx.target tx.amount), content, wallet)
    | Execute_lambda e ->
       let new_content = Execute_lambda { e with lambda = None } in
       (Option.map (fun (f : (unit -> operation)) : operation -> f ()) e.lambda, new_content, wallet)
    | Adjust_threshold t -> (None, content, Storage.Op.adjust_threshold t wallet)
    | Add_owners s -> (None, content, Storage.Op.add_owners s wallet)
    | Remove_owners s -> (None, content, Storage.Op.remove_owners s wallet)
    | Adjust_effective_period i -> (None, content, Storage.Op.adjust_effective_period i wallet)

let clear (type a) (content : a proposal_content) : (a proposal_content) =
    match content with
    | Transfer _ -> content
    | Execute _ -> content
    | Execute_lambda e -> Execute_lambda { e with lambda = None }
    | Adjust_threshold _ -> content
    | Add_owners _ -> content
    | Remove_owners _ -> content
    | Adjust_effective_period _ -> content

let perform_operations (type a) (proposal: a storage_types_proposal) (wallet : a storage_wallet) : operation list * a storage_types_proposal * a storage_wallet =
    let batch (type a) ((ops, cs, w), c : (operation list * a proposal_content list * a storage_wallet) * a proposal_content) : (operation list * a proposal_content list * a storage_wallet) =
      let (opt_op, new_c, new_s) = send c w in
      match opt_op with
      | Some op -> op::ops, new_c::cs, new_s
      | None ->
          ops, cs, new_s
    in
    match proposal.state with
    | Executed ->
      let (ops, cs, w) = List.fold_left batch (Constants.no_operation, [], wallet) proposal.contents in
      let ops = Util.reverse ops in
      let cs = Util.reverse cs in
      (ops, { proposal with contents = cs} , w)
    | Proposing -> (Constants.no_operation, proposal, wallet)
    | Rejected ->
      let proposal = { proposal with contents = List.map clear proposal.contents } in
      (Constants.no_operation, proposal, wallet)
    | Expired ->
      let proposal = { proposal with contents = List.map clear proposal.contents } in
      (Constants.no_operation, proposal, wallet)

