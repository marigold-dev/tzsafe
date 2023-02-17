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
type storage_tickets = Storage.Types.tickets
type storage_types_proposal = Storage.Types.proposal
type storage_types_proposal_state = Storage.Types.proposal_state
type proposal_content = Proposal_content.Types.t

let send_by (type a) (parameter: a) (target : address) (amount : tez) : operation =
    [@no_mutation]
    let contract_opt : a contract option = Tezos.get_contract_opt target in
    let contract = Option.unopt_with_error contract_opt Errors.unknown_contract in
    Tezos.transaction parameter amount contract

let store
  (type a)
  (tickets, t: a storage_tickets * a ticket)
  : a storage_tickets =
     let (addr,(content, _v)), _t = Tezos.read_ticket t in
     match Big_map.find_opt (content, addr) tickets with
     | None -> Big_map.add (content, addr) t tickets
     | Some s ->
        begin
          match Tezos.join_tickets (t, s) with
          | None -> failwith Errors.cannot_happen
          | Some new_t -> Big_map.update (content, addr) (Some new_t) tickets
        end

let execute_lambda_without_args
  (type a)
  (f:(a ticket) option -> operation * (a ticket) list)
  (tickets: a storage_tickets)
  : (operation option * a storage_tickets) =
  let (op, ts) = f (None : (a ticket) option) in
  Some op, List.fold_left store tickets ts

let execute_lambda_with_args
  (type a)
  (f:((a ticket) option -> operation * (a ticket) list))
  (content, addr, amount:(a * address * nat))
  (tickets: a storage_tickets)
  : (operation option * a storage_tickets) =
     match Big_map.find_opt (content, addr) tickets with
     | None -> failwith Errors.nonexisted_ticket
     | Some t ->
        let (_addr,(_content, value)), _t = Tezos.read_ticket t in
        let balance = Conditions.balance_must_be_positive amount value in
        let split_ticket_opt = Tezos.split_ticket t (amount, balance) in
        let (t1, t2) = Option.unopt split_ticket_opt in
        let (op, ts) = f (Some t1) in
        Some op, List.fold_left store tickets ts

let execute_lambda
  (type a)
  (f: ((a ticket) option -> operation * (a ticket) list))
  (args_opt :(a * address * nat) option)
  (tickets: a storage_tickets)
  : (operation option * a storage_tickets) =
  match args_opt with
  | None -> execute_lambda_without_args f tickets
  | Some args -> execute_lambda_with_args f args tickets

(* TODO: check the args of lambda *)
let send (type a)
  (content : a proposal_content)
  (wallet: a storage_wallet)
  (tickets: a storage_tickets)
  : (operation option * a proposal_content * a storage_wallet * a storage_tickets) =
    match content with
    | Transfer tx -> (Some (send_by tx.parameter tx.target tx.amount), content, wallet, tickets)
    | Execute tx -> (Some (send_by tx.parameter tx.target tx.amount), content, wallet, tickets)
    | Execute_lambda e ->
       let lambda = Option.unopt e.lambda in (* cannnot happend *)
       let (op_opt, ts) = execute_lambda lambda e.args tickets in
       let new_content = Execute_lambda { e with lambda = None } in
       (op_opt, new_content, wallet, ts)
    | Adjust_threshold t -> (None, content, Storage.Op.adjust_threshold t wallet, tickets)
    | Add_owners s -> (None, content, Storage.Op.add_owners s wallet, tickets)
    | Remove_owners s -> (None, content, Storage.Op.remove_owners s wallet, tickets)
    | Adjust_effective_period i -> (None, content, Storage.Op.adjust_effective_period i wallet, tickets)

let clear (type a) (content : a proposal_content) : (a proposal_content) =
    match content with
    | Transfer _ -> content
    | Execute _ -> content
    | Execute_lambda e -> Execute_lambda { e with lambda = None }
    | Adjust_threshold _ -> content
    | Add_owners _ -> content
    | Remove_owners _ -> content
    | Adjust_effective_period _ -> content

let perform_operations (type a)
  (proposal: a storage_types_proposal)
  (wallet : a storage_wallet)
  (tickets: a storage_tickets)
  : operation list * a storage_types_proposal * a storage_wallet * a storage_tickets =
    let batch
      (type a) 
      ((ops, cs, w, ts), c
        : (operation list * a proposal_content list * a storage_wallet * a storage_tickets) * a proposal_content)
      : (operation list * a proposal_content list * a storage_wallet * a storage_tickets) =
      let (opt_op, new_c, new_s, new_ts) = send c w ts in
      match opt_op with
      | Some op -> op::ops, new_c::cs, new_s, new_ts
      | None -> ops, cs, new_s, new_ts
    in
    match proposal.state with
    | Executed ->
      let (ops, cs, w, ts) =
        List.fold_left batch (Constants.no_operation, [], wallet, tickets) proposal.contents
      in
      let ops = Util.reverse ops in
      let cs = Util.reverse cs in
      (ops, { proposal with contents = cs} , w, ts)
    | Proposing -> (Constants.no_operation, proposal, wallet, tickets)
    | Rejected ->
      let proposal = { proposal with contents = List.map clear proposal.contents } in
      (Constants.no_operation, proposal, wallet, tickets)
    | Expired ->
      let proposal = { proposal with contents = List.map clear proposal.contents } in
      (Constants.no_operation, proposal, wallet, tickets)

