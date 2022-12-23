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
#import "../common/util.mligo" "Util"
#import "parameter.mligo" "Parameter"
#import "proposal_content.mligo" "Proposal_content"

module Types = struct
    type proposal_id = Parameter.Types.proposal_id
    type proposal_content = Proposal_content.Types.t
    type view_proposal_content = Proposal_content.Types.view

    type actor =
    [@layout:comb]
    {
      actor     : address;
      timestamp : timestamp;
    }

    type proposal_state = Proposing | Executed | Rejected

    type 'a view_proposal =
    [@layout:comb]
    {
        state: proposal_state;
        signatures: (address, bool) map;
        proposer : actor;
        resolver : actor option;
        content : ('a view_proposal_content) list
    }

    type 'a proposal =
    [@layout:comb]
    {
        state: proposal_state;
        signatures: (address, bool) map;
        proposer : actor;
        resolver : actor option;
        content : ('a proposal_content) list
    }

    type 'a t =
    [@layout:comb]
    {
        proposal_counter: nat;
        proposal_map    : (proposal_id, 'a proposal) big_map;
        owners          : address set;
        threshold       : nat;
        metadata        : (string, bytes) big_map;
    }
end

module Op = struct
    type proposal_content = Proposal_content.Types.t
    type proposal_id = Parameter.Types.proposal_id
    type agreement = Parameter.Types.agreement
    type proposal = Types.proposal
    type proposal_state = Types.proposal_state
    type types = Types.t

    [@inline]
    let create_proposal (type a) (contents: (a proposal_content) list) : a proposal =
        {
            state            = Proposing;
            signatures       = Map.empty;
            proposer         =
              {
                actor = Tezos.get_sender ();
                timestamp = Tezos.get_now ()
              };
            resolver         = None;
            content          = contents;
        }


    [@inline]
    let register_proposal (type a) (proposal, storage: a proposal * a types) : a types =
        let proposal_counter = storage.proposal_counter + 1n in
        let proposal_map = Big_map.add proposal_counter proposal storage.proposal_map in
        {
            storage with
            proposal_map     = proposal_map;
            proposal_counter = proposal_counter
        }

    [@inline]
    let retrieve_proposal (type a) (proposal_number, storage : proposal_id * a types) : a proposal =
        match Big_map.find_opt proposal_number storage.proposal_map with
        | None -> failwith Errors.no_proposal_exist
        | Some proposal  -> proposal


    [@inline]
    let update_signature (type a) (proposal, owner, agreement: a proposal * address * agreement) : a proposal =
        {
            proposal with
            signatures = Map.update owner (Some agreement) proposal.signatures;
        }

    [@inline]
    let ready_execution (type a) (proposal, approvals, threshold : a proposal * nat * nat) : a proposal =
        let is_executed = approvals >= threshold && proposal.state = (Proposing : proposal_state) in
        if is_executed
        then
          {
              proposal with
              state    = Executed;
              resolver =
                Some {
                  actor = Tezos.get_sender ();
                  timestamp = Tezos.get_now ()
                };
          }
        else proposal

    [@inline]
    let close_proposal (type a) (proposal, disapprovals, threshold : a proposal * nat * nat) : a proposal =
        let is_closed = disapprovals > threshold && proposal.state = (Proposing : proposal_state) in
        if is_closed
        then
          {
              proposal with
              state    = Rejected;
              resolver =
                Some {
                  actor = Tezos.get_sender ();
                  timestamp = Tezos.get_now ()
                };
          }
        else proposal

    [@inline]
    let update_proposal_state (type a) (proposal, number_of_owners, threshold: a proposal * nat * nat) : a proposal =
        let statistic ((t_acc,f_acc), (_, v) : (nat * nat) * (address * bool)) : (nat * nat) =
          if v then (t_acc + 1n, f_acc) else (t_acc, f_acc + 1n) in
        let (approvals, disapprovals) = Map.fold statistic proposal.signatures (0n, 0n) in
        let proposal = ready_execution (proposal, approvals, threshold) in
        close_proposal (proposal, disapprovals, abs(number_of_owners - threshold))

    [@inline]
    let update_proposal (type a) (proposal_number, proposal, storage: proposal_id * a proposal * a types) : a types =
        let proposal_map = Big_map.update proposal_number (Some proposal) storage.proposal_map in
        {
            storage with
            proposal_map = proposal_map
        }

    [@inline]
    let adjust_threshold (type a) (threshold : nat) (storage : a types) : a types =
      { storage with threshold = threshold }

    [@inline]
    let add_owners (type a) (owners: address set) (storage : a types) : a types =
      let add (set, s : address set * address) : address set = Set.add s set in
      { storage with owners = Set.fold add owners storage.owners }

    [@inline]
    let remove_owners (type a) (owners: address set) (storage : a types) : a types =
      let remove (set, s : address set * address) : address set = Set.remove s set in
      { storage with owners = Set.fold remove owners storage.owners }
end
