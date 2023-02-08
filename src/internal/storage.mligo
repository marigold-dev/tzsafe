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
    type effective_period = int

    type actor =
    [@layout:comb]
    {
      actor     : address;
      timestamp : timestamp;
    }

    type proposal_state = Proposing | Executed | Rejected | Expired

    type 'a proposal =
    [@layout:comb]
    {
        state: proposal_state;
        signatures: (address, bool) map;
        proposer : actor;
        resolver : actor option;
        contents : ('a proposal_content) list
    }

    type 'a t =
    [@layout:comb]
    {
        proposal_counter : nat;
        proposals        : (proposal_id, 'a proposal) big_map;
        owners           : address set;
        threshold        : nat;
        effective_period : effective_period;
        metadata         : (string, bytes) big_map;
    }
end

module Op = struct
    type proposal_content = Proposal_content.Types.t
    type proposal_id = Parameter.Types.proposal_id
    type agreement = Parameter.Types.agreement
    type proposal = Types.proposal
    type proposal_state = Types.proposal_state
    type effective_period = Types.effective_period
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
            contents         = contents;
        }


    [@inline]
    let register_proposal (type a) (proposal, storage: a proposal * a types) : a types =
        let proposal_counter = storage.proposal_counter + 1n in
        let proposals = Big_map.add proposal_counter proposal storage.proposals in
        {
            storage with
            proposals     = proposals;
            proposal_counter = proposal_counter
        }

    [@inline]
    let retrieve_proposal (type a) (proposal_number, storage : proposal_id * a types) : a proposal =
        match Big_map.find_opt proposal_number storage.proposals with
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
    let reject_proposal (type a) (proposal, disapprovals, threshold : a proposal * nat * nat) : a proposal =
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
    let expire_proposal (type a) (proposal, expiration_time : a proposal * timestamp) : a proposal =
      if expiration_time < Tezos.get_now () then
          {
              proposal with
              state    = Expired;
              resolver =
                Some {
                  actor = Tezos.get_sender ();
                  timestamp = Tezos.get_now ()
                };
          }
      else proposal

    [@inline]
    let remove_invalid_signature (type a) (owners : address set) (proposal : a proposal) : a proposal =
      let aux ((acc, k): ((address, bool) map * address)) =
        match Map.find_opt k proposal.signatures with
        | None -> acc
        | Some v -> Map.add k v acc in
      { proposal with signatures = Set.fold aux owners Map.empty }

    [@inline]
    let update_proposal_state (type a) (proposal, owners , threshold, expiration_time : a proposal * address set * nat * timestamp) : a proposal =
        let proposal = expire_proposal (proposal, expiration_time) in
        if proposal.state = (Expired : proposal_state) then proposal
        else
          let proposal = remove_invalid_signature owners proposal in
          let statistic ((t_acc,f_acc), (_, v) : (nat * nat) * (address * bool)) : (nat * nat) =
            if v then (t_acc + 1n, f_acc) else (t_acc, f_acc + 1n) in
          let (approvals, disapprovals) = Map.fold statistic proposal.signatures (0n, 0n) in
          let proposal = ready_execution (proposal, approvals, threshold) in
          let number_of_owners = Set.cardinal owners in
          reject_proposal (proposal, disapprovals, abs(number_of_owners - threshold))

    [@inline]
    let update_proposal (type a) (proposal_number, proposal, storage: proposal_id * a proposal * a types) : a types =
        let proposals = Big_map.update proposal_number (Some proposal) storage.proposals in
        {
            storage with
            proposals = proposals
        }

    [@inline]
    let adjust_threshold (type a) (threshold : nat) (storage : a types) : a types =
      { storage with threshold = threshold }

    [@inline]
    let adjust_effective_period (type a) (effective_period: int) (storage : a types) : a types =
      { storage with effective_period = effective_period }

    [@inline]
    let add_owners (type a) (owners: address set) (storage : a types) : a types =
      let add (set, s : address set * address) : address set = Set.add s set in
      { storage with owners = Set.fold add owners storage.owners }

    [@inline]
    let remove_owners (type a) (owners: address set) (storage : a types) : a types =
      let remove (set, s : address set * address) : address set = Set.remove s set in
      { storage with owners = Set.fold remove owners storage.owners }
end
