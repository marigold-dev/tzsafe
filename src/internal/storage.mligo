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
    type challenge_id = Parameter.Types.challenge_id
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

    type proposal =
    [@layout:comb]
    {
        state: proposal_state;
        signatures: (address, bool) map;
        proposer : actor;
        resolver : actor option;
        contents : proposal_content list
    }

    type t =
    [@layout:comb]
    {
        proposal_counter : nat;
        proposals        : (proposal_id, proposal) big_map;
        archives         : (proposal_id, proposal_state) big_map;
        owners           : address set;
        threshold        : nat;
        effective_period : effective_period;
        metadata         : (string, bytes) big_map;
    }
end

module Op = struct
    type proposal_content = Proposal_content.Types.t
    type challenge_id = Parameter.Types.challenge_id
    type proposal_id = Parameter.Types.proposal_id
    type agreement = Parameter.Types.agreement
    type proposal = Types.proposal
    type proposal_state = Types.proposal_state
    type effective_period = Types.effective_period
    type proposal_state = Types.proposal_state
    type types = Types.t

    let create_proposal (contents: proposal_content list) : proposal =
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


    let register_proposal (proposal, storage: proposal * types) : types =
        let proposal_counter = storage.proposal_counter + 1n in
        let proposals = Big_map.add proposal_counter proposal storage.proposals in
        {
            storage with
            proposals     = proposals;
            proposal_counter = proposal_counter
        }

    let retrieve_proposal (proposal_number, storage : proposal_id * types) : proposal =
        match Big_map.find_opt proposal_number storage.proposals with
        | Some proposal -> proposal
        | None ->
          begin
            match Big_map.find_opt proposal_number storage.archives with
            | Some _ -> failwith Errors.already_resolved
            | None -> failwith Errors.no_proposal_exist
          end

    let update_signature (proposal, owner, agreement: proposal * address * agreement) : proposal =
        {
            proposal with
            signatures = Map.update owner (Some agreement) proposal.signatures;
        }

    let ready_execution (proposal, approvals, threshold : proposal * nat * nat) : proposal =
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

    let reject_proposal (proposal, disapprovals, threshold : proposal * nat * nat) : proposal =
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

    let expire_proposal (proposal, expiration_time : proposal * timestamp) : proposal =
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

    let remove_invalid_signature (owners : address set) (proposal : proposal) : proposal =
      let aux ((acc, k): ((address, bool) map * address)) =
        match Map.find_opt k proposal.signatures with
        | None -> acc
        | Some v -> Map.add k v acc in
      { proposal with signatures = Set.fold aux owners Map.empty }

    let update_proposal_state (proposal, owners , threshold, expiration_time : proposal * address set * nat * timestamp) : proposal =
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

    let update_proposal (proposal_id , proposal, storage: proposal_id * proposal * types) : types =
        let proposals = Big_map.update proposal_id (Some proposal) storage.proposals in
        {
            storage with
            proposals = proposals
        }

    let archive_proposal (proposal_id, proposal_state, storage: proposal_id * proposal_state * types) : types =
        let proposals = Big_map.remove proposal_id storage.proposals in
        let archives = Big_map.add proposal_id proposal_state storage.archives in
        {
            storage with
            proposals = proposals;
            archives = archives
        }

    let update_metadata (key, value, storage: string * bytes option * types) : types =
        let metadata = Big_map.update key value storage.metadata in
        {
            storage with
            metadata = metadata
        }

    let adjust_threshold (threshold : nat) (storage : types) : types =
      { storage with threshold = threshold }

    let adjust_effective_period (effective_period: int) (storage : types) : types =
      { storage with effective_period = effective_period }

    let add_owners (owners: address set) (storage : types) : types =
      let add (set, s : address set * address) : address set = Set.add s set in
      { storage with owners = Set.fold add owners storage.owners }

    let remove_owners (owners: address set) (storage : types) : types =
      let remove (set, s : address set * address) : address set = Set.remove s set in
      { storage with owners = Set.fold remove owners storage.owners }
end
