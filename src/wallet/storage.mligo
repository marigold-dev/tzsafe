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
    type voting_option = Parameter.Types.voting_option
    type votes = Parameter.Types.votes
    type proposal_content = Proposal_content.Types.t
    type voting_duration = int
    type execution_duration = int
    type supermajority = nat
    type quorum = nat

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
        votes: (voting_option, nat) map;
        proposer : actor;
        resolver : actor option;
        contents : proposal_content list
    }

    type t =
    [@layout:comb]
    {
        proposal_counter   : nat;
        proposals          : (proposal_id, proposal) big_map;
        archives           : (proposal_id, proposal_state) big_map;
        voting_history     : ((proposal_id * address), votes) big_map;
        nft                : (address * nat);
        supermajority      : supermajority;
        quorum             : quorum;
        voting_duration    : voting_duration;
        execution_duration : execution_duration;
        metadata           : (string, bytes) big_map;
    }
end

module Op = struct
    type proposal_content = Proposal_content.Types.t
    type proposal_id = Parameter.Types.proposal_id
    type votes = Parameter.Types.votes
    type voting_options = Parameter.Types.voting_options
    type agreement = Parameter.Types.agreement
    type proposal = Types.proposal
    type proposal_state = Types.proposal_state
    type voting_duration = Types.voting_duration
    type execution_duration = Types.execution_duration
    type proposal_state = Types.proposal_state
    type types = Types.t

    let create_proposal (contents, voting_options: proposal_content list * voting_options) : proposal =
        let aux (acc, v) = Map.add v 0n acc in
        let new_votes = Set.fold aux voting_options Map.empty in
        {
            state            = Proposing;
            votes            = new_votes;
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

    [@inline]
    let adjust_votes (proposal, {vote; quantity}, is_increased: proposal * votes * bool) : proposal =
        let update_votes =
          match Map.find_opt vote proposal.votes with
          | Some a ->
             if is_increased then
                Map.update vote (Some (a + quantity)) proposal.votes
             else
                Map.update vote (Some (abs(a - quantity))) proposal.votes
          | None -> failwith "Not an option"
        in
        {
            proposal with
            votes = update_votes;
        }

    //let ready_execution (proposal, approvals, threshold : proposal * nat * nat) : proposal =
    //    let is_executed = approvals >= threshold && proposal.state = (Proposing : proposal_state) in
    //    if is_executed
    //    then
    //      {
    //          proposal with
    //          state    = Executed;
    //          resolver =
    //            Some {
    //              actor = Tezos.get_sender ();
    //              timestamp = Tezos.get_now ()
    //            };
    //      }
    //    else proposal

    //let reject_proposal (proposal, disapprovals, threshold : proposal * nat * nat) : proposal =
    //    let is_closed = disapprovals > threshold && proposal.state = (Proposing : proposal_state) in
    //    if is_closed
    //    then
    //      {
    //          proposal with
    //          state    = Rejected;
    //          resolver =
    //            Some {
    //              actor = Tezos.get_sender ();
    //              timestamp = Tezos.get_now ()
    //            };
    //      }
    //    else proposal

    //let expire_proposal (proposal, expiration_time : proposal * timestamp) : proposal =
    //  if expiration_time < Tezos.get_now () then
    //      {
    //          proposal with
    //          state    = Expired;
    //          resolver =
    //            Some {
    //              actor = Tezos.get_sender ();
    //              timestamp = Tezos.get_now ()
    //            };
    //      }
    //  else proposal

    //let remove_invalid_signature (owners : address set) (proposal : proposal) : proposal =
    //  let aux ((acc, k): ((address, bool) map * address)) =
    //    match Map.find_opt k proposal.signatures with
    //    | None -> acc
    //    | Some v -> Map.add k v acc in
    //  { proposal with signatures = Set.fold aux owners Map.empty }

    //let update_proposal_state (proposal, owners , threshold, expiration_time : proposal * address set * nat * timestamp) : proposal =
    //    let proposal = expire_proposal (proposal, expiration_time) in
    //    if proposal.state = (Expired : proposal_state) then proposal
    //    else
    //      let proposal = remove_invalid_signature owners proposal in
    //      let statistic ((t_acc,f_acc), (_, v) : (nat * nat) * (address * bool)) : (nat * nat) =
    //        if v then (t_acc + 1n, f_acc) else (t_acc, f_acc + 1n) in
    //      let (approvals, disapprovals) = Map.fold statistic proposal.signatures (0n, 0n) in
    //      let proposal = ready_execution (proposal, approvals, threshold) in
    //      let number_of_owners = Set.cardinal owners in
    //      reject_proposal (proposal, disapprovals, abs(number_of_owners - threshold))

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

    let adjust_quorum (quorum : nat) (storage : types) : types =
      { storage with quorum = quorum }

    let adjust_supermajority (supermajority : nat) (storage : types) : types =
      { storage with supermajority = supermajority }

    let adjust_voting_duration (voting_duration: int) (storage : types) : types =
      { storage with voting_duration = voting_duration }

    let adjust_execution_duration (execution_duration: int) (storage : types) : types =
      { storage with execution_duration = execution_duration }
end
