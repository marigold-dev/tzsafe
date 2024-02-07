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
    type voting_histiry = ((proposal_id * address), votes) big_map

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
        voting_history     : voting_histiry;
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
    type voting_option = Parameter.Types.voting_option
    type agreement = Parameter.Types.agreement
    type proposal = Types.proposal
    type proposal_state = Types.proposal_state
    type voting_duration = Types.voting_duration
    type execution_duration = Types.execution_duration
    type proposal_state = Types.proposal_state
    type voting_history = Types.voting_histiry
    type types = Types.t

    [@inline]
    let create_proposal (contents: proposal_content list) : proposal =
        let new_votes = Map.literal [(Yes, 0n); (No, 0n); (Abstention, 0n)] in
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

    [@inline]
    let register_proposal (proposal, storage: proposal * types) : types =
        let proposal_counter = storage.proposal_counter + 1n in
        let proposals = Big_map.add proposal_counter proposal storage.proposals in
        {
            storage with
            proposals     = proposals;
            proposal_counter = proposal_counter
        }

    [@inline]
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
    let adjust_votes (proposal, {vote; quantity}, history: proposal * votes * votes option) : proposal =
      let votes =
        match Map.find_opt vote proposal.votes with
        | Some a -> Map.update vote (Some (a + quantity)) proposal.votes
        | None -> failwith "Vote option not found"
      in
      let votes = 
        match history with
        | Some {vote = hv; quantity = hq} ->
            begin
              match Map.find_opt hv votes with
              | Some a -> Map.update vote (Some (abs (a - hq))) proposal.votes
              | None -> failwith "Vote option not found"
            end
        | None -> votes
      in
      { proposal with votes = votes }

    let in_voting_history (proposal_id, addr, voting_history: proposal_id * address * voting_history) : bool =
      Option.is_some (Big_map.find_opt (proposal_id, addr) voting_history) 

    let get_voting_history (proposal_id, addr, voting_history: proposal_id * address * voting_history) : votes =
      match Big_map.find_opt (proposal_id, addr) voting_history with
      | Some v -> v
      | None -> failwith "no history"
    
    let update_voting_history ((proposal_id, voter_address, new_votes, s) : proposal_id * address * votes * types) : types =
      let key = (proposal_id, voter_address) in
      let updated_voting_history = Big_map.update key (Some new_votes) s.voting_history in
      { s with voting_history = updated_voting_history}

    let resolve (proposal, is_executed: proposal * bool) : proposal =
      let state =
        if is_executed then
          Executed
        else
          Rejected
      in
      {
          proposal with
          state    = state;
          resolver =
            Some {
              actor = Tezos.get_sender ();
              timestamp = Tezos.get_now ()
            };
      }

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

    let is_quorum_met (total_votes : nat) (votes : (voting_option, nat) map) (quorum_requirement : nat) : bool =
      let folded = fun ((votes_count : nat), (_, (v : nat))) -> v + votes_count in
      let total_votes_cast = Map.fold folded votes 0n in
      let required_votes_for_quorum = (total_votes * quorum_requirement) in
      total_votes_cast * 100n >= required_votes_for_quorum
    
    let is_supermajority_met (votes: (voting_option, nat) map) (supermajority_requirement : nat) : bool =
      let yes_votes = match Map.find_opt Yes votes with
        | Some(v) -> v
        | None -> failwith "No positive option, it should not happen"
      in
      let no_votes = match Map.find_opt No votes with
        | Some(v) -> v
        | None -> failwith "No negative option, it should not happen"
      in
      let total_votes_for_supermajority = yes_votes + no_votes in
      let required_yes_votes = (total_votes_for_supermajority * supermajority_requirement) in
      yes_votes * 100n >= required_yes_votes


    let update_proposal_state (proposal, quorum, supermajority, total_supply, expiration_time : proposal * nat * nat * nat * timestamp) : proposal =
        let proposal = expire_proposal (proposal, expiration_time) in
        if proposal.state = (Expired : proposal_state) then proposal
        else
          let is_executed = is_quorum_met total_supply proposal.votes quorum && is_supermajority_met proposal.votes supermajority in
          resolve (proposal, is_executed)

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
