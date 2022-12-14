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
#import "parameter.mligo" "Parameter"
#import "proposal_content.mligo" "Proposal_content"

module Types = struct
    type proposal_id = Parameter.Types.proposal_id
    type proposal_content = Proposal_content.Types.t
    type view_proposal_content = Proposal_content.Types.view

    type 'a view_proposal =
    [@layout:comb]
    {
        approved_signers: address set;
        proposer: address;
        executed: bool;
        number_of_signer: nat;
        timestamp: timestamp;
        content : ('a view_proposal_content) list
    }

    type 'a proposal =
    [@layout:comb]
    {
        approved_signers: address set;
        proposer: address;
        executed: bool;
        number_of_signer: nat;
        timestamp: timestamp;
        content : ('a proposal_content) list
    }

    type 'a t =
    [@layout:comb]
    {
        proposal_counter: nat;
        proposal_map    : (proposal_id, 'a proposal) big_map;
        signers         : address set;
        threshold       : nat;
        metadata        : (string, bytes) big_map;
    }
end

module Op = struct
    type proposal_content = Proposal_content.Types.t
    type proposal_id = Parameter.Types.proposal_id
    type proposal = Types.proposal
    type types = Types.t

    [@inline]
    let create_proposal (type a) (contents: (a proposal_content) list) : a proposal =
        {
            approved_signers = Set.empty;
            proposer         = Tezos.get_sender ();
            executed         = false;
            number_of_signer = 0n;
            timestamp        = (Tezos.get_now ());
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
    let add_approval (type a) (proposal, signer: a proposal * address) : a proposal =
        let approved_signers : address set = Set.add signer proposal.approved_signers in
        {
            proposal with
            approved_signers = approved_signers;
            number_of_signer = proposal.number_of_signer + 1n ;
        }

    [@inline]
    let update_execution_flag (type a) (proposal, threshold: a proposal * nat) : a proposal =
        let executed = Set.cardinal proposal.approved_signers >= threshold || proposal.executed in
        {
            proposal with
            executed         = executed
        }


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
    let add_signers (type a) (signers: address set) (storage : a types) : a types =
      let add (set, s : address set * address) : address set = Set.add s set in
      { storage with signers = Set.fold add signers storage.signers }

    [@inline]
    let remove_signers (type a) (signers: address set) (storage : a types) : a types =
      let remove (set, s : address set * address) : address set = Set.remove s set in
      { storage with signers = Set.fold remove signers storage.signers }
end
