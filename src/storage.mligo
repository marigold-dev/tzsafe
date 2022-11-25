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

#import "parameter.mligo" "Parameter"
#import "./common/errors.mligo" "Errors"

module Types = struct
    type proposal_id = Parameter.Types.proposal_id

    type 'a proposal_params =
    [@layout:comb]
    {
        approved_signers: address set;
        proposer: address;
        executed: bool;
        number_of_signer: nat;
        target: address;
        parameter: 'a;
        amount: tez;
        timestamp: timestamp;
    }

    type 'a proposal =
    | Transfer of unit proposal_params
    | Execute of ('a proposal_params)

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
    type raw_proposal = Parameter.Types.raw_proposal
    type raw_proposal_params = Parameter.Types.raw_proposal_params
    type proposal_id = Parameter.Types.proposal_id
    type proposal = Types.proposal
    type proposal_params = Types.proposal_params
    type types = Types.t

    [@inline]
    let create_proposal_by (type a) (params: a raw_proposal_params) : a proposal_params =
        {
            approved_signers = Set.empty;
            proposer         = Tezos.get_sender ();
            executed         = false;
            number_of_signer = 0n;
            target           = params.target;
            timestamp        = (Tezos.get_now ());
            parameter        = params.parameter;
            amount           = params.amount;
        }

    [@inline]
    let create_proposal (type a) (raw_proposal: a raw_proposal) : a proposal =
        match raw_proposal with
        | Raw_transfer param -> Transfer (create_proposal_by param)
        | Raw_execute param -> Execute (create_proposal_by param)

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
    let add_signer_to_proposal_by (type a) (proposal, signer, threshold: a proposal_params * address * nat) : a proposal_params =
        let approved_signers : address set = Set.add signer proposal.approved_signers in
        let executed = Set.cardinal approved_signers >= threshold || proposal.executed in
        {
            proposal with
            approved_signers = approved_signers;
            number_of_signer = proposal.number_of_signer + 1n ;
            executed         = executed
        }

    [@inline]
    let add_signer_to_proposal (type a) (proposal, signer, threshold: a proposal * address * nat) : a proposal =
      match proposal with
      | Transfer p -> Transfer (add_signer_to_proposal_by (p, signer, threshold))
      | Execute p  -> Execute (add_signer_to_proposal_by (p, signer, threshold))

    [@inline]
    let update_proposal (type a) (proposal_number, proposal, storage: proposal_id * a proposal * a types) : a types =
        let proposal_map = Big_map.update proposal_number (Some proposal) storage.proposal_map in
        {
            storage with
            proposal_map = proposal_map
        }
end
