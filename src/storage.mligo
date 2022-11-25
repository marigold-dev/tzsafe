#import "parameter.mligo" "Parameter"
#import "./common/errors.mligo" "Errors"
#import "./fa2/fa2.mligo" "FA2"


module Types = struct
    type proposal =
    [@layout:comb]
    {
        approved_signers: address set;
        executed: bool;
        number_of_signer: nat;
        target_fa2: address;
        transfers: FA2.transfer;
        timestamp: timestamp;
    }

    type t =
    [@layout:comb]
    {
        proposal_counter: nat;
        proposal_map    : (nat, proposal) big_map;
        signers         : address set;
        threshold       : nat;
        metadata        : (string, bytes) big_map;
    }
end

module Utils = struct
    [@inline]
    let new_storage (signers, threshold: address set * nat) : Types.t =
        {
            proposal_counter = 0n;
            proposal_map     = (Big_map.empty : (nat, Types.proposal) big_map);
            signers          = signers;
            threshold        = threshold;
            metadata         = (Big_map.empty: (string, bytes) big_map);
        }

    [@inline]
    let create_proposal (params: Parameter.Types.proposal_params) : Types.proposal =
        {
            approved_signers = Set.literal [(Tezos.get_sender ())];
            executed         = false;
            number_of_signer = 1n;
            target_fa2       = params.target_fa2;
            timestamp        = (Tezos.get_now ());
            transfers        = params.transfers;
        }

    [@inline]
    let register_proposal (proposal, storage: Types.proposal * Types.t) : Types.t =
        let proposal_counter = storage.proposal_counter + 1n in
        let proposal_map = Big_map.add proposal_counter proposal storage.proposal_map in
        {
            storage with
            proposal_map     = proposal_map;
            proposal_counter = proposal_counter
        }

    [@inline]
    let retrieve_proposal (proposal_number, storage : nat * Types.t) : Types.proposal =
        match Big_map.find_opt proposal_number storage.proposal_map with
        | None -> failwith Errors.no_proposal_exist
        | Some(proposal) -> proposal


    [@inline]
    let add_signer_to_proposal (proposal, signer, threshold: Types.proposal * address * nat) : Types.proposal =
        let approved_signers : address set = Set.add signer proposal.approved_signers in
        let executed = Set.cardinal approved_signers >= threshold || proposal.executed in
        {
            proposal with
            approved_signers = approved_signers;
            number_of_signer = proposal.number_of_signer + 1n ;
            executed         = executed
        }

    [@inline]
    let update_proposal (proposal_number, proposal, storage: Parameter.Types.proposal_number * Types.proposal * Types.t) : Types.t =
        let proposal_map = Big_map.update proposal_number (Some proposal) storage.proposal_map in
        {
            storage with
            proposal_map = proposal_map
        }
end
