#import "./common/constants.mligo" "Constants"
#import "parameter.mligo" "Parameter"
#import "storage.mligo" "Storage"
#import "conditions.mligo" "Conditions"
#import "contracts/fa2.mligo" "FA2"

// ===============================================================================================

module Preamble = struct
    [@inline]
    let prepare_new_proposal (params, storage: Parameter.Types.proposal_params * Storage.Types.t) : Storage.Types.proposal =
        let () = Conditions.only_signer storage in
        let () = Conditions.amount_must_be_zero_tez (Tezos.get_amount ()) in
        Storage.Utils.create_proposal params

    [@inline]
    let retrieve_a_proposal (proposal_number, storage: Parameter.Types.proposal_number * Storage.Types.t) : Storage.Types.proposal =
        let () = Conditions.only_signer storage in
        let target_proposal = Storage.Utils.retrieve_proposal(proposal_number, storage) in
        let () = Conditions.not_yet_signer target_proposal in
        target_proposal
end

// ===============================================================================================

type request = Parameter.Types.t * Storage.Types.t
type result = operation list * Storage.Types.t

(**
 * Proposal creation
 *)
let create_proposal (params, storage : Parameter.Types.proposal_params * Storage.Types.t) : result =
    let proposal = Preamble.prepare_new_proposal(params, storage) in
    let storage = Storage.Utils.register_proposal(proposal, storage) in
    (Constants.no_operation, storage)

(**
 * Proposal signature
 *)

 // LIGO INFO UNCURRIED FUNCTION DOESN'T WORK
let sign_proposal (proposal_number, storage : Parameter.Types.proposal_number * Storage.Types.t) : result =
    let proposal = Preamble.retrieve_a_proposal(proposal_number, storage) in

    let proposal = Storage.Utils.add_signer_to_proposal(proposal, (Tezos.get_sender ()), storage.threshold) in
    let storage = Storage.Utils.update_proposal(proposal_number, proposal, storage) in

    let operations = FA2.perform_operations proposal in

    (operations, storage)

// ===============================================================================================

let main (action, storage : request) : result =
    match action with
    | Create_proposal(proposal_params) ->
        create_proposal (proposal_params, storage)
    | Sign_proposal(proposal_number) ->
        sign_proposal (proposal_number, storage)
