#import "../common/errors.mligo" "Errors"
#import "../common/constants.mligo" "Constants"
#import "../storage.mligo" "Storage"
#import "../fa2/fa2.mligo" "FA2"

let send (transfers : FA2.transfer) (target_fa2_address : address) : operation = 
    [@no_mutation] let fa2_contract_opt : FA2.transfer contract option = Tezos.get_entrypoint_opt "%transfer" target_fa2_address in
    match fa2_contract_opt with
    | Some contr -> Tezos.transaction (transfers) 0mutez contr
    | None -> (failwith Errors.unknown_contract_entrypoint : operation)

let perform_operations (proposal: Storage.Types.proposal) : operation list =
    if proposal.executed
    then [ send proposal.transfers proposal.target_fa2 ]
    else Constants.no_operation
