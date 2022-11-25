#import "./common/errors.mligo" "Errors"
#import "storage.mligo" "Storage"

[@inline]
let only_signer (storage : Storage.Types.t) : unit = 
    assert_with_error (Set.mem (Tezos.get_sender ()) storage.signers) Errors.only_signer

[@inline]
let amount_must_be_zero_tez (an_amout : tez) : unit =
    assert_with_error (an_amout = 0tez) Errors.amount_must_be_zero_tez

[@inline]
let not_yet_signer (proposal : Storage.Types.proposal) : unit = 
    assert_with_error (not Set.mem (Tezos.get_sender ()) proposal.approved_signers) Errors.has_already_signed

