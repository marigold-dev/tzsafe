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
#import "proposal_content.mligo" "Proposal_content"
#import "parameter.mligo" "Parameter"
#import "storage.mligo" "Storage"

type storage_types = Storage.Types.t
type storage_wallet = Storage.Types.wallet
type storage_types_proposal = Storage.Types.proposal
type storage_types_proposal_state = Storage.Types.proposal_state
type effective_period = Storage.Types.effective_period
type proposal_content = Proposal_content.Types.t

[@inline]
let only_owner (type a) (wallet : a storage_wallet) : unit =
    assert_with_error (Set.mem (Tezos.get_sender ()) wallet.owners) Errors.only_owner

[@inline]
let amount_must_be_zero_tez (an_amout : tez) : unit =
    assert_with_error (an_amout = 0tez) Errors.amount_must_be_zero_tez

[@inline]
let unsigned (type a) (proposal : a storage_types_proposal) : unit =
    assert_with_error (not Map.mem (Tezos.get_sender ()) proposal.signatures) Errors.has_already_signed

[@inline]
let ready_to_execute (state : storage_types_proposal_state) : unit =
    assert_with_error (not (state = (Proposing : storage_types_proposal_state))) Errors.no_enough_signature_to_resolve

[@inline]
let unresolved (state : storage_types_proposal_state) : unit =
    assert_with_error (state = (Proposing : storage_types_proposal_state)) Errors.unresolved

[@inline]
let check_proposal (type a) (content: a proposal_content) : unit =
    match content with
    | Transfer t ->
        assert_with_error (not (t.amount = 0tez)) Errors.amount_is_zero
    | Execute _ -> ()
    | Execute_lambda e ->
        assert_with_error (Util.is_some e.lambda) Errors.no_proposal
    | Adjust_threshold t ->
        assert_with_error (t > 0n) Errors.invalidated_threshold
    | Add_owners s ->
        assert_with_error (Set.cardinal s > 0n) Errors.no_owners
    | Remove_owners s ->
        assert_with_error (Set.cardinal s > 0n) Errors.no_owners
    | Adjust_effective_period p ->
        assert_with_error (p > 0) Errors.invalid_effective_period

[@inline]
let not_empty_content (type a) (proposals_content: (a proposal_content) list) : unit =
    let () = assert_with_error ((List.length proposals_content) > 0n) Errors.no_proposal in
    List.iter check_proposal proposals_content

[@inline]
let check_setting (type a) (wallet : a storage_wallet) : unit =
    let () = assert_with_error (Set.cardinal wallet.owners > 0n) Errors.no_owner  in
    let () = assert_with_error (Set.cardinal wallet.owners >= wallet.threshold) Errors.no_enough_owner in
    let () = assert_with_error (wallet.threshold > 0n) Errors.invalidated_threshold in
    let () = assert_with_error (wallet.effective_period > 0) Errors.invalid_effective_period in
    ()

[@inline]
let check_proposals_content (type a) (from_input: (a proposal_content) list) (from_wallet: (a proposal_content) list) : unit =
  let pack_from_input = Bytes.pack from_input in
  let pack_from_wallet = Bytes.pack from_wallet in
  assert_with_error (pack_from_input = pack_from_wallet) Errors.not_the_same_content

[@inline]
let within_expiration_time (created_timestamp: timestamp) (effective_period: effective_period) : unit =
  assert_with_error (created_timestamp + effective_period > Tezos.get_now ()) Errors.pass_expiration_time
