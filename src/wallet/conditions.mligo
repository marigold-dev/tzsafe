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


#import "@ligo/fa/lib/main.mligo" "FA2"

#import "../common/errors.mligo" "Errors"
#import "../common/util.mligo" "Util"
#import "proposal_content.mligo" "Proposal_content"
#import "parameter.mligo" "Parameter"
#import "storage.mligo" "Storage"

#import "../fa2/storage.mligo" "FStorage"

module FA2 = FA2.MultiAssetExtendable

type storage_types = Storage.Types.t
type storage_types_proposal = Storage.Types.proposal
type storage_types_proposal_state = Storage.Types.proposal_state
type proposal_content = Proposal_content.Types.t

let amount_must_be_zero_tez (amount : tez) : unit =
    assert_with_error (amount = 0tez) Errors.amount_must_be_zero_tez

let ready_to_execute (state : storage_types_proposal_state) : unit =
    assert_with_error (not (state = (Proposing : storage_types_proposal_state))) "Can not resolve proposal"

let check_proposal (content: proposal_content) : unit =
    match content with
    | Transfer t ->
        assert_with_error (not (t.amount = 0tez)) Errors.amount_is_zero
    | Execute_lambda _ -> ()
    | Adjust_quorum q ->
        assert_with_error (q > 0n) Errors.invalid_quorum
    | Adjust_supermajority s ->
        assert_with_error (s > 0n) Errors.invalid_supermajority
    | Adjust_voting_duration v ->
        assert_with_error (v > 0) Errors.invalid_voting_period
    | Adjust_execution_duration e ->
        assert_with_error (e > 0) Errors.invalid_execution_period
    | Adjust_token _ -> ()
    | Add_or_update_metadata _ -> ()
    | Remove_metadata _ -> ()
    | Proof_of_event _ -> ()
    | Mint _ -> ()
    | Create_token _ -> ()

let not_empty_content (proposals_content: proposal_content list) : unit =
    let () = assert_with_error ((List.length proposals_content) > 0n) Errors.no_proposal in
    List.iter check_proposal proposals_content

let check_setting (storage : storage_types) : unit =
    let () = assert_with_error (storage.supermajority > 0n) "Invalid settings: supermajority" in
    let () = assert_with_error (storage.quorum > 0n) "Invalid settings: quorum" in
    let () = assert_with_error (storage.voting_duration > 0) "Invalid settings: voting_duration" in
    let () = assert_with_error (storage.execution_duration > 0) "Invalid settings: execution_duration" in
    ()

let check_proposals_content (from_input: proposal_content list) (from_storage: proposal_content list) : unit =
  let pack_from_input = Bytes.pack from_input in
  let pack_from_storage = Bytes.pack from_storage in
  assert_with_error (pack_from_input = pack_from_storage) Errors.not_the_same_content

let within_voting_time (created_timestamp: timestamp) (voting_duration: int) : unit =
  assert_with_error (created_timestamp + voting_duration > Tezos.get_now ()) Errors.pass_voting_time

let pass_voting_time (created_timestamp: timestamp) (voting_duration: int) : unit =
  assert_with_error (created_timestamp + voting_duration <= Tezos.get_now ()) "Current period is for voting"

let within_execution_time (created_timestamp: timestamp) (voting_duration: int) (execution_duration: int) : unit =
  let now = Tezos.get_now () in
  assert_with_error (created_timestamp + voting_duration <= now ||
   created_timestamp + voting_duration + execution_duration > now
   ) Errors.pass_voting_time

let check_ownership (token_id : nat) (addr : address) (fa2_s : FStorage.t): nat =
  let balance = FA2.get_balance (addr, token_id) fa2_s in
  if balance  <= 0n then
    failwith "Balance is non-positive"
  else
    balance

let sufficient_token (tokens : nat) (quantity : nat) : unit =
  if tokens < quantity then
    failwith "Error: tokens are less than votes"
  else
    ()