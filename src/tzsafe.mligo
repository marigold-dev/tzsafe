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

#import "./wallet/parameter.mligo" "WParameter"
#import "./wallet/storage.mligo" "WStorage"
#import "./wallet/contract.mligo" "WContract"

#import "@ligo/fa/lib/main.mligo" "FA2"
#import "./fa2/storage.mligo" "FStorage"
#import "./fa2/total_supply.mligo" "Total_supply"
#import "./fa2/lock_table.mligo" "LockTable"
#import "./fa2/registered_lock_key.mligo" "LockKey"

#import "./storage.mligo" "Storage"

type storage = Storage.t

module FA2 = FA2.MultiAssetExtendable

type ret = operation list * storage

[@entry]
let contract (parameter: WParameter.Types.t) (storage : storage)  : ret =
  let ops, s = WContract.contract parameter storage in
  ops, s


type mint =
  { owner : address
  ; amount : nat
  ; token_id : nat
  }

[@entry]
let mint (mint : mint) (s : storage) : ret =
  let fa2 = s.fa2 in
  let sender = Tezos.get_sender () in
  let () = assert_with_error (sender = fa2.extension.admin) "not an admin" in
  let {owner; amount; token_id} = mint in
  let () = FA2.Assertions.assert_token_exist fa2.token_metadata token_id in
  let new_ledger = Big_map.update (owner, token_id) (Some amount) fa2.ledger in
  let () = assert (Option.is_none (Big_map.find_opt (owner, token_id) fa2.ledger)) in
  let fa2 = FA2.set_ledger fa2 new_ledger in
  let new_total_supply = Total_supply.update_supply fa2.extension.total_supply token_id amount in
  let fa2 = FStorage.set_total_supply fa2 new_total_supply in
  [], { s with fa2 = fa2}


let decrease_token_amount_for_user
  (ledger : FA2.ledger)
  (lock_key : LockKey.t)
  (locktable : LockTable.t)
  (from_ : address)
  (token_id : nat)
  (amount_ : nat)
: FA2.ledger =
  let balance_ = FA2.get_for_user ledger from_ token_id in
  let lock = LockKey.get_lock_amount lock_key from_ token_id locktable in
  let () = assert_with_error (balance_ - lock >= 0) FA2.Errors.ins_balance in
  let () = assert_with_error (abs (balance_ - lock) >= amount_) FA2.Errors.ins_balance in
  let balance_ = abs (balance_ - amount_) in
  let ledger = FA2.set_for_user ledger from_ token_id balance_ in
  ledger


[@entry]
let transfer (t: FA2.TZIP12.transfer) (s: storage) : ret =
  let fa2 = s.fa2 in
  let process_atomic_transfer
    (from_ : address)
    (ledger, t : FA2.ledger * FA2.TZIP12.atomic_trans) =
    let {
     to_;
     token_id;
     amount = amount_
    } = t in
    let () = FA2.assert_token_exist fa2 token_id in
    let () = FA2.assert_authorisation fa2.operators from_ token_id in
    let ledger = decrease_token_amount_for_user ledger fa2.extension.lock_keys fa2.extension.lock_table from_ token_id amount_ in
    let ledger = FA2.increase_token_amount_for_user ledger to_ token_id amount_ in
    ledger in
  let process_single_transfer (ledger, t : FA2.ledger * FA2.TZIP12.transfer_from) =
    let {
     from_;
     txs
    } = t in
    let ledger = List.fold_left (process_atomic_transfer from_) ledger txs in
    ledger in
  let ledger = List.fold_left process_single_transfer fa2.ledger t in
  let s = { s with fa2 = FA2.set_ledger fa2 ledger} in
  ([] : operation list), s

[@entry]
let balance_of (b: FA2.TZIP12.balance_of) (s: storage) : ret =
  let fa2 = s.fa2 in
  let ops, fa2 = FA2.balance_of b fa2 in
  ops, { s with fa2 = fa2 }

[@entry]
let update_operators (u: FA2.TZIP12.update_operators) (s: storage) : ret =
  let fa2 = s.fa2 in
  let ops, fa2 = FA2.update_operators  u fa2 in
  ops, { s with fa2 = fa2}

[@entry]
let unregister_lock_key(key : LockTable.lock_key) (s: storage) : ret = 
  let fa2 = s.fa2 in
  let () = assert_with_error (Tezos.get_sender () = fa2.extension.admin) "No an admin" in
  let new_lock_keys = LockKey.unregister_lock_key fa2.extension.lock_keys key in
  let fa2 = FStorage.set_lock_keys fa2 new_lock_keys in
  [], { s with fa2 = fa2 }