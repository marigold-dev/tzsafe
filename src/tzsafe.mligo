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

#import "./wallet/parameter.mligo" "Parameter"
#import "./wallet/storage.mligo" "WStorage"
#import "./wallet/contract.mligo" "Contract"

#import "@ligo/fa/lib/main.mligo" "FA2"
#import "./fa2/storage.mligo" "FStorage"
#import "./fa2/total_supply.mligo" "Total_supply"
#import "./fa2/lock_table.mligo" "LockTable"
#import "./fa2/registered_lock_key.mligo" "LockKey"

(* type 'a parameter_types  *)
type parameter_types = Parameter.Types.t

(* type 'a storage_types *)
type storage = 
  { wallet : WStorage.Types.t;
    nft :  FStorage.t;
  }

module NFT = FA2.MultiAssetExtendable

type ret = operation list * storage

(* type ['a request] is an alias of ['a parameter_types * 'a storage_types]  *)
type request = Contract.request

[@entry]
let contract (parameter: parameter_types) (storage : storage)  : ret =
  let ops, s = Contract.contract parameter storage.wallet in
  ops, { storage with wallet = s}


type mint =
  { owner : address
  ; amount : nat
  ; token_id : nat
  }

[@entry]
let mint (mint : mint) (s : storage) : ret =
  let nft = s.nft in
  let sender = Tezos.get_sender () in
  let () = assert_with_error (sender = nft.extension.admin) "not an admin" in
  let {owner; amount; token_id} = mint in
  let () = NFT.Assertions.assert_token_exist nft.token_metadata token_id in
  let new_ledger = Big_map.update (owner, token_id) (Some amount) nft.ledger in
  let () = assert (Option.is_none (Big_map.find_opt (owner, token_id) nft.ledger)) in
  let nft = NFT.set_ledger nft new_ledger in
  let new_total_supply = Total_supply.update_supply nft.extension.total_supply token_id amount in
  let nft = FStorage.set_total_supply nft new_total_supply in
  [], { s with nft = nft}


let decrease_token_amount_for_user
  (ledger : NFT.ledger)
  (lock_key : LockKey.t)
  (locktable : LockTable.t)
  (from_ : address)
  (token_id : nat)
  (amount_ : nat)
: NFT.ledger =
  let balance_ = NFT.get_for_user ledger from_ token_id in
  let lock = LockKey.get_lock_amount lock_key from_ token_id locktable in
  let () = assert_with_error (balance_ - lock >= 0) NFT.Errors.ins_balance in
  let () = assert_with_error (abs (balance_ - lock) >= amount_) NFT.Errors.ins_balance in
  let balance_ = abs (balance_ - amount_) in
  let ledger = NFT.set_for_user ledger from_ token_id balance_ in
  ledger


[@entry]
let transfer (t: NFT.TZIP12.transfer) (s: storage) : ret =
  let nft = s.nft in
  let process_atomic_transfer
    (from_ : address)
    (ledger, t : NFT.ledger * NFT.TZIP12.atomic_trans) =
    let {
     to_;
     token_id;
     amount = amount_
    } = t in
    let () = NFT.assert_token_exist nft token_id in
    let () = NFT.assert_authorisation nft.operators from_ token_id in
    let ledger = decrease_token_amount_for_user ledger nft.extension.lock_keys nft.extension.lock_table from_ token_id amount_ in
    let ledger = NFT.increase_token_amount_for_user ledger to_ token_id amount_ in
    ledger in
  let process_single_transfer (ledger, t : NFT.ledger * NFT.TZIP12.transfer_from) =
    let {
     from_;
     txs
    } = t in
    let ledger = List.fold_left (process_atomic_transfer from_) ledger txs in
    ledger in
  let ledger = List.fold_left process_single_transfer nft.ledger t in
  let s = { s with nft = NFT.set_ledger nft ledger} in
  ([] : operation list), s

[@entry]
let balance_of (b: NFT.TZIP12.balance_of) (s: storage) : ret =
  let nft = s.nft in
  let ops, nft = NFT.balance_of b nft in
  ops, { s with nft = nft }

[@entry]
let update_operators (u: NFT.TZIP12.update_operators) (s: storage) : ret =
  let nft = s.nft in
  let ops, nft = NFT.update_operators  u nft in
  ops, { s with nft = nft}

[@entry]
let lock ((key, owner, token_id), amount : LockTable.lock_id * nat) (s: storage) : ret = 
  let nft = s.nft in
  let () = assert_with_error (Tezos.get_sender () = nft.extension.admin) "No an admin" in
  let new_lock = LockTable.lock nft.extension.lock_table key token_id owner amount in
  let nft = FStorage.set_lock_table nft new_lock in
  [], { s with nft = nft}

[@entry]
let unlock ((key, owner, token_id), amount : LockTable.lock_id * nat) (s: storage) : ret = 
  let nft = s.nft in
  let () = assert_with_error (Tezos.get_sender () = nft.extension.admin) "No an admin" in
  let new_lock = LockTable.unlock nft.extension.lock_table key token_id owner amount in
  let nft = FStorage.set_lock_table nft new_lock in
  [], { s with nft = nft}

[@entry]
let register_lock_key(key : LockTable.lock_key) (s: storage) : ret = 
  let nft = s.nft in
  let () = assert_with_error (Tezos.get_sender () = nft.extension.admin) "No an admin" in
  let new_lock_keys = LockKey.register_lock_key nft.extension.lock_keys key in
  let nft = FStorage.set_lock_keys nft new_lock_keys in
  [], { s with nft = nft }

[@entry]
let unregister_lock_key(key : LockTable.lock_key) (s: storage) : ret = 
  let nft = s.nft in
  let () = assert_with_error (Tezos.get_sender () = nft.extension.admin) "No an admin" in
  let new_lock_keys = LockKey.unregister_lock_key nft.extension.lock_keys key in
  let nft = FStorage.set_lock_keys nft new_lock_keys in
  [], { s with nft = nft }