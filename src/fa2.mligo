#import "@ligo/fa/lib/main.mligo" "FA2"
#import "./fa2/storage.mligo" "Storage"
#import "./fa2/total_supply.mligo" "Total_supply"
#import "./fa2/lock_table.mligo" "LockTable"
#import "./fa2/registered_lock_key.mligo" "LockKey"

module NFT = FA2.MultiAssetExtendable

type storage = Storage.t

type ret = operation list * storage

type mint =
  { owner : address
  ; amount : nat
  ; token_id : nat
  }

[@entry]
let mint (mint : mint) (s : storage) : ret =
  let sender = Tezos.get_sender () in
  let () = assert_with_error (sender = s.extension.admin) "not an admin" in
  let {owner; amount; token_id} = mint in
  let () = NFT.Assertions.assert_token_exist s.token_metadata token_id in
  let new_ledger = Big_map.update (owner, token_id) (Some amount) s.ledger in
  let () = assert (Option.is_none (Big_map.find_opt (owner, token_id) s.ledger)) in
  let s = NFT.set_ledger s new_ledger in
  let new_total_supply = Total_supply.update_supply s.extension.total_supply token_id amount in
  let s = Storage.set_total_supply s new_total_supply in
  [], s

[@entry]
let set_admin (admin : address) (s : storage) : ret = 
  let () = assert_with_error (Tezos.get_sender () = s.extension.admin) "No an admin" in
  [], Storage.set_admin s admin
  
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
  let process_atomic_transfer
    (from_ : address)
    (ledger, t : NFT.ledger * NFT.TZIP12.atomic_trans) =
    let {
     to_;
     token_id;
     amount = amount_
    } = t in
    let () = NFT.assert_token_exist s token_id in
    let () = NFT.assert_authorisation s.operators from_ token_id in
    let ledger = decrease_token_amount_for_user ledger s.extension.lock_keys s.extension.lock_table from_ token_id amount_ in
    let ledger = NFT.increase_token_amount_for_user ledger to_ token_id amount_ in
    ledger in
  let process_single_transfer (ledger, t : NFT.ledger * NFT.TZIP12.transfer_from) =
    let {
     from_;
     txs
    } = t in
    let ledger = List.fold_left (process_atomic_transfer from_) ledger txs in
    ledger in
  let ledger = List.fold_left process_single_transfer s.ledger t in
  ([] : operation list), NFT.set_ledger s ledger

[@entry]
let balance_of (b: NFT.TZIP12.balance_of) (s: storage) : ret =
  NFT.balance_of b s

[@entry]
let update_operators (u: NFT.TZIP12.update_operators) (s: storage) : ret =
  NFT.update_operators  u s

[@entry]
let lock ((key, owner, token_id), amount : LockTable.lock_id * nat) (s: storage) : ret = 
  let () = assert_with_error (Tezos.get_sender () = s.extension.admin) "No an admin" in
  let new_lock = LockTable.lock s.extension.lock_table key token_id owner amount in
  let s = Storage.set_lock_table s new_lock in
  [], s

[@entry]
let unlock ((key, owner, token_id), amount : LockTable.lock_id * nat) (s: storage) : ret = 
  let () = assert_with_error (Tezos.get_sender () = s.extension.admin) "No an admin" in
  let new_lock = LockTable.unlock s.extension.lock_table key token_id owner amount in
  let s = Storage.set_lock_table s new_lock in
  [], s

[@entry]
let register_lock_key(key : LockTable.lock_key) (s: storage) : ret = 
  let () = assert_with_error (Tezos.get_sender () = s.extension.admin) "No an admin" in
  let new_lock_keys = LockKey.register_lock_key s.extension.lock_keys key in
  let s = Storage.set_lock_keys s new_lock_keys in
  [], s

[@entry]
let unregister_lock_key(key : LockTable.lock_key) (s: storage) : ret = 
  let () = assert_with_error (Tezos.get_sender () = s.extension.admin) "No an admin" in
  let new_lock_keys = LockKey.unregister_lock_key s.extension.lock_keys key in
  let s = Storage.set_lock_keys s new_lock_keys in
  [], s

[@view]
let get_balance (p : (address * nat)) (s : storage) : nat =
  NFT.get_balance p s

[@view]
let total_supply (token_id : nat) (s : storage) : nat =
  Total_supply.get_supply s.extension.total_supply token_id

[@view]
let get_lock_table (key, owner, token_id: LockTable.lock_id) (s : storage) : nat =
  LockTable.get_amount s.extension.lock_table key token_id owner