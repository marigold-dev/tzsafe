#import "@ligo/fa/lib/main.mligo" "FA2"
#import "./fa2/storage.mligo" "Storage"
#import "./fa2/total_supply.mligo" "Total_supply"
#import "./fa2/lock.mligo" "Lock"

module NFT = FA2.MultiAssetExtendable

type storage = Storage.t

type ret = operation list * storage

type mint =
  { owner : address
  ; amount : nat
  ; token_id : nat
  }

// can only create new token
[@entry]
let mint (mint : mint) (s : storage) : ret =
  let sender = Tezos.get_sender () in
  let () = assert (sender = s.extension.admin) in
  let {owner; amount; token_id} = mint in

  (* ???? *)
  let () = NFT.Assertions.assert_token_exist s.token_metadata token_id in

  (* TODO: Check that nobody owns the token already *)
  let new_ledger = Big_map.update (owner, token_id) (Some amount) s.ledger in

  let () = assert (Option.is_none (Big_map.find_opt (owner, token_id) s.ledger)) in
  let s = NFT.set_ledger s new_ledger in

  let new_total_supply = Total_supply.update_supply s.extension.total_supply token_id amount in
  let s = Storage.set_total_supply s new_total_supply in
  [], s

(* Standard FA2 interface, copied from the source *)


//TODO: add lock
[@entry]
let transfer (t: NFT.TZIP12.transfer) (s: storage) : ret =
  NFT.transfer t s

[@entry]
let balance_of (b: NFT.TZIP12.balance_of) (s: storage) : ret =
  NFT.balance_of b s

[@entry]
let update_operators (u: NFT.TZIP12.update_operators) (s: storage) : ret =
  NFT.update_operators  u s

[@entry]
let lock ((key, owner, token_id), amount : Lock.lock_id * nat) (s: storage) : ret = 
  let new_lock = Lock.lock s.extension.lock key token_id owner amount in
  let s = Storage.set_lock s new_lock in
  [], s

[@entry]
let unlock ((key, owner, token_id), amount : Lock.lock_id * nat) (s: storage) : ret = 
  let new_lock = Lock.unlock s.extension.lock key token_id owner amount in
  let s = Storage.set_lock s new_lock in
  [], s

[@view]
let get_balance (p : (address * nat)) (s : storage) : nat =
  NFT.get_balance p s

[@view]
let total_supply (token_id : nat) (s : storage) : nat =
  Total_supply.get_supply s.extension.total_supply token_id

[@view]
let get_lock (key, owner, token_id: Lock.lock_id) (s : storage) : nat =
  Lock.get_amount s.extension.lock key token_id owner