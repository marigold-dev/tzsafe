#import "@ligo/fa/lib/main.mligo" "FA2"
#import "./total_supply.mligo" "TS"
#import "./lock_table.mligo" "LockTable"
#import "./registered_lock_key.mligo" "LockKey"

module NFT = FA2.MultiAssetExtendable

type extension = {
  admin: address;
  total_supply: TS.t;
  lock_table: LockTable.t;
  lock_keys : LockKey.t;
}

type t = extension NFT.storage


let set_extension (s : t) (ext : extension) =
    { s with extension = ext }

let set_total_supply (s: t) (ts : TS.t) : t =
   set_extension s { s.extension with total_supply = ts }

let set_lock_table (s:t) (lock_table : LockTable.t) : t =
   set_extension s { s.extension with lock_table = lock_table }

let set_lock_keys (s:t) (lock_keys : LockKey.t) : t =
   set_extension s { s.extension with lock_keys = lock_keys }

let set_admin (s:t) (admin : address) : t =
   set_extension s { s.extension with admin = admin}