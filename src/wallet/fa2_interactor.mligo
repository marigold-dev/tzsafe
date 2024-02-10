#import "@ligo/fa/lib/main.mligo" "FA2"
#import "../fa2/storage.mligo" "FStorage"
#import "../fa2/registered_lock_key.mligo" "LockKey"
#import "../fa2/lock_table.mligo" "LockTable"
#import "../fa2/total_supply.mligo" "Total_supply"
#import "./proposal_content.mligo" "PC"

module FA2 = FA2.MultiAssetExtendable

type f_storage = FStorage.t

let call_unregister_lock_key (fa2 : f_storage) (key: nat) : f_storage =

  let new_lock_keys = LockKey.unregister_lock_key fa2.extension.lock_keys key in
  FStorage.set_lock_keys fa2 new_lock_keys

let call_register_lock_key (fa2 : f_storage) (key : nat) : f_storage=
  let new_lock_keys = LockKey.register_lock_key fa2.extension.lock_keys key in
  FStorage.set_lock_keys fa2 new_lock_keys

let call_lock (fa2 : f_storage) (key: nat) (token_id : nat) (owner : address) (quantity : nat) : f_storage =
  let new_lock = LockTable.lock fa2.extension.lock_table key token_id owner quantity in
  FStorage.set_lock_table fa2 new_lock

let call_unlock (fa2 : f_storage) (key: nat) (token_id : nat) (owner : address) (quantity : nat) : f_storage =
  let new_lock = LockTable.unlock fa2.extension.lock_table key token_id owner quantity in
  FStorage.set_lock_table fa2 new_lock

let get_total_supply (fa2 : f_storage) (token_id : nat) : nat =
  Total_supply.get_supply fa2.extension.total_supply token_id

let mint (fa2 : f_storage) (mint : PC.Types.mint) : f_storage =
  let {owner; amount; token_id} = mint in
  let () = FA2.Assertions.assert_token_exist fa2.token_metadata token_id in
  let () = assert (Option.is_none (Big_map.find_opt (owner, token_id) fa2.ledger)) in
  let new_ledger = Big_map.update (owner, token_id) (Some amount) fa2.ledger in
  let fa2 = FA2.set_ledger fa2 new_ledger in
  let new_total_supply = Total_supply.update_supply fa2.extension.total_supply token_id amount in
  FStorage.set_total_supply fa2 new_total_supply

let create_token (fa2 : f_storage) (md: FA2.TZIP12.tokenMetadataData) : f_storage =
  let () = assert_with_error (not (Big_map.mem md.token_id fa2.token_metadata)) "Token is already exist" in
  let md = Big_map.add md.token_id md fa2.token_metadata in
  { fa2 with token_metadata = md}