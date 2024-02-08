#import "./lock_table.mligo" "LockTable"

// registered lock key is a set including all registered lock keys  
type t = LockTable.lock_key set 

let register_lock_key (t : t) (key: LockTable.lock_key)  =
  let () = assert_with_error (not Set.mem key t) "The given lock key does exist" in
  Set.add key t

let unregister_lock_key (t : t) (key: LockTable.lock_key)  =
  let () = assert_with_error (Set.mem key t) "The given lock key does not exist" in
  Set.remove key t

let get_lock_amount (keys: t) (owner: LockTable.owner) (token_id: LockTable.token_id) (locktable: LockTable.t) : nat =
  Set.fold (fun (key, acc) ->
    let lock_id : LockTable.lock_id = (key, owner, token_id) in
    match Big_map.find_opt lock_id locktable with
    | Some(amount) -> if amount > acc then amount else acc
    | None -> acc
  ) keys 0n