#import "./lock_table.mligo" "LockTable"

// registered lock key is a set including all registered lock keys  
type t = LockTable.lock_key set 

let register_lock_key (t : t) (key: LockTable.lock_key)  =
  let () = assert_with_error (not Set.mem key t) "The given lock key does exist" in
  Set.add key t

let unregister_lock_key (t : t) (key: LockTable.lock_key)  =
  let () = assert_with_error (Set.mem key t) "The given lock key does not exist" in
  Set.remove key t