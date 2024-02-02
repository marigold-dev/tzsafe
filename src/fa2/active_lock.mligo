#import "./lock.mligo" "Lock"

type t = (Lock.key, unit) big_map

let create_active_key (t : t) (key: Lock.key)  =
  let () = assert_with_error (not Big_map.mem key t) "Active key is existed" in
  Big_map.add key () t

let remove_active_key (t : t) (key: Lock.key)  =
  let () = assert_with_error (Big_map.mem key t) "Active key is non-existed" in
  Big_map.remove key t
    