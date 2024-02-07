
type lock_key = nat 
type token_id = nat
type owner = address
type amount = nat
type lock_id = (lock_key * owner * token_id)

type t = (lock_id, amount) big_map

let get_amount (lock : t) (key : nat) (token_id : nat) (owner : owner) =
   match Big_map.find_opt (key, owner, token_id) lock with
    Some (a) -> a
    | None -> 0n

let lock (lock: t) (key : nat) (token_id : nat) (owner : owner) (amount_ : nat) : t =
    let cur_lock = get_amount lock key token_id owner in
    let new_amount = cur_lock + amount_ in
    Big_map.update (key, owner, token_id) (Some(new_amount)) lock

let unlock (lock: t) (key : nat) (token_id : nat) (owner : owner) (amount_ : nat) : t =
    let cur_lock = get_amount lock key token_id owner in
    let new_amount = abs(cur_lock - amount_) in
    Big_map.update (key, owner, token_id) (Some(new_amount)) lock