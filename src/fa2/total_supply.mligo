#import "@ligo/fa/lib/main.mligo" "FA2"

module NFT = FA2.MultiAssetExtendable

(* token_id -> total_supply *)
type t = (nat, nat) big_map

let get_supply (supply : t) (token_id : nat) =
   match Big_map.find_opt token_id supply with
    Some (a) -> a
    | None -> failwith NFT.Errors.undefined_token

let create_supply (supply : t) (new_token_id : nat) (amount_ : nat) =
    Big_map.add new_token_id amount_ supply

let increase_supply (supply : t) (token_id : nat) (amount_ : nat) =
    let cur_amount = get_supply supply token_id in
    let new_amount = cur_amount + amount_ in
    Big_map.update token_id (Some(new_amount)) supply

let update_supply (supply : t) (token_id : nat) (amount_ : nat) =
   match Big_map.find_opt token_id supply with
   Some _ -> increase_supply supply token_id amount_
   | None -> create_supply supply token_id amount_

let decrease_supply (supply : t) (token_id : nat) (amount_ : nat) =
    let cur_amount = get_supply supply token_id in
    let new_supply = abs(cur_amount - amount_) in
    Big_map.update token_id (Some(new_supply)) supply