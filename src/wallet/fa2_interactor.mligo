#import "parameter.mligo" "Parameter"

type proposal_id = Parameter.Types.proposal_id

[@inline]
let call_unregister_lock_key (addr : address) (proposal_id : proposal_id) : operation =
  match Tezos.get_entrypoint_opt "%unregister_lock_key" addr with
  | Some(contr) ->
    Tezos.transaction proposal_id 0tez contr
  | None -> 
    failwith "The entrypoint %unregister_lock_key does not exist."

[@inline]
let call_register_lock_key (addr : address) (proposal_id : proposal_id) : operation =
  match Tezos.get_entrypoint_opt "%register_lock_key" addr with
  | Some(contr) ->
    Tezos.transaction proposal_id 0tez contr
  | None -> 
    failwith "The entrypoint %register_lock_key does not exist."

[@inline]
let call_lock (addr : address) (proposal_id : proposal_id) (token_id : nat) (owner : address) (quantity : nat) : operation =
  match Tezos.get_entrypoint_opt "%lock" addr with
  | Some(contr) ->
    Tezos.transaction ((proposal_id, token_id, owner), quantity) 0tez contr
  | None -> 
    failwith "The entrypoint `%lock` does not exist."

[@inline]
let call_unlock (addr : address) (proposal_id : proposal_id) (token_id : nat) (owner : address) (quantity : nat) : operation =
  match Tezos.get_entrypoint_opt "%unlock" addr with
  | Some(contr) ->
    Tezos.transaction ((proposal_id, token_id, owner), quantity) 0tez contr
  | None -> 
    failwith "The entrypoint `%unlock` does not exist."

let get_total_supply (token_id : nat) (addr : address) : nat =
  match (Tezos.call_view "total_supply" token_id addr : nat option) with
  | Some(supply) -> 
      if supply <= 0n then
        failwith "Total supply is non-positive"
      else
        supply 
  | None -> failwith "No Total supply found"
