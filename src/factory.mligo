#import "./wallet.mligo" "Wallet"

[@entry]
let factory (t: Wallet.Storage.Types.t) (() : unit) : operation list * unit = 
  // create NFT
  // create Wallet
  let (op2, _) = Tezos.create_contract Wallet.contract None 0tez t in
  // set NFT admin
  [op2], unit

//// ligo compile parameter ./src/factory.mligo input -e factor
//let input : (unit -> operation list)  =
//  let t :  =
//    {
//        proposal_counter   = 0n;
//        proposals          = Big_map.empty;
//        archives           = Big_map.empty;
//        voting_history     = Big_map.empty;
//        nft                = (("tz1inzFwAjE4oWXMabJFZdPHoDQN5S4XB3wH" : address), 0n);
//        supermajority      = 1n;
//        quorum             = 1n;
//        voting_duration    = 1;
//        execution_duration = 1;
//        metadata           = Big_map.empty;
//    } in
//  
//  let (o1, _addr1) = Tezos.create_contract Wallet.contract None 0tez t in
//  fun () -> [o1]