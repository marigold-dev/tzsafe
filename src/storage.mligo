
#import "./fa2/storage.mligo" "FStorage"
#import "./wallet/storage.mligo" "WStorage"

type metadata =(string, bytes) big_map 

(* type 'a storage_types *)
type t = 
  { wallet : WStorage.Types.t;
    fa2 :  FStorage.t;
    metadata: metadata;
  }