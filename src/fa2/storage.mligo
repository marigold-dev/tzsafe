#import "@ligo/fa/lib/main.mligo" "FA2"
#import "./total_supply.mligo" "TS"

module NFT = FA2.MultiAssetExtendable

type extension = {
  admin: address;
  total_supply: TS.t;
}

type t = extension NFT.storage


let set_extension (s : t) (ext : extension) =
    { s with extension = ext }

let set_total_supply (s: t) (ts : TS.t) =
   set_extension s { s.extension with total_supply = ts }
