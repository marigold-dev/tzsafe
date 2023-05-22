(* MIT License
   Copyright (c) 2022 Marigold <contact@marigold.dev>
   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal in
   the Software without restriction, including without limitation the rights to
   use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
   the Software, and to permit persons to whom the Software is furnished to do so,
   subject to the following conditions:
   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.
   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

#import "ligo-breathalyzer/lib/lib.mligo" "Breath"
#include "../../src/internal/contract.mligo"
#import "../../src/internal/proposal_content.mligo" "Proposal_content"
#import "../../src/internal/storage.mligo" "Storage"
#import "./util.mligo" "Util"

type proposal_content = Proposal_content.Types.t
type storage_wallet = Storage.Types.wallet

let init_storage_with_effective_period
  (type a) (owners, threshold, effective_period: address set * nat * int) : a storage_types =
{ wallet =
    { proposal_counter = 0n;
      proposals     = (Big_map.empty : (nat, a storage_types_proposal) big_map);
      owners           = owners;
      threshold        = threshold;
      effective_period = effective_period;
      metadata         = (Big_map.empty: (string, bytes) big_map);
    };
  tickets = Big_map.empty
}

let init_storage (type a) (owners, threshold: address set * nat) : a storage_types =
init_storage_with_effective_period (owners, threshold, 172800)

type 'a unforged_storage =
[@layout:comb] { wallet  : 'a storage_wallet;
                 tickets : ('a * address, 'a unforged_ticket) big_map;
               }

type originated = Breath.Contract.originated

let originate (type a) (level: Breath.Logger.level) (main : a request -> a result) (init_storage : a storage_types ) (amount : tez) =
  Breath.Contract.originate
    level
    "multisig"
    main
    init_storage
    amount

let create_proposal_with_amount (type a) (amount : tez) (contract : (a parameter_types, a storage_types) originated) (proposal : (a proposal_content) list) () =
  Breath.Contract.transfer_with_entrypoint_to contract "create_proposal" proposal amount

let sign_proposal_with_amount (type a) (amount : tez) (contract : (a parameter_types, a storage_types) originated) (proposal_id : nat) (agreement : bool) (proposal : (a proposal_content) list) () =
  Breath.Contract.transfer_with_entrypoint_to contract "sign_proposal" (proposal_id, proposal, agreement) amount

let resolve_proposal_with_amount (type a) (amount : tez) (contract : (a parameter_types, a storage_types) originated) (proposal_id : nat) (proposal : (a proposal_content) list) () =
  Breath.Contract.transfer_with_entrypoint_to contract "resolve_proposal" (proposal_id, proposal) amount

let create_proposal =
  create_proposal_with_amount 0tez

let sign_proposal =
  sign_proposal_with_amount 0tez

let resolve_proposal =
  resolve_proposal_with_amount 0tez
