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

let init_storage (type a) (signers, threshold: address set * nat) : a storage_types =
{ proposal_counter = 0n;
  proposal_map     = (Big_map.empty : (nat, a storage_types_proposal) big_map);
  signers          = signers;
  threshold        = threshold;
  metadata         = (Big_map.empty: (string, bytes) big_map);
}

type originated = Breath.Contract.originated

let originate (type a) (level: Breath.Logger.level) (main : a request -> a result) (init_storage : a storage_types ) (amount : tez) =
  Breath.Contract.originate
    level
    "multisig"
    main
    init_storage
    amount

let create_proposal (type a) (contract : (a parameter_types, a storage_types) originated) (proposal : a parameter_types_raw_proposal) () =
  Breath.Contract.transfert_with_entrypoint_to contract "create_proposal" proposal 0tez

let sign_proposal (type a) (contract : (a parameter_types, a storage_types) originated) (proposal_number : nat) () =
  Breath.Contract.transfert_with_entrypoint_to contract "sign_proposal" proposal_number 0tez
