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
#import "../../src/internal/contract.mligo" "Contract"
#import "../../src/internal/proposal_content.mligo" "Proposal_content"
#import "./util.mligo" "Util"

type proposal_content = Proposal_content.Types.t
type storage_types = Contract.storage_types
type storage_types_proposal = Contract.storage_types_proposal
type parameter_types = Contract.parameter_types
type contract_request = Contract.request
type contract_result = Contract.result

let init_storage (type a) (owners, threshold: address set * nat) : a storage_types =
{ proposal_counter = 0n;
  proposals     = (Big_map.empty : (bytes, a storage_types_proposal) big_map);
  owners           = owners;
  threshold        = threshold;
  effective_period = 172800;
  metadata         = (Big_map.empty: (string, bytes) big_map);
}

type originated = Breath.Contract.originated

let originate (type a) (level: Breath.Logger.level) (main : a contract_request -> a contract_result) (init_storage : a storage_types ) (amount : tez) =
  Breath.Contract.originate level "multisig" main init_storage amount

let create_proposal_with_amount (type a) (amount : tez) (contract : (a parameter_types, a storage_types) originated) (proposal : (a proposal_content) list) () : Breath.Result.result =
  Breath.Contract.transfer_with_entrypoint_to contract "create_proposal" proposal amount

let sign_proposal_with_amount (type a) (amount : tez) (contract : (a parameter_types, a storage_types) originated) (proposal_id : nat) (agreement : bool) (proposal : (a proposal_content) list) () : Breath.Result.result =
  let b_proposal_id = bytes proposal_id in
  Breath.Contract.transfer_with_entrypoint_to contract "sign_proposal" (b_proposal_id, proposal, agreement) amount

let resolve_proposal_with_amount (type a) (amount : tez) (contract : (a parameter_types, a storage_types) originated) (proposal_id : nat) (proposal : (a proposal_content) list) () : Breath.Result.result =
  let poe = { challenge_id = bytes proposal_id; payload = Bytes.pack proposal} in
  Breath.Contract.transfer_with_entrypoint_to contract "proof_of_event_challenge" poe amount

let create_proposal (type a) (contract : (a parameter_types, a storage_types) originated) (proposal : (a proposal_content) list) () : Breath.Result.result =
  create_proposal_with_amount 0tez contract proposal ()

let sign_proposal (type a) (contract : (a parameter_types, a storage_types) originated) (proposal_id : nat) (agreement : bool) (proposal : (a proposal_content) list) () : Breath.Result.result =
  sign_proposal_with_amount 0tez contract proposal_id agreement proposal ()

let resolve_proposal (type a) (contract : (a parameter_types, a storage_types) originated) (proposal_id : nat) (proposal : (a proposal_content) list) () : Breath.Result.result =
  resolve_proposal_with_amount 0tez contract proposal_id proposal ()
