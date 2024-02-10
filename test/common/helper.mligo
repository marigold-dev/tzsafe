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
#include "../../src/wallet/contract.mligo"
#import "../../src/wallet/proposal_content.mligo" "Proposal_content"
#import "../../src/wallet/parameter.mligo" "Param"
#import "./util.mligo" "Util"
#import "../../src/tzsafe.mligo" "App"
#import "../../src/storage.mligo" "GStorage"
#import "../../src/fa2/storage.mligo" "FStorage"

type proposal_content = Proposal_content.Types.t
type sign_proposal = Param.Types.sign_proposal
type resolve_proposal = Param.Types.resolve_proposal
type votes = Param.Types.votes

let init_storage (owners, quorum: address set * nat) : GStorage.t =
  let wallet : storage_types = 
    {
        proposal_counter   = 0n;
        proposals          = Big_map.empty;
        archives           = Big_map.empty;
        voting_history     = Big_map.empty;
        token              = { token_id = 0n};
        supermajority      = 80n;
        quorum             = quorum;
        voting_duration    = 172800;
        execution_duration = 172800;
        metadata           = Big_map.literal [("", 0x01)];
    }
  in
  let fa2 : FStorage.t = 
    {
        ledger = Big_map.empty;
        operators = Big_map.empty;
        token_metadata = Big_map.empty;
        metadata = Big_map.empty;
        extension = {
          total_supply = Big_map.empty;
          lock_table = Big_map.empty;
          lock_keys = Set.empty;
        }
    }
  in { wallet; fa2; }

type originated = Breath.Contract.originated

let originate (level: Breath.Logger.level) (init_storage : g_storage) (amount : tez) : (App parameter_of, g_storage) originated=
  Breath.Contract.originate
    level
    "multisig"
    (contract_of App)
    init_storage
    amount

let proof_of_event_challenge (amount: tez) (contract : (App parameter_of, g_storage) originated) (payload : payload) () =
  Breath.Contract.transfer_with_entrypoint_to contract "proof_of_event_challenge" payload amount

let create_proposal_with_amount (amount : tez) (contract : (App parameter_of, g_storage) originated) (proposal : (proposal_content) list) () =
  Breath.Contract.transfer_with_entrypoint_to contract "create_proposal" proposal amount

let sign_proposal_with_amount (amount : tez) (contract : (App parameter_of, g_storage) originated) (proposal_id : nat) (votes: votes) (proposal_contents : (proposal_content) list) () =
  let p : sign_proposal = { proposal_id; proposal_contents; votes} in
  Breath.Contract.transfer_with_entrypoint_to contract "sign_proposal" p amount

let resolve_proposal_with_amount (amount : tez) (contract : (App parameter_of, g_storage) originated) (proposal_id : nat) (proposal_contents: (proposal_content) list) () =
  let p : resolve_proposal = { proposal_id; proposal_contents;} in
  Breath.Contract.transfer_with_entrypoint_to contract "resolve_proposal" p amount

let create_proposal =
  create_proposal_with_amount 0tez

let sign_proposal =
  sign_proposal_with_amount 0tez

let resolve_proposal =
  resolve_proposal_with_amount 0tez
