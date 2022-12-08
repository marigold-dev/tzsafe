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


#import "../common/constants.mligo" "Constants"
#import "proposal_content.mligo" "Proposal_content"
#import "parameter.mligo" "Parameter"
#import "storage.mligo" "Storage"
#import "conditions.mligo" "Conditions"
#import "execution.mligo" "Execution"

type parameter_types = Parameter.Types.t
type storage_types = Storage.Types.t
type storage_types_proposal = Storage.Types.proposal
type proposal_content = Proposal_content.Types.t

type 'a request = 'a parameter_types * 'a storage_types
type 'a result = operation list * 'a storage_types

(**
 * Proposal creation
 *)
let create_proposal (type a) (proposal, storage : (a proposal_content) list * a storage_types) : a result =
    let () = Conditions.only_signer storage in
    let () = Conditions.amount_must_be_zero_tez (Tezos.get_amount ()) in
    let proposal = Storage.Op.create_proposal proposal in
    let storage = Storage.Op.register_proposal(proposal, storage) in
    (Constants.no_operation, storage)

(**
 * Proposal signature
 *)

let sign_proposal (type a) (proposal_id, storage : Parameter.Types.proposal_id * a storage_types) : a result =
    let () = Conditions.only_signer storage in
    let proposal = Storage.Op.retrieve_proposal(proposal_id, storage) in
    let () = Conditions.not_yet_signer proposal in
    let proposal = Storage.Op.add_signer_to_proposal (proposal, Tezos.get_sender (), storage.threshold) in
    let storage = Storage.Op.update_proposal(proposal_id, proposal, storage) in
    let operations = Execution.perform_operations proposal in
    (operations, storage)

let contract (type a) (action, storage : a request) : a result =
    let _ = Conditions.check_setting storage in
    match action with
    | Default _ -> Constants.no_operation, storage
    | Create_proposal proposal_params ->
        create_proposal (proposal_params, storage)
    | Sign_proposal proposal_number ->
        sign_proposal (proposal_number, storage)
