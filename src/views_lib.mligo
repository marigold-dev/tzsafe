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

#import "./internal/storage.mligo" "Storage"

type storage = Storage.Types.t
type proposal_id = Storage.Types.proposal_id
type proposal = Storage.Types.proposal
type proposal_content = Storage.Types.proposal_content
type view_proposal = Storage.Types.view_proposal
type view_proposal_content = Storage.Types.view_proposal_content


let to_view_proposal_content (type a) (p : a proposal_content) : (a view_proposal_content) =
  match p with
  | Transfer t -> Transfer t
  | Execute e -> Execute e
  | Execute_lambda _ -> Execute_lambda
  | Adjust_threshold t -> Adjust_threshold t
  | Add_signers s -> Add_signers s
  | Remove_signers s -> Remove_signers s

let to_view_proposal (type a) (p : a proposal) : (a view_proposal) =
  {
    approved_signers = p.approved_signers;
    proposer         = p.proposer;
    executed         = p.executed;
    number_of_signer = p.number_of_signer;
    timestamp        = p.timestamp;
    content          = List.map to_view_proposal_content p.content;
  }

let rec slice (type a) (start, end_, proposals, acc :
    proposal_id * proposal_id * (proposal_id, a proposal) big_map * (proposal_id, a proposal) map) : (proposal_id, a proposal) map =
  let idx = start + 1n in
  if idx > end_
  then acc
  else
    match Big_map.find_opt idx proposals with
    | None -> slice (idx, end_, proposals, acc)
    | Some p -> slice (idx, end_, proposals, Map.add idx p acc)

let signers (type a) ((), storage : unit * a storage) : address set =
  storage.signers

let threshold (type a) ((), storage : unit * a storage) : nat =
  storage.threshold

let proposal (type a) (id, storage : proposal_id * a storage) : a view_proposal =
  to_view_proposal (Storage.Op.retrieve_proposal (id, storage))

let proposals (type a) (((limit,  offset), storage) : (proposal_id * proposal_id) * a storage) : (proposal_id, a view_proposal) map=
   let end_ = offset + limit in
   Map.map
     (fun (_id, p : proposal_id * a proposal) : a view_proposal -> to_view_proposal p)
     (slice(offset, end_, storage.proposal_map, Map.empty))
