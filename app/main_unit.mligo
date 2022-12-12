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

#import "../src/lib.mligo" "Lib"
#import "../src/views_lib.mligo" "Views"

type result = Lib.result
type request =  Lib.request

type storage = Lib.storage_types
type proposal_id = Lib.Storage.Types.proposal_id
type view_proposal = Lib.Storage.Types.view_proposal

let main (request : unit request) : unit result =
  Lib.contract request

[@view]
let signers (input : unit * unit storage) : address set =
  Views.signers input

[@view]
let threshold (input : unit * unit storage) : nat =
  Views.threshold input

[@view]
let proposal (input : proposal_id * unit storage) : unit view_proposal =
  Views.proposal input

[@view]
let proposals (input : (proposal_id * proposal_id) * unit storage) : (proposal_id, unit view_proposal) map =
  Views.proposals input
