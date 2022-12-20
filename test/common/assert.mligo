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
#import "../../src/internal/storage.mligo" "Storage"

type storage_types_proposal = Storage.Types.proposal
type storage_types_view_proposal = Storage.Types.view_proposal

let is_proposal_equal (type a) (msg:string) (actual : a storage_types_proposal) (expected : a storage_types_proposal) =
  let mock_time = Tezos.get_now () in
  let actual = { actual with timestamp = mock_time } in
  let expected = { expected with timestamp = mock_time } in
  Breath.Assert.is_equal msg actual expected

let is_view_proposal_equal (type a) (msg:string) (actual : a storage_types_view_proposal) (expected : a storage_types_view_proposal) =
  let mock_time = Tezos.get_now () in
  let actual = { actual with timestamp = mock_time } in
  let expected = { expected with timestamp = mock_time } in
  Breath.Assert.is_equal msg actual expected

let is_view_proposals_equal (type a) (msg:string) (actual : (nat, a storage_types_view_proposal) map) (expected : (nat, a storage_types_view_proposal) map) =
  let mock_time = Tezos.get_now () in
  let mock (type a) (_, t : (nat * a storage_types_view_proposal)) : a storage_types_view_proposal = { t with timestamp = mock_time } in
  let actual = Map.map mock actual  in
  let expected = Map.map mock expected in
  Breath.Assert.is_equal msg actual expected
