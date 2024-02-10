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
#import "../../src/wallet/storage.mligo" "Storage"

type storage_types_proposal = Storage.Types.proposal
type storage_types_actor = Storage.Types.actor

let mock_timestamp (timestamp : timestamp) (actor: storage_types_actor) : storage_types_actor =
  { actor with timestamp = timestamp }

let is_proposal_equal (msg:string) (actual : storage_types_proposal) (expected : storage_types_proposal) =
  let mock_time = Tezos.get_now () in
  let actual = { actual with
    proposer = mock_timestamp mock_time actual.proposer ;
    resolver = Option.map (mock_timestamp mock_time) actual.resolver }
  in
  let expected = { expected with
    proposer = mock_timestamp mock_time expected.proposer ;
    resolver = Option.map (mock_timestamp mock_time) expected.resolver }
  in
  Breath.Assert.is_equal msg actual expected
