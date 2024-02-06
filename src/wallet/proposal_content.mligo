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

module Types = struct
    type transaction =
    [@layout:comb]
    {
        target: address;
        amount: tez;
    }

    type t =
    | Transfer of transaction
    | Execute_lambda of { metadata: bytes option; lambda: (unit -> operation list)}
    | Adjust_quorum of nat
    | Adjust_supermajority of nat
    | Adjust_voting_duration of int
    | Adjust_execution_duration of int
    | Adjust_nft of (address * nat)
    | Add_or_update_metadata of { key: string; value: bytes; }
    | Remove_metadata of { key: string }
    | Proof_of_event of { payload: bytes}
end