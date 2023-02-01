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
    type 'a transaction =
    [@layout:comb]
    {
        target: address;
        parameter: 'a;
        amount: tez;
    }

    (* The view function is forbidden operation. Therefore, the lambda of
       [Execute_lambda] is removed *)
    type 'a view =
    | Transfer of unit transaction
    | Execute of ('a transaction)
    | Execute_lambda of { metadata: bytes option }
    | Adjust_threshold of nat
    | Add_owners of address set
    | Remove_owners of address set
    | Adjust_effective_period of int

    type 'a t =
    | Transfer of unit transaction
    | Execute of ('a transaction)
    | Execute_lambda of { metadata: bytes option; lambda: (unit -> operation) option }
    | Adjust_threshold of nat
    | Add_owners of address set
    | Remove_owners of address set
    | Adjust_effective_period of int
end
