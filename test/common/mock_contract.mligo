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

#import "../../src/internal/contract.mligo" "Contract"

(* multisig wallet 1 *)
type result = Contract.result
type request =  Contract.request

(* ------------------------ *)
(* contract 1 *)
type add_action = nat
let add_main (n,storage : add_action * nat) : operation list * nat =
  [], n + storage


let multisig_main (request : add_action request) : add_action result =
  Contract.contract request

(* ------------------------ *)
(* contract 2 *)
type set_action = nat
let set_main (n,_storage : set_action * nat) : operation list * nat =
  [], n

(* multisig wallet 2 *)
let multisig_set_main (request : set_action request) : set_action result =
  Contract.contract request
