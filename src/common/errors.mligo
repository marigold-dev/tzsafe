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

let only_owner = "Only the contract owners can perform this operation"
[@inline]
let amount_must_be_zero_tez = "You must not send tez to the smart contract"
let no_proposal_exist = "No proposal exists for this counter"
let has_already_signed = "You have already signed this proposal"
let unknown_contract = "Unknown contract"
let no_owner = "Require at least one owner in the contract"
let no_enough_owner = "Number of owner should be greater than threshold"
let already_resolved = "This proposal has been resolved"
let no_enough_signature_to_resolve = "No enough signature to resolve the proposal"
let no_proposal = "There is no content in proposal"
let no_owners = "No owner to be added or removed"
let amount_is_zero = "Amount should be greater than zero"
let not_the_same_content = "The proposal content doesn't match"
let pass_voting_time = "The proposal has passed its voting time"
let invalid_quorum = "The quorum must be >= 1"
let invalid_supermajority = "The supermajority must be >= 1"
let invalid_voting_period = "The voting period must be >= 1"
let invalid_execution_period = "The execution period must be >= 1"
