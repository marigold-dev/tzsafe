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

#import "parameter.mligo" "Parameter"
#import "storage.mligo" "Storage"

module Types = struct

  type challenge_id = Parameter.Types.challenge_id
  type payload = Parameter.Types.payload
  type proposal_id = Parameter.Types.proposal_id
  type agreement = Parameter.Types.agreement
  type proposal_state = Storage.Types.proposal_state

  type create_proposal = { proposal_id: proposal_id;}
  type sign_proposal = { proposal_id: proposal_id; signer: address; agreement: agreement}
  type resolve_proposal = { proposal_id: proposal_id; proposal_state: proposal_state}
  type archive_proposal = { proposal_id: proposal_id; proposal: bytes }
  type proof_of_event = { challenge_id: challenge_id; payload: payload}
  type receiving_tez = { from :address ; amount : tez}
end
