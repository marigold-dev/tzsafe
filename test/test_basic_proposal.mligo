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
#import "./common/helper.mligo" "Helper"
#import "./common/assert.mligo" "Assert"
#import "./common/mock_contract.mligo" "Mock_contract"
#import "./common/util.mligo" "Util"
#import "../src/internal/proposal_content.mligo" "Proposal_content"

type proposal_content = Proposal_content.Types.t

let case_create_proposal =
  Breath.Model.case
  "test create proposal"
  "successuful create proposal"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in
      let add_contract = Breath.Contract.originate level "add_contr" Mock_contract.add_main 1n 0tez in
      let param = ([] : (nat proposal_content) list) in

      (* create proposal 1 *)
      let param1 = (Execute { target = add_contract.originated_address; parameter = 10n; amount = 0tez;} :: param) in
      let action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in

      (* create proposal 2 *)
      let param2 = (Execute { target = add_contract.originated_address; parameter = 20n; amount = 10tez;} :: param)  in
      let action2 = Breath.Context.act_as bob (Helper.create_proposal multisig_contract param2) in

      (* create proposal 3 *)
      let param3 = (Transfer { target = alice.address; parameter = (); amount = 10tez;} :: param) in
      let action3 = Breath.Context.act_as bob (Helper.create_proposal multisig_contract param3) in

      (* create proposal 4 *)
      let param4 =
        [ Transfer { target = alice.address; parameter = (); amount = 10tez;}
        ; Transfer { target = alice.address; parameter = (); amount = 10tez;}
        ; Execute { target = add_contract.originated_address; parameter = 20n; amount = 10tez;}
        ] in
      let action4 = Breath.Context.act_as bob (Helper.create_proposal multisig_contract param4) in

      let balance = Breath.Contract.balance_of multisig_contract in
      let storage = Breath.Contract.storage_of multisig_contract in

      let proposal1 = Util.unopt (Big_map.find_opt 1n storage.proposals) "proposal 1 doesn't exist" in
      let proposal2 = Util.unopt (Big_map.find_opt 2n storage.proposals) "proposal 2 doesn't exist" in
      let proposal3 = Util.unopt (Big_map.find_opt 3n storage.proposals) "proposal 3 doesn't exist" in
      let proposal4 = Util.unopt (Big_map.find_opt 4n storage.proposals) "proposal 4 doesn't exist" in

      Breath.Result.reduce [
        action1
      ; action2
      ; action3
      ; action4
      ; Breath.Assert.is_equal "balance" balance 0tez
      ; Breath.Assert.is_equal "the counter of proposal" storage.proposal_counter 4n
      ; Assert.is_proposal_equal "#1 proposal" proposal1
        ({
          state            = Proposing;
          signatures       = Map.empty;
          proposer         = { actor = alice.address; timestamp = Tezos.get_now () };
          resolver         = None;
          contents         = [ Execute {
            parameter        = 10n;
            amount           = 0tez;
            target           = add_contract.originated_address;
          }]
        })
      ; Assert.is_proposal_equal "#2 proposal" proposal2
        ({
          state            = Proposing;
          signatures       = Map.empty;
          proposer         = { actor = bob.address; timestamp = Tezos.get_now () };
          resolver         = None;
          contents         = [ Execute {
            target           = add_contract.originated_address;
            amount           = 10tez;
            parameter        = 20n;
          }]
        })
      ; Assert.is_proposal_equal "#3 proposal" proposal3
        ({
          state            = Proposing;
          signatures       = Map.empty;
          proposer         = { actor = bob.address; timestamp = Tezos.get_now () };
          resolver         = None;
          contents         = [ Transfer {
            parameter        = ();
            amount           = 10tez;
            target           = alice.address;
          }]
        })
      ; Assert.is_proposal_equal "#4 proposal" proposal4
        ({
          state            = Proposing;
          signatures       = Map.empty;
          proposer         = { actor = bob.address; timestamp = Tezos.get_now () };
          resolver         = None;
          contents         =
            [ Transfer {parameter = (); amount = 10tez; target = alice.address; }
            ; Transfer {parameter = (); amount = 10tez; target = alice.address; }
            ; Execute  {parameter = 20n; amount = 10tez; target = add_contract.originated_address; }
            ]
        })
      ])

let case_fail_to_create_empty_proposal =
  Breath.Model.case
  "test create empty proposal"
  "failed to create proposal"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in
      let param = ([] : (nat proposal_content) list) in

      let action = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param) in

      Breath.Result.reduce [
        Breath.Expect.fail_with_message "There is no content in proposal" action
      ])

let case_fail_to_create_transfer_0_amount_proposal =
  Breath.Model.case
  "test create transfer 0 amount proposal"
  "failed to create proposal"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in
      let param = ([] : (nat proposal_content) list) in
      let param = (Transfer { target = alice.address; parameter = (); amount = 0tez;} :: param) in

      let action = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param) in

      Breath.Result.reduce [
        Breath.Expect.fail_with_message "Amount should be greater than zero" action
      ])

let case_fail_to_create_proposal_with_empty_owner_adjustment =
  Breath.Model.case
  "test create proposal with empty owner adjustment"
  "failed to create proposal"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in
      let param = ([] : (nat proposal_content) list) in

      let param1 = Remove_owners (Set.literal []) :: param in
      let action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in

      let param2 = Add_owners (Set.literal []) :: param in
      let action2 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param2) in

      Breath.Result.reduce [
        Breath.Expect.fail_with_message "No owner to be added or removed" action1
      ; Breath.Expect.fail_with_message "No owner to be added or removed" action2
      ])

let case_unauthorized_user_fail_to_create_proposal =
  Breath.Model.case
  "unauthorized user creates proposal"
  "fail to create proposal"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in
      let add_contract = Breath.Contract.originate level "add_contr" Mock_contract.add_main 1n 0tez in

      (* create proposal 1 *)
      let param1 = [Execute { target = add_contract.originated_address; parameter = 10n; amount = 0tez;}] in
      let action1 = Breath.Context.act_as carol (Helper.create_proposal multisig_contract param1) in

      (* create proposal 1 *)
      let param2 = [Transfer { target = alice.address; parameter = (); amount = 0tez;}] in
      let action2 = Breath.Context.act_as carol (Helper.create_proposal multisig_contract param2) in

      let balance = Breath.Contract.balance_of multisig_contract in
      let storage = Breath.Contract.storage_of multisig_contract in

      Breath.Result.reduce [
        Breath.Expect.fail_with_message "Only the contract owners can perform this operation" action1
      ; Breath.Expect.fail_with_message "Only the contract owners can perform this operation" action2
      ; Breath.Assert.is_equal "balance" balance 0tez
      ; Breath.Assert.is_equal "the counter of proposal" storage.proposal_counter 0n
      ])

let test_suite =
  Breath.Model.suite "Suite for create proposal" [
    case_create_proposal
  ; case_unauthorized_user_fail_to_create_proposal
  ; case_fail_to_create_empty_proposal
  ; case_fail_to_create_transfer_0_amount_proposal
  ; case_fail_to_create_proposal_with_empty_owner_adjustment
  ]

