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
#import "./common/proxy_ticket.mligo" "Proxy_ticket"
#import "../src/internal/proposal_content.mligo" "Proposal_content"
#import "../src/internal/storage.mligo" "Storage"

type proposal_content = Proposal_content.Types.t

let case_execute_lambda_proposal_without_ticket =
  Breath.Model.case
  "test create lambda proposal and execute"
  "successuful execute"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in
      let add_contract = Breath.Contract.originate level "add_contr" Mock_contract.add_main 1n 0tez in
      let add_contract_address = add_contract.originated_address in

      let param = ([] : (nat proposal_content) list) in

      (* lambda *)
      let call_add_contract (type a) (_ : (a ticket) option) : operation * (a ticket) list =
        let add_contr = Tezos.get_contract_with_error add_contract_address "add contract doesn't exist" in
           Tezos.transaction 10n 0tez add_contr, []
      in

      (* create proposal *)
      let param = Execute_lambda { metadata = None; lambda = Some call_add_contract; args = None } :: param in
      let action = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param) in
      let sign_action = Breath.Context.act_as bob (Helper.sign_proposal multisig_contract 1n true param) in
      let resolve_action = Breath.Context.act_as bob (Helper.resolve_proposal multisig_contract 1n param) in

      let add_contract_storage = Breath.Contract.storage_of add_contract in
      let storage = Breath.Contract.storage_of multisig_contract in
      let proposal1 = Util.unopt (Big_map.find_opt 1n storage.wallet.proposals) "proposal 1 doesn't exist" in

      Breath.Result.reduce [
        action
      ; sign_action
      ; resolve_action
      ; Breath.Assert.is_equal "storage of add contract" add_contract_storage 11n
      ; Breath.Assert.is_equal "empty ticket in multisig storage" storage.tickets Big_map.empty
      ; Assert.is_proposal_equal "#1 proposal" proposal1
        ({
          state            = Executed;
          signatures       = Map.literal [(bob.address, true)];
          proposer         = { actor = alice.address; timestamp = Tezos.get_now () };
          resolver         = Some { actor = bob.address; timestamp = Tezos.get_now () };
          contents         = [ Execute_lambda {
            metadata       = None;
            lambda         = None;
            args           = None;
          }]
        })
      ])

type unforged_storage = Helper.unforged_storage

let case_execute_lambda_proposal_with_returning_ticket =
  Breath.Model.case
  "test create lambda proposal and execute"
  "successuful execute"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in
      let multisig_address = multisig_contract.originated_address in
      let add_contract = Breath.Contract.originate level "add_contr" Mock_contract.add_main 1n 0tez in
      let add_contract_address = add_contract.originated_address in

      let param = ([] : (nat proposal_content) list) in

      let lambda (_ : (nat ticket) option) : operation * (nat ticket) list =
        let add_contr = Tezos.get_contract_with_error add_contract_address "add contract doesn't exist" in
        let t1= Option.unopt (Tezos.create_ticket 1n 10n) in
        let t2= Option.unopt (Tezos.create_ticket 2n 12n) in
        let t3= Option.unopt (Tezos.create_ticket 3n 13n) in
           Tezos.transaction 10n 0tez add_contr, [t1; t2; t3]
      in

      (* create proposal *)
      let param = Execute_lambda { metadata = None; lambda = Some lambda ; args = None } :: param in
      let action = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param) in
      let sign_action = Breath.Context.act_as bob (Helper.sign_proposal multisig_contract 1n true param) in
      let resolve_action = Breath.Context.act_as bob (Helper.resolve_proposal multisig_contract 1n param) in

      let storage = Breath.Contract.storage_of multisig_contract in
      let proposal1 = Util.unopt (Big_map.find_opt 1n storage.wallet.proposals) "proposal 1 doesn't exist" in

      let storage_m : michelson_program = Test.get_storage_of_address multisig_address in
      let unforged_storage = (Test.decompile storage_m : nat unforged_storage) in

      let ticket1 = Util.unopt (Big_map.find_opt (1n, multisig_address) unforged_storage.tickets) "ticket 1 doesn't exist" in
      let ticket1_ = ({ ticketer = multisig_address ; amount = 10n ;  value = 1n } : nat unforged_ticket) in

      Breath.Result.reduce [
        action
      ; sign_action
      ; resolve_action
      ; Breath.Assert.is_equal "ticket #1 in multisig storage" ticket1_ ticket1
      //; Assert.is_proposal_equal "#1 proposal" proposal1
      //  ({
      //    state            = Executed;
      //    signatures       = Map.literal [(bob.address, true)];
      //    proposer         = { actor = alice.address; timestamp = Tezos.get_now () };
      //    resolver         = Some { actor = bob.address; timestamp = Tezos.get_now () };
      //    contents         = [ Execute_lambda {
      //      metadata       = None;
      //      lambda         = None;
      //      args           = None;
      //    }]
      //  })
      ])

let case_receive_ticket =
  Breath.Model.case
  "test receiving ticket"
  "successuful receiving ticket"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in
      let multisig_address = multisig_contract.originated_address in

      let proxy_taddr =
        let mk_param : nat ticket -> nat ticket = fun (t : nat ticket) -> t in
        Proxy_ticket.init_transfer mk_param
      in

      let _ =
        let ticket_info = (1n,10n) in
        Proxy_ticket.transfer proxy_taddr (ticket_info, multisig_address)
      in

      let storage = Breath.Contract.storage_of multisig_contract in

      Breath.Result.reduce [
      ])

let test_suite =
  Breath.Model.suite "Suite for lambda proposal" [
    case_execute_lambda_proposal_without_ticket
  ; case_execute_lambda_proposal_with_returning_ticket
  //; case_execute_lambda_proposal_with_args_to_withdraw_partial_ticket
  //; case_execute_lambda_proposal_with_invalid_args
  //; case_execute_lambda_proposal_with_args_to_withdraw_whole_ticket
  //; case_execute_lambda_proposal_with_returning_zero_ticket
  ; case_receive_ticket
  ]

