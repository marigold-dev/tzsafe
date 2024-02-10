#import "ligo-breathalyzer/lib/lib.mligo" "Breath"
#import "./common/helper.mligo" "Helper"
#import "./common/mock_contract.mligo" "Mock"
#import "../src/tzsafe.mligo" "App"

let case_receive_tez =
  Breath.Model.case
  "test multisig wallet receiving tez"
  "successuful receiving tez"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let multisig = Helper.originate level init_storage 0tez in
      let contract = Mock.originate_transfer_only_contract level multisig.originated_address in

      let action = Breath.Context.act_as carol (fun (_u:unit) -> (Breath.Contract.transfer_with_entrypoint_to contract "default" ()  0tez)) in

      let balance = Breath.Contract.balance_of multisig in

      Breath.Result.reduce [
        action;
        Breath.Assert.is_equal "multisig receive 10tez" balance 10tez
      ])

let case_invalidated_setting =
  Breath.Model.case
  "invalidated setting"
  "fail to receive tez"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, _bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address] in
      let init_storage = Helper.init_storage (owners, 0n) in
      let multisig = Helper.originate level init_storage 0tez in
      let contract = Mock.originate_transfer_only_contract level multisig.originated_address in

      let action = Breath.Context.act_as alice (fun (_u:unit) -> (Breath.Contract.transfer_with_entrypoint_to contract "default" ()  0tez)) in

      Breath.Result.reduce [
        Breath.Expect.fail_with_message "Threshold must be greater than 1" action
      ])

let test_suite =
  Breath.Model.suite "Suite for Tez" [
    case_receive_tez
  ; case_invalidated_setting
  ]
