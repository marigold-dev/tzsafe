#import "ligo-breathalyzer/lib/lib.mligo" "Breath"
#import "./common/helper.mligo" "Helper"
#import "../app/main_unit.mligo" "App"

let transfer_only_contract (addr, storage: address * unit) : operation list * unit =
  let contr = Tezos.get_contract_with_error addr "contract doesn't exist" in
  [Tezos.transaction () 10tez contr], storage

let originate_transfer_only_contract (level: Breath.Logger.level) =
  Breath.Contract.originate
    level
    "transfer only contract"
    transfer_only_contract
    ()
    100tez

let case_receive_tez =
  Breath.Model.case
  "test multisig wallet receiving tez"
  "successuful receiving tez"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, carol)) = Breath.Context.init_default () in
      let signers : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (signers, 1n) in
      let multisig = Helper.originate level App.main init_storage 0tez in
      let contract = originate_transfer_only_contract level in

      let action = Breath.Context.act_as carol (fun (_u:unit) -> (Breath.Contract.transfert_to contract multisig.originated_address 0tez)) in

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
      let signers : address set = Set.literal [alice.address] in
      let init_storage = Helper.init_storage (signers, 0n) in
      let multisig = Helper.originate level App.main init_storage 0tez in
      let contract = originate_transfer_only_contract level in

      let action = Breath.Context.act_as alice (fun (_u:unit) -> (Breath.Contract.transfert_to contract multisig.originated_address 0tez)) in

      Breath.Result.reduce [
        Breath.Expect.fail_with_message "Threshold must be greater than 1" action
      ])

let test_suite =
  Breath.Model.suite "Suite for Tez" [
    case_receive_tez
  ; case_invalidated_setting
  ]
