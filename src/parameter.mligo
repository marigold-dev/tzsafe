#import "./fa2/fa2.mligo" "FA2"

module Types = struct
    type proposal_params = 
    [@layout:comb]
    {
        target_fa2 : address;
        transfers  : FA2.transfer;
    }

    type proposal_number = nat

    type t =
    | Create_proposal of (proposal_params)
    | Sign_proposal of (nat)
end
