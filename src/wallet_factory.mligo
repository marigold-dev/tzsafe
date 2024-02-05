
[@entry]
let factory (f : unit -> operation list) (() : unit) : operation list * unit = 
  f (), unit