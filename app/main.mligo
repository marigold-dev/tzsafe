#include "../src/contract.mligo"

type storage = string
type input = unit * storage
type output = operation list * storage

let main (_action, store: input) : output =
  [], store
