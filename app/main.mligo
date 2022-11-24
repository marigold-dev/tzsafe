#include "../src/lib.mligo"

type storage = string
type input = unit * storage
type output = operation list * storage

let main (_input: input) : output =
  let new_store = some_func () in
  [], new_store
