let functions = ref ([]: (int -> int) list)

let register f =
  functions := f :: !functions

let get_functions () =
  !functions
