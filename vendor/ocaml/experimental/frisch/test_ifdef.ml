type t =
  | A
  | DBG [@IFDEF DEBUG] of string
  | B

[%%IFDEF DEBUG]
let debug s = prerr_endline ([%GETENV DEBUG] ^ ":" ^ s)
let x = DBG "xxx"
[%%ELSE]
let debug _ = ()
let x = A
[%%END]

let f = function
  | A -> "A"
  | DBG s when [%IFDEF DEBUG] -> "DEBUG:" ^ s
  | B -> "B"

let () = debug "ABC"

let () =
  Printf.printf "compiled by user %s in directory %s\n%!"
    [%GETENV USER]
    [%GETENV PWD]

