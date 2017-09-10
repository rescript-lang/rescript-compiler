
[@@@bs.config no_export ]

let f x =
  match x with
  | `A -> "A" 
  | `B -> "B"

let f1 x = match x with `A -> "A"

let ff =
  let x = 66 in   
  if x >= 66 then
    "B"
  else "A"      
let () = Js.log (f `A,ff, f1 `A)

(* local variables: *)
(* compile-command: "bsc.exe -I ../runtime -I ../stdlib -bs-main polyvar_test.ml" *)
(* end: *)
