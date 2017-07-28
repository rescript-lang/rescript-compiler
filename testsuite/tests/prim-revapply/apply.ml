(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                  Fabrice Le Fessant, INRIA Saclay                   *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

external ( @@ ) :  ('a -> 'b) -> 'a -> 'b = "%apply"

let f x = x + x
let g x = x * x
let h x = x + 1
let add x y = x + y

let _ =
  List.iter (fun x ->
    print_int x; print_newline ()
  )
    [
      f @@ 3; (* 6 *)
      g @@ f @@ 3; (* 36 *)
      f @@ g @@ 3; (* 18 *)
      h @@ g @@ f @@ 3; (* 37 *)
      add 4 @@ g @@ f @@ add 3 @@ add 2 @@ 3; (* 260 *)
    ]
external ( @@ ) :  ('a -> 'b) -> 'a -> 'b = "%apply"

let f x = x + x
let g x = x * x
let h x = x + 1
let add x y = x + y

let _ =
  List.iter (fun x ->
    print_int x; print_newline ()
  )
    [
      f @@ 3; (* 6 *)
      g @@ f @@ 3; (* 36 *)
      f @@ g @@ 3; (* 18 *)
      h @@ g @@ f @@ 3; (* 37 *)
      add 4 @@ g @@ f @@ add 3 @@ add 2 @@ 3; (* 260 *)
    ]
