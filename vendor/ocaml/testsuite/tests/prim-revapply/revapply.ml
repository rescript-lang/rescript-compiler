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

external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"

let f x = x + x
let g x = x * x
let h x = x + 1
let add x y = x + y

let _ =
  List.iter (fun x ->
    print_int x; print_newline ()
  )
    [
      3 |> f; (* 6 *)
      3 |> f |> g; (* 36 *)
      3 |> g |> f; (* 18 *)
      3 |> f |> g |> h; (* 37 *)
      3 |> add 2 |> add 3 |> f |> g |> add 4; (* 260 *)
    ]
