(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*              Jacques Garrigue, Nagoya University                    *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* PR#6435 *)
let count = ref 0

module F (M : sig
           type t
           module Set : Set.S with type elt = t
         end) =
struct
 let test set = count := (M.Set.cardinal set) + !count
end

module M = F (Offset)

let () = M.test (Offset.M.Set.singleton "42")
(* 
  here we assume value access does not have side effect 
  however, the module should be included 
  since it may contain side effect unless we 
  analyze it does not have side effect
*)
let v = Pr6726.Test.v

let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq f a b = Mt_global.collect_eq test_id suites f a b 

let () = eq __LOC__ !count 1 

let () = Mt.from_pair_suites __MODULE__ !suites
