(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*              Damien Doligez, projet Para, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Random allocation test *)

(*
  Allocate arrays of strings, of random sizes in [0..1000[, and put them
  into an array of 32768.  Replace a randomly-selected array with a new
  random-length array.  Reiterate ad infinitum.
*)

let l = 32768;;
let m = 1000;;

let ar = Array.make l "";;

Random.init 1234;;

let compact_flag = ref false;;

let main () =
  while true do
    for i = 1 to 100000 do
      ar.(Random.int l) <- String.create (Random.int m);
    done;
    if !compact_flag then Gc.compact () else Gc.full_major ();
    print_newline ();
    Gc.print_stat stdout;
    flush stdout;
  done
;;

let argspecs = [
  "-c", Arg.Set compact_flag, "do heap compactions";
];;

Arg.parse argspecs (fun _ -> ()) "Usage: alloc [-c]";;

main ();;
