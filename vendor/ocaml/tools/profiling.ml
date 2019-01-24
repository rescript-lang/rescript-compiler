(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*       Damien Doligez and Francois Rouaix, INRIA Rocquencourt           *)
(*    Ported to Caml Special Light by John Malecki and Xavier Leroy       *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Run-time library for profiled programs *)

type profiling_counters = (string * (string * int array)) list

let counters = ref ([] : profiling_counters);;
let incr a i = a.(i) <- a.(i) + 1;;

exception Bad_profile

let dump_counters () =
  let dumpfile =
    try Sys.getenv "OCAMLPROF_DUMP" with Not_found -> "ocamlprof.dump"
  in
  begin try
    let ic = open_in_bin dumpfile in
    let prevl = (input_value ic : profiling_counters) in
    close_in ic;
    List.iter2
      (fun (curname, (curmodes,curcount)) (prevname, (prevmodes,prevcount)) ->
        if curname <> prevname
        || curmodes <> prevmodes
        || Array.length curcount <> Array.length prevcount
        then raise Bad_profile)
      !counters prevl;
    List.iter2
      (fun (_curname, (_,curcount)) (_prevname, (_,prevcount)) ->
        for i = 0 to Array.length curcount - 1 do
          curcount.(i) <- curcount.(i) + prevcount.(i)
        done)
      !counters prevl
  with _ -> ()
  end;
  begin try
    let oc = open_out_bin dumpfile in
    output_value oc !counters;
    close_out oc
  with _ -> ()
  end

let _ = at_exit dump_counters
