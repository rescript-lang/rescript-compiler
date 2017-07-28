(*************************************************************************)
(*                                                                       *)
(*                                OCaml                                  *)
(*                                                                       *)
(*         Damien Doligez, projet Gallium, INRIA Rocquencourt            *)
(*                                                                       *)
(*   Copyright 2008 Institut National de Recherche en Informatique et    *)
(*   en Automatique.  All rights reserved.  This file is distributed     *)
(*   under the terms of the Q Public License version 1.0.                *)
(*                                                                       *)
(*************************************************************************)

let debug = false;;

open Printf;;

module Hashed = struct
  type t = string list;;
  let equal x y =
    eprintf "equal: %s / %s\n" (List.hd x) (List.hd y);
    x = y
  ;;
  let hash x = Hashtbl.hash (List.hd x);;
end;;

module HT = Weak.Make (Hashed);;

let tbl = HT.create 7;;

let r = ref [];;

let bunch =
  if Array.length Sys.argv < 2
  then 10000
  else int_of_string Sys.argv.(1)
;;

Random.init 314;;

let random_string n =
  let result = String.create n in
  for i = 0 to n - 1 do
    result.[i] <- Char.chr (32 + Random.int 95);
  done;
  result
;;

let added = ref 0;;
let mistakes = ref 0;;

let print_status () =
  let (len, entries, sumbuck, buckmin, buckmed, buckmax) = HT.stats tbl in
  if entries > bunch * (!added + 1) then begin
    if debug then begin
      printf "\n===================\n";
      printf "len = %d\n" len;
      printf "entries = %d\n" entries;
      printf "sum of bucket sizes = %d\n" sumbuck;
      printf "min bucket = %d\n" buckmin;
      printf "med bucket = %d\n" buckmed;
      printf "max bucket = %d\n" buckmax;
      printf "GC count = %d\n" (Gc.quick_stat ()).Gc.major_collections;
      flush stdout;
    end;
    incr mistakes;
  end;
  added := 0;
;;

Gc.create_alarm print_status;;

for j = 0 to 99 do
  r := [];
  incr added;

  for i = 1 to bunch do
    let c = random_string 7 in
    r := c :: !r;
    HT.add tbl !r;
  done;
done;;

if !mistakes < 5 then printf "pass\n" else printf "fail\n";;
