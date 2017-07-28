(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2011 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Hashtable operations, using maps as a reference *)

open Printf

module Test(H: Hashtbl.S) (M: Map.S with type key = H.key) = struct

  let incl_mh m h =
    try
      M.iter
        (fun k d ->
          let d' = H.find h k in if d <> d' then raise Exit)
        m;
      true
    with Exit | Not_found -> false

  let domain_hm h m =
    try
      H.iter
        (fun k d -> if not (M.mem k m) then raise Exit)
        h;
      true
    with Exit -> false

  let incl_hm h m =
    try
      H.iter
        (fun k d ->
           let d' = M.find k m in if d <> d' then raise Exit)
        h;
      true
    with Exit | Not_found -> false

  let test data =
    let n = Array.length data in
    let h = H.create 51 and m = ref M.empty in
    (* Insert all data with H.add *)
    Array.iter
      (fun (k, d) -> H.add h k d; m := M.add k d !m)
      data;
    printf "Insertion: %s\n"
           (if incl_mh !m h && domain_hm h !m then "passed" else "FAILED");
    (* Insert all data with H.replace *)
    H.clear h; m := M.empty;
    Array.iter
      (fun (k, d) -> H.replace h k d; m := M.add k d !m)
      data;
    printf "Insertion: %s\n"
           (if incl_mh !m h && incl_hm h !m then "passed" else "FAILED");
    (* Remove some of the data *)
    for i = 0 to n/3 - 1 do
      let (k, _) = data.(i) in H.remove h k; m := M.remove k !m
    done;
    printf "Removal: %s\n"
           (if incl_mh !m h && incl_hm h !m then "passed" else "FAILED")

end

module MS = Map.Make(struct type t = string
                            let compare (x:t) (y:t) = Pervasives.compare x y
                     end)
module MI = Map.Make(struct type t = int
                            let compare (x:t) (y:t) = Pervasives.compare x y
                     end)

module MSP = Map.Make(struct type t = string*string
                            let compare (x:t) (y:t) = Pervasives.compare x y
                     end)

module MSL = Map.Make(struct type t = string list
                            let compare (x:t) (y:t) = Pervasives.compare x y
                     end)

(* Generic hash wrapped as a functorial hash *)

module HofM (M: Map.S) : Hashtbl.S with type key = M.key =
  struct
    type key = M.key
    type 'a t = (key, 'a) Hashtbl.t
    let create s = Hashtbl.create s
    let clear = Hashtbl.clear
    let reset = Hashtbl.reset
    let copy = Hashtbl.copy
    let add = Hashtbl.add
    let remove = Hashtbl.remove
    let find = Hashtbl.find
    let find_all = Hashtbl.find_all
    let replace = Hashtbl.replace
    let mem = Hashtbl.mem
    let iter = Hashtbl.iter
    let fold = Hashtbl.fold
    let length = Hashtbl.length
    let stats = Hashtbl.stats
  end

module HS1 = HofM(MS)
module HI1 = HofM(MI)
module HSP = HofM(MSP)
module HSL = HofM(MSL)

(* Specific functorial hashes *)

module HS2 = Hashtbl.Make(struct type t = string
                                 let equal (x:t) (y:t) = x=y
                                 let hash = Hashtbl.hash end)

module HI2 = Hashtbl.Make(struct type t = int
                                 let equal (x:t) (y:t) = x=y
                                 let hash = Hashtbl.hash end)
(* Instantiating the test *)

module TS1 = Test(HS1)(MS)
module TS2 = Test(HS2)(MS)
module TI1 = Test(HI1)(MI)
module TI2 = Test(HI2)(MI)
module TSP = Test(HSP)(MSP)
module TSL = Test(HSL)(MSL)

(* Data set: strings from a file, associated with their line number *)

let file_data filename =
  let ic = open_in filename in
  let lineno = ref 0 in
  let data = ref [] in
  begin try
    while true do
      let l = input_line ic in
      incr lineno;
      data := (l, !lineno) :: !data
    done
  with End_of_file -> ()
  end;
  close_in ic;
  Array.of_list !data

(* Data set: fixed strings *)

let string_data = [|
  "Si", 0; "non", 1; "e", 2; "vero", 3; "e", 4; "ben", 5; "trovato", 6;
  "An", 10; "apple", 11; "a", 12; "day", 13; "keeps", 14; "the", 15;
  "doctor", 16; "away", 17;
  "Pierre", 20; "qui", 21; "roule", 22; "n'amasse", 23; "pas", 24; "mousse", 25;
  "Asinus", 30; "asinum", 31; "fricat", 32
|]

(* Data set: random integers *)

let random_integers num range =
  let data = Array.make num (0,0) in
  for i = 0 to num - 1 do
    data.(i) <- (Random.int range, i)
  done;
  data

(* Data set: pairs *)

let pair_data data =
  Array.map (fun (k, d) -> ((k, k), d)) data

(* Data set: lists *)

let list_data data =
  let d = Array.make (Array.length data / 10) ([], 0) in
  let j = ref 0 in
  let rec mklist n =
    if n <= 0 || !j >= Array.length data then [] else begin
      let hd = fst data.(!j) in
      incr j;
      let tl = mklist (n-1) in
      hd :: tl
    end in
  for i = 0 to Array.length d - 1 do
    d.(i) <- (mklist (Random.int 16), i)
  done;
  d

(* The test *)

let _ =
  printf "-- Random integers, large range\n%!";
  TI1.test (random_integers 100_000 1_000_000);
  printf "-- Random integers, narrow range\n%!";
  TI2.test (random_integers 100_000 1_000);
  let d =
    try file_data "../../LICENSE" with Sys_error _ -> string_data in
  printf "-- Strings, generic interface\n%!";
  TS1.test d;
  printf "-- Strings, functorial interface\n%!";
  TS2.test d;
  printf "-- Pairs of strings\n%!";
  TSP.test (pair_data d);
  printf "-- Lists of strings\n%!";
  TSL.test (list_data d)
