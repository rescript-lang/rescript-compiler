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

module SS = struct
  type t = string
  let compare (x:t) (y:t) = Pervasives.compare x y
  let equal (x:t) (y:t) = x=y
  let hash = Hashtbl.hash
end
module SI = struct
  type t = int
  let compare (x:t) (y:t) = Pervasives.compare x y
  let equal (x:t) (y:t) = x=y
  let hash = Hashtbl.hash
end
module SSP = struct
  type t = string*string
  let compare (x:t) (y:t) = Pervasives.compare x y
  let equal (x:t) (y:t) = x=y
  let hash = Hashtbl.hash
end
module SSL = struct
  type t = string list
  let compare (x:t) (y:t) = Pervasives.compare x y
  let equal (x:t) (y:t) = x=y
  let hash = Hashtbl.hash
end
module SSA = struct
  type t = string array
  let compare (x:t) (y:t) = Pervasives.compare x y
  let equal (x:t) (y:t) = x=y
  let hash = Hashtbl.hash
end

module MS = Map.Make(SS)
module MI = Map.Make(SI)
module MSP = Map.Make(SSP)
module MSL = Map.Make(SSL)
module MSA = Map.Make(SSA)


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
    let find_opt = Hashtbl.find_opt
    let find_all = Hashtbl.find_all
    let replace = Hashtbl.replace
    let mem = Hashtbl.mem
    let iter = Hashtbl.iter
    let fold = Hashtbl.fold
    let length = Hashtbl.length
    let stats = Hashtbl.stats
    let filter_map_inplace = Hashtbl.filter_map_inplace
  end

module HS1 = HofM(MS)
module HI1 = HofM(MI)
module HSP = HofM(MSP)
module HSL = HofM(MSL)

(* Specific functorial hashes *)

module HS2 = Hashtbl.Make(SS)
module HI2 = Hashtbl.Make(SI)

(* Specific weak functorial hashes *)
module WS = Ephemeron.K1.Make(SS)
module WSP1 = Ephemeron.K1.Make(SSP)
module WSP2 = Ephemeron.K2.Make(SS)(SS)
module WSL = Ephemeron.K1.Make(SSL)
module WSA = Ephemeron.Kn.Make(SS)

(* Instantiating the test *)

module TS1 = Test(HS1)(MS)
module TS2 = Test(HS2)(MS)
module TI1 = Test(HI1)(MI)
module TI2 = Test(HI2)(MI)
module TSP = Test(HSP)(MSP)
module TSL = Test(HSL)(MSL)
module TWS  = Test(WS)(MS)
module TWSP1 = Test(WSP1)(MSP)
module TWSP2 = Test(WSP2)(MSP)
module TWSL = Test(WSL)(MSL)
module TWSA = Test(WSA)(MSA)

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
  let d = Array.make (Array.length data / 10) ([], "0") in
  let j = ref 0 in
  let rec mklist n =
    if n <= 0 || !j >= Array.length data then [] else begin
      let hd = fst data.(!j) in
      incr j;
      let tl = mklist (n-1) in
      hd :: tl
    end in
  for i = 0 to Array.length d - 1 do
    d.(i) <- (mklist (Random.int 16), string_of_int i)
  done;
  d

(* The test *)

let _ =
  printf "-- Random integers, large range\n%!";
  TI1.test (random_integers 20_000 1_000_000);
  printf "-- Random integers, narrow range\n%!";
  TI2.test (random_integers 20_000 1_000);
  let d =
    try file_data "../../LICENSE" with Sys_error _ -> string_data in
  printf "-- Strings, generic interface\n%!";
  TS1.test d;
  printf "-- Strings, functorial interface\n%!";
  TS2.test d;
  printf "-- Pairs of strings\n%!";
  TSP.test (pair_data d);
  printf "-- Lists of strings\n%!";
  TSL.test (list_data d);
  (* weak *)
  let d =
    try file_data "../../LICENSE" with Sys_error _ -> string_data in
  printf "-- Weak K1 -- Strings, functorial interface\n%!";
  TWS.test d;
  printf "-- Weak K1 -- Pairs of strings\n%!";
  TWSP1.test (pair_data d);
  printf "-- Weak K2 -- Pairs of strings\n%!";
  TWSP2.test (pair_data d);
  printf "-- Weak K1 -- Lists of strings\n%!";
  TWSL.test (list_data d);
  printf "-- Weak Kn -- Arrays of strings\n%!";
  TWSA.test (Array.map (fun (l,i) -> (Array.of_list l,i)) (list_data d))


let () =
  let h = Hashtbl.create 16 in
  for i = 1 to 1000 do Hashtbl.add h i (i * 2) done;
  Printf.printf "%i elements\n" (Hashtbl.length h);
  Hashtbl.filter_map_inplace (fun k v ->
      if k mod 100 = 0 then ((*Hashtbl.add h v v;*) Some (v / 100)) else None)
    h;
  let l = Hashtbl.fold (fun k v acc -> (k, v) :: acc) h [] in
  let l = List.sort compare l in
  List.iter (fun (k, v) -> Printf.printf "%i,%i\n" k v) l;
  Printf.printf "%i elements\n" (Hashtbl.length h)
