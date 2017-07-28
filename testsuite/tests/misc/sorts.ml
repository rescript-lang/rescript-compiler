(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*          Damien Doligez, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2000 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Test bench for sorting algorithms. *)


(*
  ocamlopt -noassert sorts.ml -cclib -lunix
*)

open Printf;;

(*
  Criteres:
  0. overhead en pile: doit etre logn au maximum.
  1. stable ou non.
  2. overhead en espace.
  3. vitesse.
*)

(************************************************************************)
(* auxiliary functions *)

let rec exp2 n = if n <= 0 then 1 else 2 * exp2 (n-1);;
let id x = x;;
let postl x y = Array.of_list y;;
let posta x y = x;;

let mkconst n = Array.make n 0;;
let chkconst _ n a = (a = mkconst n);;

let mksorted n =
  let a = Array.make n 0 in
  for i = 0 to n - 1 do
    a.(i) <- i;
  done;
  a
;;
let chksorted _ n a = (a = mksorted n);;

let mkrev n =
  let a = Array.make n 0 in
  for i = 0 to n - 1 do
    a.(i) <- n - 1 - i;
  done;
  a
;;
let chkrev _ n a = (a = mksorted n);;

let seed = ref 0;;
let random_reinit () = Random.init !seed;;

let random_get_state () =
  let a = Array.make 55 0 in
  for i = 0 to 54 do a.(i) <- Random.bits (); done;
  Random.full_init a;
  a
;;
let random_set_state a = Random.full_init a;;

let chkgen mke cmp rstate n a =
  let marks = Array.make n (-1) in
  let skipmarks l =
    if marks.(l) = -1 then l else begin
      let m = ref marks.(l) in
      while marks.(!m) <> -1 do incr m; done;
      marks.(l) <- !m;
      !m
    end
  in
  let linear e l =
    let l = skipmarks l in
    let rec loop l =
      if cmp a.(l) e > 0 then raise Exit
      else if e = a.(l) then marks.(l) <- l+1
      else loop (l+1)
    in loop l
  in
  let rec dicho e l r =
    if l = r then linear e l
    else begin
      assert (l < r);
      let m = (l + r) / 2 in
      if cmp a.(m) e >= 0 then dicho e l m else dicho e (m + 1) r
    end
  in
  try
    for i = 0 to n-2 do if cmp a.(i) a.(i+1) > 0 then raise Exit; done;
    random_set_state rstate;
    for i = 0 to n-1 do dicho (mke i) 0 (Array.length a - 1); done;
    true
  with Exit | Invalid_argument _ -> false;
;;

let mkrand_dup n =
  let a = Array.make n 0 in
  for i = 0 to (n-1) do a.(i) <- Random.int n; done;
  a
;;

let chkrand_dup rstate n a =
  chkgen (fun i -> Random.int n) compare rstate n a
;;

let mkrand_nodup n =
  let a = Array.make n 0 in
  for i = 0 to (n-1) do a.(i) <- Random.bits (); done;
  a
;;

let chkrand_nodup rstate n a =
  chkgen (fun i -> Random.bits ()) compare rstate n a
;;

let mkfloats n =
  let a = Array.make n 0.0 in
  for i = 0 to (n-1) do a.(i) <- Random.float 1.0; done;
  a
;;

let chkfloats rstate n a =
  chkgen (fun i -> Random.float 1.0) compare rstate n a
;;

type record = {
  s1 : string;
  s2 : string;
  i1 : int;
  i2 : int;
};;

let rand_string () =
  let len = Random.int 10 in
  let s = String.create len in
  for i = 0 to len-1 do
    s.[i] <- Char.chr (Random.int 256);
  done;
  s
;;

let mkrec1 b i = {
  s1 = rand_string ();
  s2 = rand_string ();
  i1 = Random.int b;
  i2 = i;
};;

let mkrecs b n = Array.init n (mkrec1 b);;

let mkrec1_rev b i = {
  s1 = rand_string ();
  s2 = rand_string ();
  i1 = - i;
  i2 = i;
};;

let mkrecs_rev n = Array.init n (mkrec1_rev 0);;

let cmpstr r1 r2 =
  let c1 = compare r1.s1 r2.s1 in
  if c1 = 0 then compare r1.s2 r2.s2 else c1
;;
let lestr r1 r2 =
  let c1 = compare r1.s1 r2.s1 in
  if c1 = 0 then r1.s2 <= r2.s2 else (c1 < 0)
;;
let chkstr b rstate n a = chkgen (mkrec1 b) cmpstr rstate n a;;

let cmpint r1 r2 = compare r1.i1 r2.i1;;
let leint r1 r2 = r1.i1 <= r2.i1;;
let chkint b rstate n a = chkgen (mkrec1 b) cmpint rstate n a;;

let cmplex r1 r2 =
  let c1 = compare r1.i1 r2.i1 in
  if c1 = 0 then compare r1.i2 r2.i2 else c1
;;
let lelex r1 r2 =
  let c1 = compare r1.i1 r2.i1 in
  if c1 = 0 then r1.i2 <= r2.i2 else (c1 < 0)
;;
let chklex b rstate n a = chkgen (mkrec1 b) cmplex rstate n a;;

(************************************************************************)

let lens = [
  0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 28;
  100; 127; 128; 129; 191; 192; 193; 506;
  1000; 1023; 1024; 1025; 1535; 1536; 1537; 2323;
  4000; 4094; 4096; 4098; 5123;
];;

type ('a, 'b, 'c, 'd) aux = {
  prepf : ('a -> 'a -> int) -> ('a -> 'a -> bool) -> 'b;
  prepd : 'a array -> 'c;
  postd : 'a array -> 'd -> 'a array;
};;

let ll = { prepf = (fun x y -> y); prepd = Array.to_list; postd = postl };;
let lc = { prepf = (fun x y -> x); prepd = Array.to_list; postd = postl };;
let al = { prepf = (fun x y -> y); prepd = id; postd = posta };;
let ac = { prepf = (fun x y -> x); prepd = id; postd = posta };;

type 'a outcome = Value of 'a | Exception of exn;;

let numfailed = ref 0;;

let test1 name f prepdata postdata cmp desc mk chk =
  random_reinit ();
  printf "  %s with %s" name desc;
  let i = ref 0 in
  List.iter (fun n ->
      if !i = 0 then printf "\n    "; incr i; if !i > 11 then i := 0;
      printf "%5d" n; flush stdout;
      let rstate = random_get_state () in
      let a = mk n in
      let input = prepdata a in
      let output = try Value (f cmp input) with e -> Exception e in
      printf "."; flush stdout;
      begin match output with
      | Value v ->
         if not (chk rstate n (postdata a v))
         then (incr numfailed; printf "\n*** FAIL\n")
      | Exception e ->
         incr numfailed; printf "\n*** %s\n" (Printexc.to_string e)
      end;
      flush stdout;
    ) lens;
  printf "\n";
;;

let test name stable f1 f2 aux1 aux2 =
  printf "Testing %s...\n" name;
  let t a b c d = test1 name f1 aux1.prepd aux1.postd a b c d in
  let cmp = aux1.prepf compare (<=) in
  t cmp "constant ints" mkconst chkconst;
  t cmp "sorted ints" mksorted chksorted;
  t cmp "reverse-sorted ints" mkrev chkrev;
  t cmp "random ints (many dups)" mkrand_dup chkrand_dup;
  t cmp "random ints (few dups)" mkrand_nodup chkrand_nodup;
(*
  let t a b c d = test1 name f3 aux3.prepd aux3.postd a b c d in
  t cmp "random floats" mkfloats chkfloats;
*)
  let t a b c d = test1 name f2 aux2.prepd aux2.postd a b c d in
  let cmp = aux2.prepf cmpstr lestr in
  t cmp "records (str)" (mkrecs 1) (chkstr 1);
  let cmp = aux2.prepf cmpint leint in
  List.iter (fun m -> t cmp (sprintf "records (int[%d])" m) (mkrecs m)
                        (chkint m)
            ) [1; 10; 100; 1000];
  if stable then
    List.iter (fun m -> t cmp (sprintf "records (int[%d]) [stable]" m)
                          (mkrecs m) (chklex m)
              ) [1; 10; 100; 1000];
;;

(************************************************************************)

(* Warning: rpt_timer cannot be used for the array sorts because
   the sorting functions have effects.
*)

let rpt_timer1 repeat f x =
  Gc.compact ();
  ignore (f x);
  let st = Sys.time () in
  for i = 1 to repeat do ignore (f x); done;
  let en = Sys.time () in
  en -. st
;;

let rpt_timer f x =
  let repeat = ref 1 in
  let t = ref (rpt_timer1 !repeat f x) in
  while !t < 0.2 do
    repeat := 10 * !repeat;
    t := rpt_timer1 !repeat f x;
  done;
  if !t < 2.0 then begin
    repeat := (int_of_float (10. *. (float !repeat) /. !t) + 1);
    t := rpt_timer1 !repeat f x;
  end;
  !t /. (float !repeat)
;;

let timer f x =
  let st = Sys.time () in
  ignore (f x);
  let en = Sys.time () in
  (en -. st)
;;

let table1 limit f mkarg =
  printf "  %10s  %9s  %9s  %9s  %9s  %9s\n" "n" "t1" "t2" "t3" "t4" "t5";
  let sz = ref 49151 in
  while !sz < int_of_float (2. ** float limit) do
    begin try
      printf "  %10d  " !sz; flush stdout;
      for i = 0 to 4 do
        let arg = mkarg !sz in
        let t = timer f arg in
        printf " %.2e   " t; flush stdout;
      done;
      printf "\n";
    with e -> printf "*** %s\n" (Printexc.to_string e);
    end;
    flush stdout;
    sz := 2 * !sz + 1;
  done;
;;

let table2 limit f mkarg =
  printf "  %10s  %9s  %9s  %9s  %9s  %9s\n"
         " n" "t" "t/n" "t/nlogn" "t/nlog^2n" "t/n^2";
  let sz = ref 49151 in
  while float !sz < 2. ** float limit do
    begin try
      printf "  %10d   " !sz; flush stdout;
      Gc.compact ();
      let arg = mkarg !sz in
      let t = timer f arg in
      let n = float !sz in
      let logn = log (float !sz) /. log 2. in
      printf "%.2e   %.2e   %.2e   %.2e   %.2e\n"
             t (t/.n) (t/.n/.logn) (t/.n/.logn/.logn) (t/.n/.n);
    with e -> printf "*** %s\n" (Printexc.to_string e);
    end;
    flush stdout;
    sz := 2 * !sz + 1;
  done;
;;

let table3 limit f mkarg =
  printf "  %10s  %9s  %9s  %9s  %9s  %9s\n" "n" "t1" "t2" "t3" "t4" "t5";
  let sz = ref 2 in
  while float !sz < 2. ** float limit do
    begin try
      printf "  %10d  " !sz; flush stdout;
      for i = 0 to 4 do
        let arg = mkarg !sz in
        let t = rpt_timer f arg in
        printf " %.2e   " t; flush stdout;
      done;
      printf "\n";
    with e -> printf "*** %s\n" (Printexc.to_string e);
    end;
    flush stdout;
    sz := 2 * !sz + 1;
  done;
;;

(************************************************************************)

(* benchmarks:
   1a. random records, sorted with two keys
   1b. random integers
   1c. random floats

   2a. integers, constant
   2b. integers, already sorted
   2c. integers, reverse sorted

   only for short lists:
   3a. random records, sorted with two keys
   3b. random integers
   3c. random floats
*)
let bench1a limit name f aux =

  (* Don't do benchmarks with assertions enabled. *)
  assert (not true);

  random_reinit ();

  printf "\n%s with random records [10]:\n" name;
  let cmp = aux.prepf cmplex lelex in
  table1 limit (f cmp) (fun n -> aux.prepd (mkrecs 10 n));
;;

let bench1b limit name f aux =

  (* Don't do benchmarks with assertions enabled. *)
  assert (not true);

  random_reinit ();

  printf "\n%s with random integers:\n" name;
  let cmp = aux.prepf (-) (<=) in
  table1 limit (f cmp) (fun n -> aux.prepd (mkrand_nodup n));
;;

let bench1c limit name f aux =

  (* Don't do benchmarks with assertions enabled. *)
  assert (not true);

  random_reinit ();

  printf "\n%s with random floats:\n" name;
  let cmp = aux.prepf compare (<=) in
  table1 limit (f cmp) (fun n -> aux.prepd (mkfloats n));
;;

let bench2 limit name f aux =

  (* Don't do benchmarks with assertions enabled. *)
  assert (not true);

  printf "\n%s with constant integers:\n" name;
  let cmp = aux.prepf compare (<=) in
  table2 limit (f cmp) (fun n -> aux.prepd (mkconst n));

  printf "\n%s with sorted integers:\n" name;
  let cmp = aux.prepf compare (<=) in
  table2 limit (f cmp) (fun n -> aux.prepd (mksorted n));

  printf "\n%s with reverse-sorted integers:\n" name;
  let cmp = aux.prepf compare (<=) in
  table2 limit (f cmp) (fun n -> aux.prepd (mkrev n));
;;

let bench3a limit name f aux =

  (* Don't do benchmarks with assertions enabled. *)
  assert (not true);

  random_reinit ();

  printf "\n%s with random records [10]:\n" name;
  let cmp = aux.prepf cmplex lelex in
  table3 limit (f cmp) (fun n -> aux.prepd (mkrecs 10 n));
;;

let bench3b limit name f aux =

  (* Don't do benchmarks with assertions enabled. *)
  assert (not true);

  random_reinit ();

  printf "\n%s with random integers:\n" name;
  let cmp = aux.prepf (-) (<=) in
  table3 limit (f cmp) (fun n -> aux.prepd (mkrand_nodup n));
;;

let bench3c limit name f aux =

  (* Don't do benchmarks with assertions enabled. *)
  assert (not true);

  random_reinit ();

  printf "\n%s with random floats:\n" name;
  let cmp = aux.prepf compare (<=) in
  table3 limit (f cmp) (fun n -> aux.prepd (mkfloats n));
;;

(************************************************************************)
(* merge sort on lists *)

(* FIXME to do: cutoff
         to do: cascader les pattern-matchings (enlever les paires)
         to do: fermeture intermediaire pour merge
*)
let (@@) = List.rev_append;;

let lmerge_1a cmp l =
  let rec init accu = function
    | [] -> accu
    | e::rest -> init ([e] :: accu) rest
  in
  let rec merge rest accu2 accu l1 l2 = (* l1,l2,rest are forward;
                                           accu,accu2 are rev *)
    match l1, l2 with
    | []    , _      -> mergepairs ((l2 @@ accu)::accu2) rest
    | _     , []     -> mergepairs ((l1 @@ accu)::accu2) rest
    | h1::t1, h2::t2 -> if cmp h1 h2 <= 0
                        then merge rest accu2 (h1::accu) t1 l2
                        else merge rest accu2 (h2::accu) l1 t2
  and merge_rev rest accu2 accu l1 l2 = (* accu, accu2 are forward;
                                           l1,l2,rest are rev *)
    match l1, l2 with
    | []    , _      -> mergepairs_rev ((l2 @@ accu)::accu2) rest
    | _     , []     -> mergepairs_rev ((l1 @@ accu)::accu2) rest
    | h1::t1, h2::t2 -> if cmp h2 h1 <= 0
                        then merge_rev rest accu2 (h1::accu) t1 l2
                        else merge_rev rest accu2 (h2::accu) l1 t2
  and mergepairs accu = function       (* accu is rev, arg is forward *)
    | [] -> mergeall_rev accu
    | [l] -> mergeall_rev ((List.rev l)::accu)
    | l1::l2::rest -> merge rest accu [] l1 l2
  and mergepairs_rev accu = function   (* accu is forward, arg is rev *)
    | [] -> mergeall accu
    | [l] -> mergeall ((List.rev l)::accu)
    | l1::l2::rest -> merge_rev rest accu [] l1 l2
  and mergeall = function              (* arg is forward *)
    | [] -> []
    | [l] -> l
    | llist -> mergepairs [] llist
  and mergeall_rev = function          (* arg is rev *)
    | [] -> []
    | [l] -> List.rev l
    | llist -> mergepairs_rev [] llist
  in
  mergeall_rev (init [] l)
;;

let lmerge_1b cmp l =
  let rec init accu = function
    | [] -> accu
    | [e] -> [e] :: accu
    | e1::e2::rest ->
        init ((if cmp e1 e2 <= 0 then [e2;e1] else [e1;e2])::accu) rest
  in
  let rec merge rest accu2 accu l1 l2 = (* l1,l2,rest are forward;
                                           accu,accu2 are rev *)
    match l1, l2 with
    | []    , _      -> mergepairs ((l2 @@ accu)::accu2) rest
    | _     , []     -> mergepairs ((l1 @@ accu)::accu2) rest
    | h1::t1, h2::t2 -> if cmp h1 h2 <= 0
                        then merge rest accu2 (h1::accu) t1 l2
                        else merge rest accu2 (h2::accu) l1 t2
  and merge_rev rest accu2 accu l1 l2 = (* accu, accu2 are forward;
                                           l1,l2,rest are rev *)
    match l1, l2 with
    | []    , _      -> mergepairs_rev ((l2 @@ accu)::accu2) rest
    | _     , []     -> mergepairs_rev ((l1 @@ accu)::accu2) rest
    | h1::t1, h2::t2 -> if cmp h2 h1 <= 0
                        then merge_rev rest accu2 (h1::accu) t1 l2
                        else merge_rev rest accu2 (h2::accu) l1 t2
  and mergepairs accu = function       (* accu is rev, arg is forward *)
    | [] -> mergeall_rev accu
    | [l] -> mergeall_rev ((List.rev l)::accu)
    | l1::l2::rest -> merge rest accu [] l1 l2
  and mergepairs_rev accu = function   (* accu is forward, arg is rev *)
    | [] -> mergeall accu
    | [l] -> mergeall ((List.rev l)::accu)
    | l1::l2::rest -> merge_rev rest accu [] l1 l2
  and mergeall = function              (* arg is forward *)
    | [] -> []
    | [l] -> l
    | llist -> mergepairs [] llist
  and mergeall_rev = function          (* arg is rev *)
    | [] -> []
    | [l] -> List.rev l
    | llist -> mergepairs_rev [] llist
  in
  mergeall_rev (init [] l)
;;

let lmerge_1c cmp l =
  let rec init accu = function
    | [] -> accu
    | [e] -> [e] :: accu
    | e1::e2::rest ->
        init ((if cmp e1 e2 <= 0 then [e2;e1] else [e1;e2])::accu) rest
  in
  let rec merge rest accu2 accu l1 l2 = (* l1,l2,rest are forward;
                                           accu,accu2 are rev *)
    match l1 with
    | [] -> mergepairs ((l2 @@ accu)::accu2) rest
    | h1::t1 ->
       match l2 with
       | []     -> mergepairs ((l1 @@ accu)::accu2) rest
       | h2::t2 -> if cmp h1 h2 <= 0
                   then merge rest accu2 (h1::accu) t1 l2
                   else merge rest accu2 (h2::accu) l1 t2
  and merge_rev rest accu2 accu l1 l2 = (* accu, accu2 are forward;
                                           l1,l2,rest are rev *)
    match l1 with
    | [] -> mergepairs_rev ((l2 @@ accu)::accu2) rest
    | h1::t1 ->
       match l2 with
       | []     -> mergepairs_rev ((l1 @@ accu)::accu2) rest
       | h2::t2 -> if cmp h2 h1 <= 0
                   then merge_rev rest accu2 (h1::accu) t1 l2
                   else merge_rev rest accu2 (h2::accu) l1 t2
  and mergepairs accu = function       (* accu is rev, arg is forward *)
    | [] -> mergeall_rev accu
    | [l] -> mergeall_rev ((List.rev l)::accu)
    | l1::l2::rest -> merge rest accu [] l1 l2
  and mergepairs_rev accu = function   (* accu is forward, arg is rev *)
    | [] -> mergeall accu
    | [l] -> mergeall ((List.rev l)::accu)
    | l1::l2::rest -> merge_rev rest accu [] l1 l2
  and mergeall = function              (* arg is forward *)
    | [] -> []
    | [l] -> l
    | llist -> mergepairs [] llist
  and mergeall_rev = function          (* arg is rev *)
    | [] -> []
    | [l] -> List.rev l
    | llist -> mergepairs_rev [] llist
  in
  mergeall_rev (init [] l)
;;

let lmerge_1d cmp l =
  let rec init accu = function
    | [] -> accu
    | [e] -> [e] :: accu
    | e1::e2::rest ->
        init ((if cmp e1 e2 <= 0 then [e2;e1] else [e1;e2])::accu) rest
  in
  let rec merge rest accu2 accu l1 l2 = (* l1,l2,rest are forward;
                                           accu,accu2 are rev *)
    let merge_rest_accu2 accu l1 l2 =
      match l1 with
      | [] -> mergepairs ((l2 @@ accu)::accu2) rest
      | h1::t1 ->
         match l2 with
         | []     -> mergepairs ((l1 @@ accu)::accu2) rest
         | h2::t2 -> if cmp h1 h2 <= 0
                     then merge rest accu2 (h1::accu) t1 l2
                     else merge rest accu2 (h2::accu) l1 t2
    in merge_rest_accu2 accu l1 l2
  and merge_rev rest accu2 accu l1 l2 = (* accu, accu2 are forward;
                                           l1,l2,rest are rev *)
    let merge_rev_rest_accu2 accu l1 l2 =
      match l1 with
      | [] -> mergepairs_rev ((l2 @@ accu)::accu2) rest
      | h1::t1 ->
         match l2 with
         | []     -> mergepairs_rev ((l1 @@ accu)::accu2) rest
         | h2::t2 -> if cmp h2 h1 <= 0
                     then merge_rev rest accu2 (h1::accu) t1 l2
                     else merge_rev rest accu2 (h2::accu) l1 t2
    in merge_rev_rest_accu2 accu l1 l2
  and mergepairs accu = function       (* accu is rev, arg is forward *)
    | [] -> mergeall_rev accu
    | [l] -> mergeall_rev ((List.rev l)::accu)
    | l1::l2::rest -> merge rest accu [] l1 l2
  and mergepairs_rev accu = function   (* accu is forward, arg is rev *)
    | [] -> mergeall accu
    | [l] -> mergeall ((List.rev l)::accu)
    | l1::l2::rest -> merge_rev rest accu [] l1 l2
  and mergeall = function              (* arg is forward *)
    | [] -> []
    | [l] -> l
    | llist -> mergepairs [] llist
  and mergeall_rev = function          (* arg is rev *)
    | [] -> []
    | [l] -> List.rev l
    | llist -> mergepairs_rev [] llist
  in
  mergeall_rev (init [] l)
;;

(************************************************************************)
(* merge sort on lists, user-contributed (NOT STABLE) *)

(* BEGIN code contributed by Yann Coscoy *)

  let rec rev_merge_append order l1 l2 acc =
    match l1 with
      [] ->  List.rev_append l2 acc
    | h1 :: t1 ->
        match l2 with
          [] -> List.rev_append l1 acc
        | h2 :: t2 ->
            if order h1 h2
            then  rev_merge_append order t1 l2 (h1::acc)
            else  rev_merge_append order l1 t2 (h2::acc)

  let rev_merge order l1 l2 = rev_merge_append order l1 l2 []

  let rec rev_merge_append' order l1 l2 acc =
    match l1 with
    | [] ->  List.rev_append l2 acc
    | h1 :: t1 ->
        match l2 with
          | [] -> List.rev_append l1 acc
          | h2 :: t2 ->
              if order h2 h1
              then  rev_merge_append' order t1 l2 (h1::acc)
              else  rev_merge_append' order l1 t2 (h2::acc)

  let rev_merge' order l1 l2 = rev_merge_append' order l1 l2 []

  let lmerge_3 order l =
    let rec initlist l acc = match l with
      | e1::e2::rest ->
          initlist rest
           ((if order e1 e2 then [e1;e2] else [e2;e1])::acc)
      | [e] -> [e]::acc
      | [] -> acc
    in
    let rec merge2 ll acc  = match ll with
      | [] -> acc
      | [l] -> [List.rev l]@acc
      | l1::l2::rest ->
          merge2 rest (rev_merge order l1 l2::acc)
    in
    let rec merge2' ll acc  = match ll with
      | [] -> acc
      | [l] -> [List.rev l]@acc
      | l1::l2::rest ->
          merge2' rest (rev_merge' order l1 l2::acc)
    in
    let rec mergeall rev = function
      | [] -> []
      | [l] -> if rev then List.rev l else l
      | llist ->
          mergeall
          (not rev) ((if rev then merge2' else merge2) llist [])
    in
    mergeall false (initlist l [])

(* END code contributed by Yann Coscoy *)

(************************************************************************)
(* merge sort on short lists, Francois Pottier *)

(* BEGIN code contributed by Francois Pottier *)

  (* [chop k l] returns the list [l] deprived of its [k] first
     elements. The length of the list [l] must be [k] at least. *)

  let rec chop k l =
    match k, l with
    | 0, _ -> l
    | _, x :: l -> chop (k-1) l
    | _, _ -> assert false
  ;;

  let rec merge order l1 l2 =
    match l1 with
      [] -> l2
    | h1 :: t1 ->
        match l2 with
          [] -> l1
        | h2 :: t2 ->
            if order h1 h2
            then h1 :: merge order t1 l2
            else h2 :: merge order l1 t2
  ;;

  let rec lmerge_4a order l =
    match l with
    | []
    | [ _ ] -> l
    | _ ->
        let rec sort k l = (* k > 1 *)
          match k, l with
          | 2, x1 :: x2 :: _ ->
              if order x1 x2 then [ x1; x2 ] else [ x2; x1 ]
          | 3, x1 :: x2 :: x3 :: _ ->
              if order x1 x2 then
                if order x2 x3 then
                  [ x1 ; x2 ; x3 ]
                else
                  if order x1 x3 then [ x1 ; x3 ; x2 ] else [ x3; x1; x2 ]
              else
                if order x1 x3 then
                  [ x2; x1; x3 ]
                else
                  if order x2 x3 then [ x2; x3; x1 ] else [ x3; x2; x1 ]
          | _, _ ->
              let k1 = k / 2 in
              let k2 = k - k1 in
              merge order (sort k1 l) (sort k2 (chop k1 l))
        in
        sort (List.length l) l
  ;;
(* END code contributed by Francois Pottier *)

(************************************************************************)
(* merge sort on short lists, Francois Pottier,
   adapted to new-style interface *)

(* BEGIN code contributed by Francois Pottier *)

  (* [chop k l] returns the list [l] deprived of its [k] first
     elements. The length of the list [l] must be [k] at least. *)

  let rec chop k l =
    match k, l with
    | 0, _ -> l
    | _, x :: l -> chop (k-1) l
    | _, _ -> assert false
  ;;

  let rec merge order l1 l2 =
    match l1 with
      [] -> l2
    | h1 :: t1 ->
        match l2 with
          [] -> l1
        | h2 :: t2 ->
            if order h1 h2 <= 0
            then h1 :: merge order t1 l2
            else h2 :: merge order l1 t2
  ;;

  let rec lmerge_4b order l =
    match l with
    | []
    | [ _ ] -> l
    | _ ->
        let rec sort k l = (* k > 1 *)
          match k, l with
          | 2, x1 :: x2 :: _ ->
              if order x1 x2 <= 0 then [ x1; x2 ] else [ x2; x1 ]
          | 3, x1 :: x2 :: x3 :: _ ->
              if order x1 x2 <= 0 then
                if order x2 x3 <= 0 then
                  [ x1 ; x2 ; x3 ]
                else
                  if order x1 x3 <= 0 then [ x1 ; x3 ; x2 ] else [ x3; x1; x2 ]
              else
                if order x1 x3 <= 0 then
                  [ x2; x1; x3 ]
                else
                  if order x2 x3 <= 0 then [ x2; x3; x1 ] else [ x3; x2; x1 ]
          | _, _ ->
              let k1 = k / 2 in
              let k2 = k - k1 in
              merge order (sort k1 l) (sort k2 (chop k1 l))
        in
        sort (List.length l) l
  ;;
(* END code contributed by Francois Pottier *)

(************************************************************************)
(* merge sort on short lists a la Pottier, modified merge *)

let rec chop k l =
  if k = 0 then l else begin
    match l with
    | x::t -> chop (k-1) t
    | _ -> assert false
  end
;;

let lmerge_4c cmp l =
  let rec merge1 h1 t1 l2 =
    match l2 with
    | [] -> h1 :: t1
    | h2 :: t2 ->
        if cmp h1 h2 <= 0
        then h1 :: (merge2 t1 h2 t2)
        else h2 :: (merge1 h1 t1 t2)
  and merge2 l1 h2 t2 =
    match l1 with
    | [] -> h2 :: t2
    | h1 :: t1 ->
        if cmp h1 h2 <= 0
        then h1 :: (merge2 t1 h2 t2)
        else h2 :: (merge1 h1 t1 t2)
  in
  let merge l1 = function
    | [] -> l1
    | h2 :: t2 -> merge2 l1 h2 t2
  in
  let rec sort n l =
    match n, l with
    | 2, x1 :: x2 :: _ ->
       if cmp x1 x2 <= 0 then [x1; x2] else [x2; x1]
    | 3, x1 :: x2 :: x3 :: _ ->
       if cmp x1 x2 <= 0 then begin
         if cmp x2 x3 <= 0 then [x1; x2; x3]
         else if cmp x1 x3 <= 0 then [x1; x3; x2]
         else [x3; x1; x2]
       end else begin
         if cmp x1 x3 <= 0 then [x2; x1; x3]
         else if cmp x2 x3 <= 0 then [x2; x3; x1]
         else [x3; x2; x1]
       end
    | n, l ->
       let n1 = n asr 1 in
       let n2 = n - n1 in
       merge (sort n1 l) (sort n2 (chop n1 l))
  in
  let len = List.length l in
  if len < 2 then l else sort len l
;;

(************************************************************************)
(* merge sort on short lists a la Pottier, logarithmic stack space *)

let rec chop k l =
  if k = 0 then l else begin
    match l with
    | x::t -> chop (k-1) t
    | _ -> assert false
  end
;;

let lmerge_4d cmp l =
  let rec rev_merge l1 l2 accu =
    match l1, l2 with
    | [], l2 -> l2 @@ accu
    | l1, [] -> l1 @@ accu
    | h1::t1, h2::t2 ->
        if cmp h1 h2 <= 0
        then rev_merge t1 l2 (h1::accu)
        else rev_merge l1 t2 (h2::accu)
  in
  let rec rev_merge_rev l1 l2 accu =
    match l1, l2 with
    | [], l2 -> l2 @@ accu
    | l1, [] -> l1 @@ accu
    | h1::t1, h2::t2 ->
        if cmp h1 h2 > 0
        then rev_merge_rev t1 l2 (h1::accu)
        else rev_merge_rev l1 t2 (h2::accu)
  in
  let rec sort n l =
    match n, l with
    | 2, x1 :: x2 :: _ ->
       if cmp x1 x2 <= 0 then [x1; x2] else [x2; x1]
    | 3, x1 :: x2 :: x3 :: _ ->
       if cmp x1 x2 <= 0 then begin
         if cmp x2 x3 <= 0 then [x1; x2; x3]
         else if cmp x1 x3 <= 0 then [x1; x3; x2]
         else [x3; x1; x2]
       end else begin
         if cmp x1 x3 <= 0 then [x2; x1; x3]
         else if cmp x2 x3 <= 0 then [x2; x3; x1]
         else [x3; x2; x1]
       end
    | n, l ->
       let n1 = n asr 1 in
       let n2 = n - n1 in
       rev_merge_rev (rev_sort n1 l) (rev_sort n2 (chop n1 l)) []
  and rev_sort n l =
    match n, l with
    | 2, x1 :: x2 :: _ ->
       if cmp x1 x2 > 0 then [x1; x2] else [x2; x1]
    | 3, x1 :: x2 :: x3 :: _ ->
       if cmp x1 x2 > 0 then begin
         if cmp x2 x3 > 0 then [x1; x2; x3]
         else if cmp x1 x3 > 0 then [x1; x3; x2]
         else [x3; x1; x2]
       end else begin
         if cmp x1 x3 > 0 then [x2; x1; x3]
         else if cmp x2 x3 > 0 then [x2; x3; x1]
         else [x3; x2; x1]
       end
    | n, l ->
       let n1 = n asr 1 in
       let n2 = n - n1 in
       rev_merge (sort n1 l) (sort n2 (chop n1 l)) []
  in
  let len = List.length l in
  if len < 2 then l else sort len l
;;


(************************************************************************)
(* merge sort on short lists a la Pottier, logarithmic stack space,
   in place: input list is freed as the output is being computed. *)

let rec chop k l =
  if k = 0 then l else begin
    match l with
    | x::t -> chop (k-1) t
    | _ -> assert false
  end
;;

let lmerge_4e cmp l =
  let rec rev_merge l1 l2 accu =
    match l1, l2 with
    | [], l2 -> l2 @@ accu
    | l1, [] -> l1 @@ accu
    | h1::t1, h2::t2 ->
        if cmp h1 h2 <= 0
        then rev_merge t1 l2 (h1::accu)
        else rev_merge l1 t2 (h2::accu)
  in
  let rec rev_merge_rev l1 l2 accu =
    match l1, l2 with
    | [], l2 -> l2 @@ accu
    | l1, [] -> l1 @@ accu
    | h1::t1, h2::t2 ->
        if cmp h1 h2 > 0
        then rev_merge_rev t1 l2 (h1::accu)
        else rev_merge_rev l1 t2 (h2::accu)
  in
  let rec sort n l =
    match n, l with
    | 2, x1 :: x2 :: _ ->
       if cmp x1 x2 <= 0 then [x1; x2] else [x2; x1]
    | 3, x1 :: x2 :: x3 :: _ ->
       if cmp x1 x2 <= 0 then begin
         if cmp x2 x3 <= 0 then [x1; x2; x3]
         else if cmp x1 x3 <= 0 then [x1; x3; x2]
         else [x3; x1; x2]
       end else begin
         if cmp x1 x3 <= 0 then [x2; x1; x3]
         else if cmp x2 x3 <= 0 then [x2; x3; x1]
         else [x3; x2; x1]
       end
    | n, l ->
       let n1 = n asr 1 in
       let n2 = n - n1 in
       let l2 = chop n1 l in
       let s1 = rev_sort n1 l in
       let s2 = rev_sort n2 l2 in
       rev_merge_rev s1 s2 []
  and rev_sort n l =
    match n, l with
    | 2, x1 :: x2 :: _ ->
       if cmp x1 x2 > 0 then [x1; x2] else [x2; x1]
    | 3, x1 :: x2 :: x3 :: _ ->
       if cmp x1 x2 > 0 then begin
         if cmp x2 x3 > 0 then [x1; x2; x3]
         else if cmp x1 x3 > 0 then [x1; x3; x2]
         else [x3; x1; x2]
       end else begin
         if cmp x1 x3 > 0 then [x2; x1; x3]
         else if cmp x2 x3 > 0 then [x2; x3; x1]
         else [x3; x2; x1]
       end
    | n, l ->
       let n1 = n asr 1 in
       let n2 = n - n1 in
       let l2 = chop n1 l in
       let s1 = sort n1 l in
       let s2 = sort n2 l2 in
       rev_merge s1 s2 []
  in
  let len = List.length l in
  if len < 2 then l else sort len l
;;

(************************************************************************)
(* chop-free version of Pottier's code, binary version *)

let rec merge cmp l1 l2 =
  match l1, l2 with
  | [], l2 -> l2
  | l1, [] -> l1
  | h1 :: t1, h2 :: t2 ->
      if cmp h1 h2 <= 0
      then h1 :: merge cmp t1 l2
      else h2 :: merge cmp l1 t2
;;

let lmerge_5a cmp l =
  let rem = ref l in
  let rec sort_prefix n =
    if n <= 1 then begin
      match !rem with
      | [] -> []
      | [x] as l -> rem := []; l
      | x::y::t -> rem := t; if cmp x y <= 0 then [x;y] else [y;x]
    end else if !rem = [] then []
    else begin
      let l1 = sort_prefix (n-1) in
      let l2 = sort_prefix (n-1) in
      merge cmp l1 l2
    end
  in
  let len = ref (List.length l) in
  let i = ref 0 in
  while !len > 0 do incr i; len := !len lsr 1; done;
  sort_prefix !i
;;

(************************************************************************)
(* chop-free version of Pottier's code, dichotomic version,
   ground cases 1 & 2 *)

let rec merge cmp l1 l2 =
  match l1, l2 with
  | [], l2 -> l2
  | l1, [] -> l1
  | h1 :: t1, h2 :: t2 ->
      if cmp h1 h2 <= 0
      then h1 :: merge cmp t1 l2
      else h2 :: merge cmp l1 t2
;;

let lmerge_5b cmp l =
  let rem = ref l in
  let rec sort_prefix n =
    match n, !rem with
    | 1, x::t -> rem := t; [x]
    | 2, x::y::t -> rem := t; if cmp x y <= 0 then [x;y] else [y;x]
    | n, _ ->
       let n1 = n/2 in
       let n2 = n - n1 in
       let l1 = sort_prefix n1 in
       let l2 = sort_prefix n2 in
       merge cmp l1 l2
  in
  let len = List.length l in
  if len <= 1 then l else sort_prefix len
;;

(************************************************************************)
(* chop-free version of Pottier's code, dichotomic version,
   ground cases 2 & 3 *)

let rec merge cmp l1 l2 =
  match l1, l2 with
  | [], l2 -> l2
  | l1, [] -> l1
  | h1 :: t1, h2 :: t2 ->
      if cmp h1 h2 <= 0
      then h1 :: merge cmp t1 l2
      else h2 :: merge cmp l1 t2
;;

let lmerge_5c cmp l =
  let rem = ref l in
  let rec sort_prefix n =
    match n, !rem with
    | 2, x::y::t -> rem := t; if cmp x y <= 0 then [x;y] else [y;x]
    | 3, x::y::z::t ->
       rem := t;
       if cmp x y <= 0 then
         if cmp y z <= 0 then [x; y; z]
         else if cmp x z <= 0 then [x; z; y]
         else [z; x; y]
       else
         if cmp x z <= 0 then [y; x; z]
         else if cmp y z <= 0 then [y; z; x]
         else [z; y; x]
    | n, _ ->
       let n1 = n/2 in
       let n2 = n - n1 in
       let l1 = sort_prefix n1 in
       let l2 = sort_prefix n2 in
       merge cmp l1 l2
  in
  let len = List.length l in
  if len <= 1 then l else sort_prefix len
;;

(************************************************************************)
(* chop-free, ref-free version of Pottier's code, dichotomic version,
   ground cases 2 & 3, modified merge *)

let lmerge_5d cmp l =
  let rec merge1 h1 t1 l2 =
    match l2 with
    | [] -> h1::t1
    | h2 :: t2 ->
        if cmp h1 h2 <= 0
        then h1 :: merge2 t1 h2 t2
        else h2 :: merge1 h1 t1 t2
  and merge2 l1 h2 t2 =
    match l1 with
    | [] -> h2::t2
    | h1 :: t1 ->
        if cmp h1 h2 <= 0
        then h1 :: merge2 t1 h2 t2
        else h2 :: merge1 h1 t1 t2
  in
  let rec sort_prefix n l =
    match n, l with
    | 2, x::y::t -> ((if cmp x y <= 0 then [x;y] else [y;x]), t)
    | 3, x::y::z::t ->
       ((if cmp x y <= 0 then
           if cmp y z <= 0 then [x; y; z]
           else if cmp x z <= 0 then [x; z; y]
           else [z; x; y]
         else
           if cmp x z <= 0 then [y; x; z]
           else if cmp y z <= 0 then [y; z; x]
           else [z; y; x]),
        t)
    | n, _ ->
       let n1 = n/2 in
       let n2 = n - n1 in
       let (l1, rest1) = sort_prefix n1 l in
       match sort_prefix n2 rest1 with
       | (h2::t2, rest2) -> ((merge2 l1 h2 t2), rest2)
       | _ -> assert false
  in
  let len = List.length l in
  if len <= 1 then l else fst (sort_prefix len l)
;;

(************************************************************************)
(* merge sort on arrays, merge with tail-rec function *)

let amerge_1a cmp a =
  let merge src1ofs src1len src2 src2ofs src2len dst dstofs =
    let src1r = src1ofs + src1len and src2r = src2ofs + src2len in
    let rec loop i1 s1 i2 s2 d =
      if cmp s1 s2 <= 0 then begin
        dst.(d) <- s1;
        let i1 = i1 + 1 in
        if i1 < src1r then
          loop i1 a.(i1) i2 s2 (d + 1)
        else
          Array.blit src2 i2 dst (d + 1) (src2r - i2)
      end else begin
        dst.(d) <- s2;
        let i2 = i2 + 1 in
        if i2 < src2r then
          loop i1 s1 i2 src2.(i2) (d + 1)
        else
          Array.blit a i1 dst (d + 1) (src1r - i1)
      end
    in loop src1ofs a.(src1ofs) src2ofs src2.(src2ofs) dstofs;
  in
  let rec sortto srcofs dst dstofs len =
    assert (len > 0);
    if len = 1 then dst.(dstofs) <- a.(srcofs)
    else begin
      let l1 = len / 2 in
      let l2 = len - l1 in
      sortto (srcofs + l1) dst (dstofs + l1) l2;
      sortto srcofs a (srcofs + l2) l1;
      merge (srcofs + l2) l1 dst (dstofs + l1) l2 dst dstofs;
    end;
  in
  let l = Array.length a in
  if l <= 1 then ()
  else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = Array.make l2 a.(0) in
    sortto l1 t 0 l2;
    sortto 0 a l2 l1;
    merge l2 l1 t 0 l2 a 0;
  end;
;;

let amerge_1b cmp a =
  let merge src1ofs src1len src2 src2ofs src2len dst dstofs =
    let src1r = src1ofs + src1len and src2r = src2ofs + src2len in
    let rec loop i1 s1 i2 s2 d =
      if cmp s1 s2 <= 0 then begin
        dst.(d) <- s1;
        let i1 = i1 + 1 in
        if i1 < src1r then
          loop i1 a.(i1) i2 s2 (d + 1)
        else
          Array.blit src2 i2 dst (d + 1) (src2r - i2)
      end else begin
        dst.(d) <- s2;
        let i2 = i2 + 1 in
        if i2 < src2r then
          loop i1 s1 i2 src2.(i2) (d + 1)
        else
          Array.blit a i1 dst (d + 1) (src1r - i1)
      end
    in loop src1ofs a.(src1ofs) src2ofs src2.(src2ofs) dstofs;
  in
  let rec sortto srcofs dst dstofs len =
    assert (len > 0);
    if len = 1 then dst.(dstofs) <- a.(srcofs)
    else if len = 2 then begin
      if cmp a.(srcofs) a.(srcofs+1) <= 0 then begin
        dst.(dstofs) <- a.(srcofs);
        dst.(dstofs+1) <- a.(srcofs+1);
      end else begin
        dst.(dstofs) <- a.(srcofs+1);
        dst.(dstofs+1) <- a.(srcofs);
      end;
    end else begin
      let l1 = len / 2 in
      let l2 = len - l1 in
      sortto (srcofs + l1) dst (dstofs + l1) l2;
      sortto srcofs a (srcofs + l2) l1;
      merge (srcofs + l2) l1 dst (dstofs + l1) l2 dst dstofs;
    end;
  in
  let l = Array.length a in
  if l <= 1 then ()
  else if l = 2 then begin
    if cmp a.(0) a.(1) > 0 then begin
      let e = a.(0) in
      a.(0) <- a.(1);
      a.(1) <- e;
    end;
  end else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = Array.make l2 a.(0) in
    sortto l1 t 0 l2;
    sortto 0 a l2 l1;
    merge l2 l1 t 0 l2 a 0;
  end;
;;

let cutoff = 3;;
let amerge_1c cmp a =
  let merge src1ofs src1len src2 src2ofs src2len dst dstofs =
    let src1r = src1ofs + src1len and src2r = src2ofs + src2len in
    let rec loop i1 s1 i2 s2 d =
      if cmp s1 s2 <= 0 then begin
        dst.(d) <- s1;
        let i1 = i1 + 1 in
        if i1 < src1r then
          loop i1 a.(i1) i2 s2 (d + 1)
        else
          Array.blit src2 i2 dst (d + 1) (src2r - i2)
      end else begin
        dst.(d) <- s2;
        let i2 = i2 + 1 in
        if i2 < src2r then
          loop i1 s1 i2 src2.(i2) (d + 1)
        else
          Array.blit a i1 dst (d + 1) (src1r - i1)
      end
    in loop src1ofs a.(src1ofs) src2ofs src2.(src2ofs) dstofs;
  in
  let isortto srcofs dst dstofs len =
    for i = 0 to len - 1 do
      let e = a.(srcofs + i) in
      let j = ref (dstofs + i - 1) in
      while (!j >= dstofs && cmp dst.(!j) e > 0) do
        dst.(!j + 1) <- dst.(!j);
        decr j;
      done;
      dst.(!j + 1) <- e;
    done;
  in
  let rec sortto srcofs dst dstofs len =
    if len <= cutoff then isortto srcofs dst dstofs len else begin
      let l1 = len / 2 in
      let l2 = len - l1 in
      sortto (srcofs + l1) dst (dstofs + l1) l2;
      sortto srcofs a (srcofs + l2) l1;
      merge (srcofs + l2) l1 dst (dstofs + l1) l2 dst dstofs;
    end;
  in
  let l = Array.length a in
  if l <= cutoff then isortto 0 a 0 l else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = Array.make l2 a.(0) in
    sortto l1 t 0 l2;
    sortto 0 a l2 l1;
    merge l2 l1 t 0 l2 a 0;
  end;
;;

let cutoff = 4;;
let amerge_1d cmp a =
  let merge src1ofs src1len src2 src2ofs src2len dst dstofs =
    let src1r = src1ofs + src1len and src2r = src2ofs + src2len in
    let rec loop i1 s1 i2 s2 d =
      if cmp s1 s2 <= 0 then begin
        dst.(d) <- s1;
        let i1 = i1 + 1 in
        if i1 < src1r then
          loop i1 a.(i1) i2 s2 (d + 1)
        else
          Array.blit src2 i2 dst (d + 1) (src2r - i2)
      end else begin
        dst.(d) <- s2;
        let i2 = i2 + 1 in
        if i2 < src2r then
          loop i1 s1 i2 src2.(i2) (d + 1)
        else
          Array.blit a i1 dst (d + 1) (src1r - i1)
      end
    in loop src1ofs a.(src1ofs) src2ofs src2.(src2ofs) dstofs;
  in
  let isortto srcofs dst dstofs len =
    for i = 0 to len - 1 do
      let e = a.(srcofs + i) in
      let j = ref (dstofs + i - 1) in
      while (!j >= dstofs && cmp dst.(!j) e > 0) do
        dst.(!j + 1) <- dst.(!j);
        decr j;
      done;
      dst.(!j + 1) <- e;
    done;
  in
  let rec sortto srcofs dst dstofs len =
    if len <= cutoff then isortto srcofs dst dstofs len else begin
      let l1 = len / 2 in
      let l2 = len - l1 in
      sortto (srcofs + l1) dst (dstofs + l1) l2;
      sortto srcofs a (srcofs + l2) l1;
      merge (srcofs + l2) l1 dst (dstofs + l1) l2 dst dstofs;
    end;
  in
  let l = Array.length a in
  if l <= cutoff then isortto 0 a 0 l else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = Array.make l2 a.(0) in
    sortto l1 t 0 l2;
    sortto 0 a l2 l1;
    merge l2 l1 t 0 l2 a 0;
  end;
;;

let cutoff = 5;;
let amerge_1e cmp a =
  let merge src1ofs src1len src2 src2ofs src2len dst dstofs =
    let src1r = src1ofs + src1len and src2r = src2ofs + src2len in
    let rec loop i1 s1 i2 s2 d =
      if cmp s1 s2 <= 0 then begin
        dst.(d) <- s1;
        let i1 = i1 + 1 in
        if i1 < src1r then
          loop i1 a.(i1) i2 s2 (d + 1)
        else
          Array.blit src2 i2 dst (d + 1) (src2r - i2)
      end else begin
        dst.(d) <- s2;
        let i2 = i2 + 1 in
        if i2 < src2r then
          loop i1 s1 i2 src2.(i2) (d + 1)
        else
          Array.blit a i1 dst (d + 1) (src1r - i1)
      end
    in loop src1ofs a.(src1ofs) src2ofs src2.(src2ofs) dstofs;
  in
  let isortto srcofs dst dstofs len =
    for i = 0 to len - 1 do
      let e = a.(srcofs + i) in
      let j = ref (dstofs + i - 1) in
      while (!j >= dstofs && cmp dst.(!j) e > 0) do
        dst.(!j + 1) <- dst.(!j);
        decr j;
      done;
      dst.(!j + 1) <- e;
    done;
  in
  let rec sortto srcofs dst dstofs len =
    if len <= cutoff then isortto srcofs dst dstofs len else begin
      let l1 = len / 2 in
      let l2 = len - l1 in
      sortto (srcofs + l1) dst (dstofs + l1) l2;
      sortto srcofs a (srcofs + l2) l1;
      merge (srcofs + l2) l1 dst (dstofs + l1) l2 dst dstofs;
    end;
  in
  let l = Array.length a in
  if l <= cutoff then isortto 0 a 0 l else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = Array.make l2 a.(0) in
    sortto l1 t 0 l2;
    sortto 0 a l2 l1;
    merge l2 l1 t 0 l2 a 0;
  end;
;;

let cutoff = 6;;
let amerge_1f cmp a =
  let merge src1ofs src1len src2 src2ofs src2len dst dstofs =
    let src1r = src1ofs + src1len and src2r = src2ofs + src2len in
    let rec loop i1 s1 i2 s2 d =
      if cmp s1 s2 <= 0 then begin
        dst.(d) <- s1;
        let i1 = i1 + 1 in
        if i1 < src1r then
          loop i1 a.(i1) i2 s2 (d + 1)
        else
          Array.blit src2 i2 dst (d + 1) (src2r - i2)
      end else begin
        dst.(d) <- s2;
        let i2 = i2 + 1 in
        if i2 < src2r then
          loop i1 s1 i2 src2.(i2) (d + 1)
        else
          Array.blit a i1 dst (d + 1) (src1r - i1)
      end
    in loop src1ofs a.(src1ofs) src2ofs src2.(src2ofs) dstofs;
  in
  let isortto srcofs dst dstofs len =
    for i = 0 to len - 1 do
      let e = a.(srcofs + i) in
      let j = ref (dstofs + i - 1) in
      while (!j >= dstofs && cmp dst.(!j) e > 0) do
        dst.(!j + 1) <- dst.(!j);
        decr j;
      done;
      dst.(!j + 1) <- e;
    done;
  in
  let rec sortto srcofs dst dstofs len =
    if len <= cutoff then isortto srcofs dst dstofs len else begin
      let l1 = len / 2 in
      let l2 = len - l1 in
      sortto (srcofs + l1) dst (dstofs + l1) l2;
      sortto srcofs a (srcofs + l2) l1;
      merge (srcofs + l2) l1 dst (dstofs + l1) l2 dst dstofs;
    end;
  in
  let l = Array.length a in
  if l <= cutoff then isortto 0 a 0 l else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = Array.make l2 a.(0) in
    sortto l1 t 0 l2;
    sortto 0 a l2 l1;
    merge l2 l1 t 0 l2 a 0;
  end;
;;

let cutoff = 7;;
let amerge_1g cmp a =
  let merge src1ofs src1len src2 src2ofs src2len dst dstofs =
    let src1r = src1ofs + src1len and src2r = src2ofs + src2len in
    let rec loop i1 s1 i2 s2 d =
      if cmp s1 s2 <= 0 then begin
        dst.(d) <- s1;
        let i1 = i1 + 1 in
        if i1 < src1r then
          loop i1 a.(i1) i2 s2 (d + 1)
        else
          Array.blit src2 i2 dst (d + 1) (src2r - i2)
      end else begin
        dst.(d) <- s2;
        let i2 = i2 + 1 in
        if i2 < src2r then
          loop i1 s1 i2 src2.(i2) (d + 1)
        else
          Array.blit a i1 dst (d + 1) (src1r - i1)
      end
    in loop src1ofs a.(src1ofs) src2ofs src2.(src2ofs) dstofs;
  in
  let isortto srcofs dst dstofs len =
    for i = 0 to len - 1 do
      let e = a.(srcofs + i) in
      let j = ref (dstofs + i - 1) in
      while (!j >= dstofs && cmp dst.(!j) e > 0) do
        dst.(!j + 1) <- dst.(!j);
        decr j;
      done;
      dst.(!j + 1) <- e;
    done;
  in
  let rec sortto srcofs dst dstofs len =
    if len <= cutoff then isortto srcofs dst dstofs len else begin
      let l1 = len / 2 in
      let l2 = len - l1 in
      sortto (srcofs + l1) dst (dstofs + l1) l2;
      sortto srcofs a (srcofs + l2) l1;
      merge (srcofs + l2) l1 dst (dstofs + l1) l2 dst dstofs;
    end;
  in
  let l = Array.length a in
  if l <= cutoff then isortto 0 a 0 l else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = Array.make l2 a.(0) in
    sortto l1 t 0 l2;
    sortto 0 a l2 l1;
    merge l2 l1 t 0 l2 a 0;
  end;
;;

let cutoff = 8;;
let amerge_1h cmp a =
  let merge src1ofs src1len src2 src2ofs src2len dst dstofs =
    let src1r = src1ofs + src1len and src2r = src2ofs + src2len in
    let rec loop i1 s1 i2 s2 d =
      if cmp s1 s2 <= 0 then begin
        dst.(d) <- s1;
        let i1 = i1 + 1 in
        if i1 < src1r then
          loop i1 a.(i1) i2 s2 (d + 1)
        else
          Array.blit src2 i2 dst (d + 1) (src2r - i2)
      end else begin
        dst.(d) <- s2;
        let i2 = i2 + 1 in
        if i2 < src2r then
          loop i1 s1 i2 src2.(i2) (d + 1)
        else
          Array.blit a i1 dst (d + 1) (src1r - i1)
      end
    in loop src1ofs a.(src1ofs) src2ofs src2.(src2ofs) dstofs;
  in
  let isortto srcofs dst dstofs len =
    for i = 0 to len - 1 do
      let e = a.(srcofs + i) in
      let j = ref (dstofs + i - 1) in
      while (!j >= dstofs && cmp dst.(!j) e > 0) do
        dst.(!j + 1) <- dst.(!j);
        decr j;
      done;
      dst.(!j + 1) <- e;
    done;
  in
  let rec sortto srcofs dst dstofs len =
    if len <= cutoff then isortto srcofs dst dstofs len else begin
      let l1 = len / 2 in
      let l2 = len - l1 in
      sortto (srcofs + l1) dst (dstofs + l1) l2;
      sortto srcofs a (srcofs + l2) l1;
      merge (srcofs + l2) l1 dst (dstofs + l1) l2 dst dstofs;
    end;
  in
  let l = Array.length a in
  if l <= cutoff then isortto 0 a 0 l else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = Array.make l2 a.(0) in
    sortto l1 t 0 l2;
    sortto 0 a l2 l1;
    merge l2 l1 t 0 l2 a 0;
  end;
;;

let cutoff = 9;;
let amerge_1i cmp a =
  let merge src1ofs src1len src2 src2ofs src2len dst dstofs =
    let src1r = src1ofs + src1len and src2r = src2ofs + src2len in
    let rec loop i1 s1 i2 s2 d =
      if cmp s1 s2 <= 0 then begin
        dst.(d) <- s1;
        let i1 = i1 + 1 in
        if i1 < src1r then
          loop i1 a.(i1) i2 s2 (d + 1)
        else
          Array.blit src2 i2 dst (d + 1) (src2r - i2)
      end else begin
        dst.(d) <- s2;
        let i2 = i2 + 1 in
        if i2 < src2r then
          loop i1 s1 i2 src2.(i2) (d + 1)
        else
          Array.blit a i1 dst (d + 1) (src1r - i1)
      end
    in loop src1ofs a.(src1ofs) src2ofs src2.(src2ofs) dstofs;
  in
  let isortto srcofs dst dstofs len =
    for i = 0 to len - 1 do
      let e = a.(srcofs + i) in
      let j = ref (dstofs + i - 1) in
      while (!j >= dstofs && cmp dst.(!j) e > 0) do
        dst.(!j + 1) <- dst.(!j);
        decr j;
      done;
      dst.(!j + 1) <- e;
    done;
  in
  let rec sortto srcofs dst dstofs len =
    if len <= cutoff then isortto srcofs dst dstofs len else begin
      let l1 = len / 2 in
      let l2 = len - l1 in
      sortto (srcofs + l1) dst (dstofs + l1) l2;
      sortto srcofs a (srcofs + l2) l1;
      merge (srcofs + l2) l1 dst (dstofs + l1) l2 dst dstofs;
    end;
  in
  let l = Array.length a in
  if l <= cutoff then isortto 0 a 0 l else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = Array.make l2 a.(0) in
    sortto l1 t 0 l2;
    sortto 0 a l2 l1;
    merge l2 l1 t 0 l2 a 0;
  end;
;;

let cutoff = 10;;
let amerge_1j cmp a =
  let merge src1ofs src1len src2 src2ofs src2len dst dstofs =
    let src1r = src1ofs + src1len and src2r = src2ofs + src2len in
    let rec loop i1 s1 i2 s2 d =
      if cmp s1 s2 <= 0 then begin
        dst.(d) <- s1;
        let i1 = i1 + 1 in
        if i1 < src1r then
          loop i1 a.(i1) i2 s2 (d + 1)
        else
          Array.blit src2 i2 dst (d + 1) (src2r - i2)
      end else begin
        dst.(d) <- s2;
        let i2 = i2 + 1 in
        if i2 < src2r then
          loop i1 s1 i2 src2.(i2) (d + 1)
        else
          Array.blit a i1 dst (d + 1) (src1r - i1)
      end
    in loop src1ofs a.(src1ofs) src2ofs src2.(src2ofs) dstofs;
  in
  let isortto srcofs dst dstofs len =
    for i = 0 to len - 1 do
      let e = a.(srcofs + i) in
      let j = ref (dstofs + i - 1) in
      while (!j >= dstofs && cmp dst.(!j) e > 0) do
        dst.(!j + 1) <- dst.(!j);
        decr j;
      done;
      dst.(!j + 1) <- e;
    done;
  in
  let rec sortto srcofs dst dstofs len =
    if len <= cutoff then isortto srcofs dst dstofs len else begin
      let l1 = len / 2 in
      let l2 = len - l1 in
      sortto (srcofs + l1) dst (dstofs + l1) l2;
      sortto srcofs a (srcofs + l2) l1;
      merge (srcofs + l2) l1 dst (dstofs + l1) l2 dst dstofs;
    end;
  in
  let l = Array.length a in
  if l <= cutoff then isortto 0 a 0 l else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = Array.make l2 a.(0) in
    sortto l1 t 0 l2;
    sortto 0 a l2 l1;
    merge l2 l1 t 0 l2 a 0;
  end;
;;

(* FIXME a essayer: *)
(* list->array->list direct et array->list->array direct *)
(* overhead = 1/3, 1/4, etc. *)
(* overhead = sqrt (n) *)
(* overhead = n/3 jusqu'a 30k, 30k jusqu'a 900M, sqrt (n) au-dela *)

(************************************************************************)
(* merge sort on arrays, merge with loop *)

(* cutoff = 1 *)
let amerge_3a cmp a =
  let merge src1ofs src1len src2 src2ofs src2len dst dstofs =
    let i1 = ref src1ofs
    and i2 = ref src2ofs
    and d = ref dstofs
    and src1r = src1ofs + src1len
    and src2r = src2ofs + src2len
    in
    while !i1 < src1r && !i2 < src2r do
      let s1 = a.(!i1) and s2 = src2.(!i2) in
      if cmp s1 s2 <= 0 then begin
        dst.(!d) <- s1;
        incr i1;
      end else begin
        dst.(!d) <- s2;
        incr i2;
      end;
      incr d;
    done;
    if !i1 < src1r then
      Array.blit a !i1 dst !d (src1r - !i1)
    else
      Array.blit src2 !i2 dst !d (src2r - !i2)
  in
  let rec sortto srcofs dst dstofs len =
    assert (len > 0);
    if len = 1 then dst.(dstofs) <- a.(srcofs) else
    let l1 = len / 2 in
    let l2 = len - l1 in
    sortto (srcofs+l1) dst (dstofs+l1) l2;
    sortto srcofs a (srcofs+l2) l1;
    merge (srcofs+l2) l1 dst (dstofs+l1) l2 dst dstofs;
  in
  let l = Array.length a in
  if l <= 1 then () else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = Array.make l2 a.(0) in
    sortto l1 t 0 l2;
    sortto 0 a l2 l1;
    merge l2 l1 t 0 l2 a 0;
  end;
;;

let amerge_3b cmp a =
  let merge src1ofs src1len src2 src2ofs src2len dst dstofs =
    let i1 = ref src1ofs
    and i2 = ref src2ofs
    and d = ref dstofs
    and src1r = src1ofs + src1len
    and src2r = src2ofs + src2len
    in
    while !i1 < src1r && !i2 < src2r do
      let s1 = a.(!i1) and s2 = src2.(!i2) in
      if cmp s1 s2 <= 0 then begin
        dst.(!d) <- s1;
        incr i1;
      end else begin
        dst.(!d) <- s2;
        incr i2;
      end;
      incr d;
    done;
    if !i1 < src1r then
      Array.blit a !i1 dst !d (src1r - !i1)
    else
      Array.blit src2 !i2 dst !d (src2r - !i2)
  in
  let rec sortto srcofs dst dstofs len =
    assert (len > 0);
    if len = 1 then dst.(dstofs) <- a.(srcofs)
    else if len = 2 then begin
      if cmp a.(srcofs) a.(srcofs+1) <= 0 then begin
        dst.(dstofs) <- a.(srcofs);
        dst.(dstofs+1) <- a.(srcofs+1);
      end else begin
        dst.(dstofs) <- a.(srcofs+1);
        dst.(dstofs+1) <- a.(srcofs);
      end
    end else begin
      let l1 = len / 2 in
      let l2 = len - l1 in
      sortto (srcofs+l1) dst (dstofs+l1) l2;
      sortto srcofs a (srcofs+l2) l1;
      merge (srcofs+l2) l1 dst (dstofs+l1) l2 dst dstofs;
    end
  in
  let l = Array.length a in
  if l <= 1 then ()
  else if l = 2 then begin
    if cmp a.(0) a.(1) > 0 then begin
      let e = a.(0) in
      a.(0) <- a.(1);
      a.(1) <- e;
    end;
  end else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = Array.make l2 a.(0) in
    sortto l1 t 0 l2;
    sortto 0 a l2 l1;
    merge l2 l1 t 0 l2 a 0;
  end;
;;

let cutoff = 3;;
let amerge_3c cmp a =
  let merge src1ofs src1len src2 src2ofs src2len dst dstofs =
    let i1 = ref src1ofs
    and i2 = ref src2ofs
    and d = ref dstofs
    and src1r = src1ofs + src1len
    and src2r = src2ofs + src2len
    in
    while !i1 < src1r && !i2 < src2r do
      let s1 = a.(!i1) and s2 = src2.(!i2) in
      if cmp s1 s2 <= 0 then begin
        dst.(!d) <- s1;
        incr i1;
      end else begin
        dst.(!d) <- s2;
        incr i2;
      end;
      incr d;
    done;
    if !i1 < src1r then
      Array.blit a !i1 dst !d (src1r - !i1)
    else
      Array.blit src2 !i2 dst !d (src2r - !i2)
  in
  let isortto srcofs dst dstofs len =
    for i = 0 to len-1 do
      let e = a.(srcofs+i) in
      let j = ref (dstofs+i-1) in
      while (!j >= dstofs && cmp dst.(!j) e > 0) do
        dst.(!j + 1) <- dst.(!j);
        decr j;
      done;
      dst.(!j + 1) <- e;
    done;
  in
  let rec sortto srcofs dst dstofs len =
    if len <= cutoff then isortto srcofs dst dstofs len else
    let l1 = len / 2 in
    let l2 = len - l1 in
    sortto (srcofs+l1) dst (dstofs+l1) l2;
    sortto srcofs a (srcofs+l2) l1;
    merge (srcofs+l2) l1 dst (dstofs+l1) l2 dst dstofs;
  in
  let l = Array.length a in
  if l <= cutoff then isortto 0 a 0 l else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = Array.make l2 a.(0) in
    sortto l1 t 0 l2;
    sortto 0 a l2 l1;
    merge l2 l1 t 0 l2 a 0;
  end;
;;

let cutoff = 4;;
let amerge_3d cmp a =
  let merge src1ofs src1len src2 src2ofs src2len dst dstofs =
    let i1 = ref src1ofs
    and i2 = ref src2ofs
    and d = ref dstofs
    and src1r = src1ofs + src1len
    and src2r = src2ofs + src2len
    in
    while !i1 < src1r && !i2 < src2r do
      let s1 = a.(!i1) and s2 = src2.(!i2) in
      if cmp s1 s2 <= 0 then begin
        dst.(!d) <- s1;
        incr i1;
      end else begin
        dst.(!d) <- s2;
        incr i2;
      end;
      incr d;
    done;
    if !i1 < src1r then
      Array.blit a !i1 dst !d (src1r - !i1)
    else
      Array.blit src2 !i2 dst !d (src2r - !i2)
  in
  let isortto srcofs dst dstofs len =
    for i = 0 to len-1 do
      let e = a.(srcofs+i) in
      let j = ref (dstofs+i-1) in
      while (!j >= dstofs && cmp dst.(!j) e > 0) do
        dst.(!j + 1) <- dst.(!j);
        decr j;
      done;
      dst.(!j + 1) <- e;
    done;
  in
  let rec sortto srcofs dst dstofs len =
    if len <= cutoff then isortto srcofs dst dstofs len else
    let l1 = len / 2 in
    let l2 = len - l1 in
    sortto (srcofs+l1) dst (dstofs+l1) l2;
    sortto srcofs a (srcofs+l2) l1;
    merge (srcofs+l2) l1 dst (dstofs+l1) l2 dst dstofs;
  in
  let l = Array.length a in
  if l <= cutoff then isortto 0 a 0 l else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = Array.make l2 a.(0) in
    sortto l1 t 0 l2;
    sortto 0 a l2 l1;
    merge l2 l1 t 0 l2 a 0;
  end;
;;

let cutoff = 5;;
let amerge_3e cmp a =
  let merge src1ofs src1len src2 src2ofs src2len dst dstofs =
    let i1 = ref src1ofs
    and i2 = ref src2ofs
    and d = ref dstofs
    and src1r = src1ofs + src1len
    and src2r = src2ofs + src2len
    in
    while !i1 < src1r && !i2 < src2r do
      let s1 = a.(!i1) and s2 = src2.(!i2) in
      if cmp s1 s2 <= 0 then begin
        dst.(!d) <- s1;
        incr i1;
      end else begin
        dst.(!d) <- s2;
        incr i2;
      end;
      incr d;
    done;
    if !i1 < src1r then
      Array.blit a !i1 dst !d (src1r - !i1)
    else
      Array.blit src2 !i2 dst !d (src2r - !i2)
  in
  let isortto srcofs dst dstofs len =
    for i = 0 to len-1 do
      let e = a.(srcofs+i) in
      let j = ref (dstofs+i-1) in
      while (!j >= dstofs && cmp dst.(!j) e > 0) do
        dst.(!j + 1) <- dst.(!j);
        decr j;
      done;
      dst.(!j + 1) <- e;
    done;
  in
  let rec sortto srcofs dst dstofs len =
    if len <= cutoff then isortto srcofs dst dstofs len else
    let l1 = len / 2 in
    let l2 = len - l1 in
    sortto (srcofs+l1) dst (dstofs+l1) l2;
    sortto srcofs a (srcofs+l2) l1;
    merge (srcofs+l2) l1 dst (dstofs+l1) l2 dst dstofs;
  in
  let l = Array.length a in
  if l <= cutoff then isortto 0 a 0 l else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = Array.make l2 a.(0) in
    sortto l1 t 0 l2;
    sortto 0 a l2 l1;
    merge l2 l1 t 0 l2 a 0;
  end;
;;

let cutoff = 6;;
let amerge_3f cmp a =
  let merge src1ofs src1len src2 src2ofs src2len dst dstofs =
    let i1 = ref src1ofs
    and i2 = ref src2ofs
    and d = ref dstofs
    and src1r = src1ofs + src1len
    and src2r = src2ofs + src2len
    in
    while !i1 < src1r && !i2 < src2r do
      let s1 = a.(!i1) and s2 = src2.(!i2) in
      if cmp s1 s2 <= 0 then begin
        dst.(!d) <- s1;
        incr i1;
      end else begin
        dst.(!d) <- s2;
        incr i2;
      end;
      incr d;
    done;
    if !i1 < src1r then
      Array.blit a !i1 dst !d (src1r - !i1)
    else
      Array.blit src2 !i2 dst !d (src2r - !i2)
  in
  let isortto srcofs dst dstofs len =
    for i = 0 to len-1 do
      let e = a.(srcofs+i) in
      let j = ref (dstofs+i-1) in
      while (!j >= dstofs && cmp dst.(!j) e > 0) do
        dst.(!j + 1) <- dst.(!j);
        decr j;
      done;
      dst.(!j + 1) <- e;
    done;
  in
  let rec sortto srcofs dst dstofs len =
    if len <= cutoff then isortto srcofs dst dstofs len else
    let l1 = len / 2 in
    let l2 = len - l1 in
    sortto (srcofs+l1) dst (dstofs+l1) l2;
    sortto srcofs a (srcofs+l2) l1;
    merge (srcofs+l2) l1 dst (dstofs+l1) l2 dst dstofs;
  in
  let l = Array.length a in
  if l <= cutoff then isortto 0 a 0 l else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = Array.make l2 a.(0) in
    sortto l1 t 0 l2;
    sortto 0 a l2 l1;
    merge l2 l1 t 0 l2 a 0;
  end;
;;

let cutoff = 7;;
let amerge_3g cmp a =
  let merge src1ofs src1len src2 src2ofs src2len dst dstofs =
    let i1 = ref src1ofs
    and i2 = ref src2ofs
    and d = ref dstofs
    and src1r = src1ofs + src1len
    and src2r = src2ofs + src2len
    in
    while !i1 < src1r && !i2 < src2r do
      let s1 = a.(!i1) and s2 = src2.(!i2) in
      if cmp s1 s2 <= 0 then begin
        dst.(!d) <- s1;
        incr i1;
      end else begin
        dst.(!d) <- s2;
        incr i2;
      end;
      incr d;
    done;
    if !i1 < src1r then
      Array.blit a !i1 dst !d (src1r - !i1)
    else
      Array.blit src2 !i2 dst !d (src2r - !i2)
  in
  let isortto srcofs dst dstofs len =
    for i = 0 to len-1 do
      let e = a.(srcofs+i) in
      let j = ref (dstofs+i-1) in
      while (!j >= dstofs && cmp dst.(!j) e > 0) do
        dst.(!j + 1) <- dst.(!j);
        decr j;
      done;
      dst.(!j + 1) <- e;
    done;
  in
  let rec sortto srcofs dst dstofs len =
    if len <= cutoff then isortto srcofs dst dstofs len else
    let l1 = len / 2 in
    let l2 = len - l1 in
    sortto (srcofs+l1) dst (dstofs+l1) l2;
    sortto srcofs a (srcofs+l2) l1;
    merge (srcofs+l2) l1 dst (dstofs+l1) l2 dst dstofs;
  in
  let l = Array.length a in
  if l <= cutoff then isortto 0 a 0 l else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = Array.make l2 a.(0) in
    sortto l1 t 0 l2;
    sortto 0 a l2 l1;
    merge l2 l1 t 0 l2 a 0;
  end;
;;

let cutoff = 8;;
let amerge_3h cmp a =
  let merge src1ofs src1len src2 src2ofs src2len dst dstofs =
    let i1 = ref src1ofs
    and i2 = ref src2ofs
    and d = ref dstofs
    and src1r = src1ofs + src1len
    and src2r = src2ofs + src2len
    in
    while !i1 < src1r && !i2 < src2r do
      let s1 = a.(!i1) and s2 = src2.(!i2) in
      if cmp s1 s2 <= 0 then begin
        dst.(!d) <- s1;
        incr i1;
      end else begin
        dst.(!d) <- s2;
        incr i2;
      end;
      incr d;
    done;
    if !i1 < src1r then
      Array.blit a !i1 dst !d (src1r - !i1)
    else
      Array.blit src2 !i2 dst !d (src2r - !i2)
  in
  let isortto srcofs dst dstofs len =
    for i = 0 to len-1 do
      let e = a.(srcofs+i) in
      let j = ref (dstofs+i-1) in
      while (!j >= dstofs && cmp dst.(!j) e > 0) do
        dst.(!j + 1) <- dst.(!j);
        decr j;
      done;
      dst.(!j + 1) <- e;
    done;
  in
  let rec sortto srcofs dst dstofs len =
    if len <= cutoff then isortto srcofs dst dstofs len else
    let l1 = len / 2 in
    let l2 = len - l1 in
    sortto (srcofs+l1) dst (dstofs+l1) l2;
    sortto srcofs a (srcofs+l2) l1;
    merge (srcofs+l2) l1 dst (dstofs+l1) l2 dst dstofs;
  in
  let l = Array.length a in
  if l <= cutoff then isortto 0 a 0 l else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = Array.make l2 a.(0) in
    sortto l1 t 0 l2;
    sortto 0 a l2 l1;
    merge l2 l1 t 0 l2 a 0;
  end;
;;

let cutoff = 9;;
let amerge_3i cmp a =
  let merge src1ofs src1len src2 src2ofs src2len dst dstofs =
    let i1 = ref src1ofs
    and i2 = ref src2ofs
    and d = ref dstofs
    and src1r = src1ofs + src1len
    and src2r = src2ofs + src2len
    in
    while !i1 < src1r && !i2 < src2r do
      let s1 = a.(!i1) and s2 = src2.(!i2) in
      if cmp s1 s2 <= 0 then begin
        dst.(!d) <- s1;
        incr i1;
      end else begin
        dst.(!d) <- s2;
        incr i2;
      end;
      incr d;
    done;
    if !i1 < src1r then
      Array.blit a !i1 dst !d (src1r - !i1)
    else
      Array.blit src2 !i2 dst !d (src2r - !i2)
  in
  let isortto srcofs dst dstofs len =
    for i = 0 to len-1 do
      let e = a.(srcofs+i) in
      let j = ref (dstofs+i-1) in
      while (!j >= dstofs && cmp dst.(!j) e > 0) do
        dst.(!j + 1) <- dst.(!j);
        decr j;
      done;
      dst.(!j + 1) <- e;
    done;
  in
  let rec sortto srcofs dst dstofs len =
    if len <= cutoff then isortto srcofs dst dstofs len else
    let l1 = len / 2 in
    let l2 = len - l1 in
    sortto (srcofs+l1) dst (dstofs+l1) l2;
    sortto srcofs a (srcofs+l2) l1;
    merge (srcofs+l2) l1 dst (dstofs+l1) l2 dst dstofs;
  in
  let l = Array.length a in
  if l <= cutoff then isortto 0 a 0 l else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = Array.make l2 a.(0) in
    sortto l1 t 0 l2;
    sortto 0 a l2 l1;
    merge l2 l1 t 0 l2 a 0;
  end;
;;

let cutoff = 10;;
let amerge_3j cmp a =
  let merge src1ofs src1len src2 src2ofs src2len dst dstofs =
    let i1 = ref src1ofs
    and i2 = ref src2ofs
    and d = ref dstofs
    and src1r = src1ofs + src1len
    and src2r = src2ofs + src2len
    in
    while !i1 < src1r && !i2 < src2r do
      let s1 = a.(!i1) and s2 = src2.(!i2) in
      if cmp s1 s2 <= 0 then begin
        dst.(!d) <- s1;
        incr i1;
      end else begin
        dst.(!d) <- s2;
        incr i2;
      end;
      incr d;
    done;
    if !i1 < src1r then
      Array.blit a !i1 dst !d (src1r - !i1)
    else
      Array.blit src2 !i2 dst !d (src2r - !i2)
  in
  let isortto srcofs dst dstofs len =
    for i = 0 to len-1 do
      let e = a.(srcofs+i) in
      let j = ref (dstofs+i-1) in
      while (!j >= dstofs && cmp dst.(!j) e > 0) do
        dst.(!j + 1) <- dst.(!j);
        decr j;
      done;
      dst.(!j + 1) <- e;
    done;
  in
  let rec sortto srcofs dst dstofs len =
    if len <= cutoff then isortto srcofs dst dstofs len else
    let l1 = len / 2 in
    let l2 = len - l1 in
    sortto (srcofs+l1) dst (dstofs+l1) l2;
    sortto srcofs a (srcofs+l2) l1;
    merge (srcofs+l2) l1 dst (dstofs+l1) l2 dst dstofs;
  in
  let l = Array.length a in
  if l <= cutoff then isortto 0 a 0 l else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = Array.make l2 a.(0) in
    sortto l1 t 0 l2;
    sortto 0 a l2 l1;
    merge l2 l1 t 0 l2 a 0;
  end;
;;

(* FIXME essayer bottom-up merge on arrays ? *)

(************************************************************************)
(* Shell sort on arrays *)

let ashell_1 cmp a =
  let l = Array.length a in
  let step = ref 1 in
  while !step < l do step := !step * 3 + 1; done;
  step := !step / 3;
  while !step > 0 do
    for j = !step to l-1 do
      let e = a.(j) in
      let k = ref (j - !step) in
      let k1 = ref j in
      while !k >= 0 && cmp a.(!k) e > 0 do
        a.(!k1) <- a.(!k);
        k1 := !k;
        k := !k - !step;
      done;
      a.(!k1) <- e;
    done;
    step := !step / 3;
  done;
;;

let ashell_2 cmp a =
  let l = Array.length a in
  let step = ref 1 in
  while !step < l do step := !step * 3 + 1; done;
  step := !step / 3;
  while !step > 0 do
    for j = !step to l-1 do
      let e = a.(j) in
      let k = ref (j - !step) in
      while !k >= 0 && cmp a.(!k) e > 0 do
        a.(!k + !step) <- a.(!k);
        k := !k - !step;
      done;
      a.(!k + !step) <- e;
    done;
    step := !step / 3;
  done;
;;

let ashell_3 cmp a =
  let l = Array.length a in
  let step = ref 1 in
  while !step < l do step := !step * 3 + 1; done;
  step := !step / 3;
  while !step > 0 do
    for i = 0 to !step - 1 do
      let j = ref (i + !step) in
      while !j < l do
        let e = ref a.(!j) in
        let k = ref (!j - !step) in
        if cmp !e a.(i) < 0 then begin
          let x = !e in e := a.(i); a.(i) <- x;
        end;
        while cmp a.(!k) !e > 0 do
          a.(!k + !step) <- a.(!k);
          k := !k - !step;
        done;
        a.(!k + !step) <- !e;
        j := !j + !step;
      done;
    done;
    step := !step / 3;
  done;
;;

let force = Lazy.force;;

type iilist = Cons of int * iilist Lazy.t;;

let rec mult n (Cons (x,l)) = Cons (n*x, lazy (mult n (force l)))

let rec merge (Cons (x1, t1) as l1) (Cons (x2, t2) as l2) =
  if x1 = x2 then Cons (x1, lazy (merge (force t1) (force t2)))
  else if x1 < x2 then Cons (x1, lazy (merge (force t1) l2))
  else Cons (x2, lazy (merge l1 (force t2)))
;;

let rec scale = Cons (1, lazy (merge (mult 2 scale) (mult 3 scale)));;

let ashell_4 cmp a =
  let l = Array.length a in
  let rec loop1 accu (Cons (x, t)) =
    if x > l then accu else loop1 (x::accu) (force t)
  in
  let sc = loop1 [] scale in
  let rec loop2 = function
    | [] -> ()
    | step::t ->
      for i = 0 to step - 1 do
        let j = ref (i + step) in
        while !j < l do
          let e = a.(!j) in
          let k = ref (!j - step) in
          while !k >= 0 && cmp a.(!k) e > 0 do
            a.(!k + step) <- a.(!k);
            k := !k - step;
          done;
          a.(!k + step) <- e;
          j := !j + step;
        done;
      done;
      loop2 t;
  in
  loop2 sc;
;;

(************************************************************************)
(* Quicksort on arrays *)
let cutoff = 1;;
let aquick_1a cmp a =
  let rec qsort l r =     (* ASSUMES r - l >= 2 *)
    let m = (l + r) / 2 in
    let al = a.(l) and am = a.(m) and ar = a.(r - 1) in
    let pivot = if cmp al am <= 0 then
                  if cmp am ar <= 0 then am
                  else if cmp al ar <= 0 then ar
                  else al
                else
                  if cmp al ar <= 0 then al
                  else if cmp am ar <= 0 then ar
                  else am
    in
    let p1 = ref l and p2 = ref l and p3 = ref (r - 1) in
    while !p2 <= !p3 do
      let e = a.(!p3) in
      let c = cmp e pivot in
      if c > 0 then begin
        decr p3;
      end else if c < 0 then begin
        a.(!p3) <- a.(!p2);
        a.(!p2) <- a.(!p1);
        a.(!p1) <- e;
        incr p1;
        incr p2;
      end else begin
        a.(!p3) <- a.(!p2);
        a.(!p2) <- e;
        incr p2;
      end;
    done;
    incr p3;
    let len1 = !p1 - l and len2 = r - !p3 in
    if len1 > cutoff then
      if len2 > cutoff then begin
        if len1 < len2
        then (qsort l !p1; qsort !p3 r)
        else (qsort !p3 r; qsort l !p1)
      end else qsort l !p1
    else if len2 > cutoff then qsort !p3 r;
  in
  let l = Array.length a in
  if l > 1 then begin
    qsort 0 l;
    let mini = ref 0 in
    for i = 1 to (min l cutoff) - 1 do
      if cmp a.(i) a.(!mini) < 0 then mini := i;
    done;
    let e = a.(0) in a.(0) <- a.(!mini); a.(!mini) <- e;
    for i = 1 to l - 1 do
      let e = a.(i) in
      let j = ref (i - 1) in
      while cmp a.(!j) e > 0 do
        a.(!j + 1) <- a.(!j);
        decr j;
      done;
      a.(!j + 1) <- e;
    done;
  end;
;;

let cutoff = 2;;
let aquick_1b cmp a =
  let rec qsort l r =     (* ASSUMES r - l >= 2 *)
    let m = (l + r) / 2 in
    let al = a.(l) and am = a.(m) and ar = a.(r - 1) in
    let pivot = if cmp al am <= 0 then
                  if cmp am ar <= 0 then am
                  else if cmp al ar <= 0 then ar
                  else al
                else
                  if cmp al ar <= 0 then al
                  else if cmp am ar <= 0 then ar
                  else am
    in
    let p1 = ref l and p2 = ref l and p3 = ref (r - 1) in
    while !p2 <= !p3 do
      let e = a.(!p3) in
      let c = cmp e pivot in
      if c > 0 then begin
        decr p3;
      end else if c < 0 then begin
        a.(!p3) <- a.(!p2);
        a.(!p2) <- a.(!p1);
        a.(!p1) <- e;
        incr p1;
        incr p2;
      end else begin
        a.(!p3) <- a.(!p2);
        a.(!p2) <- e;
        incr p2;
      end;
    done;
    incr p3;
    let len1 = !p1 - l and len2 = r - !p3 in
    if len1 > cutoff then
      if len2 > cutoff then begin
        if len1 < len2
        then (qsort l !p1; qsort !p3 r)
        else (qsort !p3 r; qsort l !p1)
      end else qsort l !p1
    else if len2 > cutoff then qsort !p3 r;
  in
  let l = Array.length a in
  if l > 1 then begin
    qsort 0 l;
    let mini = ref 0 in
    for i = 1 to (min l cutoff) - 1 do
      if cmp a.(i) a.(!mini) < 0 then mini := i;
    done;
    let e = a.(0) in a.(0) <- a.(!mini); a.(!mini) <- e;
    for i = 1 to l - 1 do
      let e = a.(i) in
      let j = ref (i - 1) in
      while cmp a.(!j) e > 0 do
        a.(!j + 1) <- a.(!j);
        decr j;
      done;
      a.(!j + 1) <- e;
    done;
  end;
;;

let cutoff = 3;;
let aquick_1c cmp a =
  let rec qsort l r =     (* ASSUMES r - l >= 2 *)
    let m = (l + r) / 2 in
    let al = a.(l) and am = a.(m) and ar = a.(r - 1) in
    let pivot = if cmp al am <= 0 then
                  if cmp am ar <= 0 then am
                  else if cmp al ar <= 0 then ar
                  else al
                else
                  if cmp al ar <= 0 then al
                  else if cmp am ar <= 0 then ar
                  else am
    in
    let p1 = ref l and p2 = ref l and p3 = ref (r - 1) in
    while !p2 <= !p3 do
      let e = a.(!p3) in
      let c = cmp e pivot in
      if c > 0 then begin
        decr p3;
      end else if c < 0 then begin
        a.(!p3) <- a.(!p2);
        a.(!p2) <- a.(!p1);
        a.(!p1) <- e;
        incr p1;
        incr p2;
      end else begin
        a.(!p3) <- a.(!p2);
        a.(!p2) <- e;
        incr p2;
      end;
    done;
    incr p3;
    let len1 = !p1 - l and len2 = r - !p3 in
    if len1 > cutoff then
      if len2 > cutoff then begin
        if len1 < len2
        then (qsort l !p1; qsort !p3 r)
        else (qsort !p3 r; qsort l !p1)
      end else qsort l !p1
    else if len2 > cutoff then qsort !p3 r;
  in
  let l = Array.length a in
  if l > 1 then begin
    qsort 0 l;
    let mini = ref 0 in
    for i = 1 to (min l cutoff) - 1 do
      if cmp a.(i) a.(!mini) < 0 then mini := i;
    done;
    let e = a.(0) in a.(0) <- a.(!mini); a.(!mini) <- e;
    for i = 1 to l - 1 do
      let e = a.(i) in
      let j = ref (i - 1) in
      while cmp a.(!j) e > 0 do
        a.(!j + 1) <- a.(!j);
        decr j;
      done;
      a.(!j + 1) <- e;
    done;
  end;
;;

let cutoff = 4;;
let aquick_1d cmp a =
  let rec qsort l r =     (* ASSUMES r - l >= 2 *)
    let m = (l + r) / 2 in
    let al = a.(l) and am = a.(m) and ar = a.(r - 1) in
    let pivot = if cmp al am <= 0 then
                  if cmp am ar <= 0 then am
                  else if cmp al ar <= 0 then ar
                  else al
                else
                  if cmp al ar <= 0 then al
                  else if cmp am ar <= 0 then ar
                  else am
    in
    let p1 = ref l and p2 = ref l and p3 = ref (r - 1) in
    while !p2 <= !p3 do
      let e = a.(!p3) in
      let c = cmp e pivot in
      if c > 0 then begin
        decr p3;
      end else if c < 0 then begin
        a.(!p3) <- a.(!p2);
        a.(!p2) <- a.(!p1);
        a.(!p1) <- e;
        incr p1;
        incr p2;
      end else begin
        a.(!p3) <- a.(!p2);
        a.(!p2) <- e;
        incr p2;
      end;
    done;
    incr p3;
    let len1 = !p1 - l and len2 = r - !p3 in
    if len1 > cutoff then
      if len2 > cutoff then begin
        if len1 < len2
        then (qsort l !p1; qsort !p3 r)
        else (qsort !p3 r; qsort l !p1)
      end else qsort l !p1
    else if len2 > cutoff then qsort !p3 r;
  in
  let l = Array.length a in
  if l > 1 then begin
    qsort 0 l;
    let mini = ref 0 in
    for i = 1 to (min l cutoff) - 1 do
      if cmp a.(i) a.(!mini) < 0 then mini := i;
    done;
    let e = a.(0) in a.(0) <- a.(!mini); a.(!mini) <- e;
    for i = 1 to l - 1 do
      let e = a.(i) in
      let j = ref (i - 1) in
      while cmp a.(!j) e > 0 do
        a.(!j + 1) <- a.(!j);
        decr j;
      done;
      a.(!j + 1) <- e;
    done;
  end;
;;

let cutoff = 5;;
let aquick_1e cmp a =
  let rec qsort l r =     (* ASSUMES r - l >= 2 *)
    let m = (l + r) / 2 in
    let al = a.(l) and am = a.(m) and ar = a.(r - 1) in
    let pivot = if cmp al am <= 0 then
                  if cmp am ar <= 0 then am
                  else if cmp al ar <= 0 then ar
                  else al
                else
                  if cmp al ar <= 0 then al
                  else if cmp am ar <= 0 then ar
                  else am
    in
    let p1 = ref l and p2 = ref l and p3 = ref (r - 1) in
    while !p2 <= !p3 do
      let e = a.(!p3) in
      let c = cmp e pivot in
      if c > 0 then begin
        decr p3;
      end else if c < 0 then begin
        a.(!p3) <- a.(!p2);
        a.(!p2) <- a.(!p1);
        a.(!p1) <- e;
        incr p1;
        incr p2;
      end else begin
        a.(!p3) <- a.(!p2);
        a.(!p2) <- e;
        incr p2;
      end;
    done;
    incr p3;
    let len1 = !p1 - l and len2 = r - !p3 in
    if len1 > cutoff then
      if len2 > cutoff then begin
        if len1 < len2
        then (qsort l !p1; qsort !p3 r)
        else (qsort !p3 r; qsort l !p1)
      end else qsort l !p1
    else if len2 > cutoff then qsort !p3 r;
  in
  let l = Array.length a in
  if l > 1 then begin
    qsort 0 l;
    let mini = ref 0 in
    for i = 1 to (min l cutoff) - 1 do
      if cmp a.(i) a.(!mini) < 0 then mini := i;
    done;
    let e = a.(0) in a.(0) <- a.(!mini); a.(!mini) <- e;
    for i = 1 to l - 1 do
      let e = a.(i) in
      let j = ref (i - 1) in
      while cmp a.(!j) e > 0 do
        a.(!j + 1) <- a.(!j);
        decr j;
      done;
      a.(!j + 1) <- e;
    done;
  end;
;;

let cutoff = 6;;
let aquick_1f cmp a =
  let rec qsort l r =     (* ASSUMES r - l >= 2 *)
    let m = (l + r) / 2 in
    let al = a.(l) and am = a.(m) and ar = a.(r - 1) in
    let pivot = if cmp al am <= 0 then
                  if cmp am ar <= 0 then am
                  else if cmp al ar <= 0 then ar
                  else al
                else
                  if cmp al ar <= 0 then al
                  else if cmp am ar <= 0 then ar
                  else am
    in
    let p1 = ref l and p2 = ref l and p3 = ref (r - 1) in
    while !p2 <= !p3 do
      let e = a.(!p3) in
      let c = cmp e pivot in
      if c > 0 then begin
        decr p3;
      end else if c < 0 then begin
        a.(!p3) <- a.(!p2);
        a.(!p2) <- a.(!p1);
        a.(!p1) <- e;
        incr p1;
        incr p2;
      end else begin
        a.(!p3) <- a.(!p2);
        a.(!p2) <- e;
        incr p2;
      end;
    done;
    incr p3;
    let len1 = !p1 - l and len2 = r - !p3 in
    if len1 > cutoff then
      if len2 > cutoff then begin
        if len1 < len2
        then (qsort l !p1; qsort !p3 r)
        else (qsort !p3 r; qsort l !p1)
      end else qsort l !p1
    else if len2 > cutoff then qsort !p3 r;
  in
  let l = Array.length a in
  if l > 1 then begin
    qsort 0 l;
    let mini = ref 0 in
    for i = 1 to (min l cutoff) - 1 do
      if cmp a.(i) a.(!mini) < 0 then mini := i;
    done;
    let e = a.(0) in a.(0) <- a.(!mini); a.(!mini) <- e;
    for i = 1 to l - 1 do
      let e = a.(i) in
      let j = ref (i - 1) in
      while cmp a.(!j) e > 0 do
        a.(!j + 1) <- a.(!j);
        decr j;
      done;
      a.(!j + 1) <- e;
    done;
  end;
;;

let cutoff = 7;;
let aquick_1g cmp a =
  let rec qsort l r =     (* ASSUMES r - l >= 2 *)
    let m = (l + r) / 2 in
    let al = a.(l) and am = a.(m) and ar = a.(r - 1) in
    let pivot = if cmp al am <= 0 then
                  if cmp am ar <= 0 then am
                  else if cmp al ar <= 0 then ar
                  else al
                else
                  if cmp al ar <= 0 then al
                  else if cmp am ar <= 0 then ar
                  else am
    in
    let p1 = ref l and p2 = ref l and p3 = ref (r - 1) in
    while !p2 <= !p3 do
      let e = a.(!p3) in
      let c = cmp e pivot in
      if c > 0 then begin
        decr p3;
      end else if c < 0 then begin
        a.(!p3) <- a.(!p2);
        a.(!p2) <- a.(!p1);
        a.(!p1) <- e;
        incr p1;
        incr p2;
      end else begin
        a.(!p3) <- a.(!p2);
        a.(!p2) <- e;
        incr p2;
      end;
    done;
    incr p3;
    let len1 = !p1 - l and len2 = r - !p3 in
    if len1 > cutoff then
      if len2 > cutoff then begin
        if len1 < len2
        then (qsort l !p1; qsort !p3 r)
        else (qsort !p3 r; qsort l !p1)
      end else qsort l !p1
    else if len2 > cutoff then qsort !p3 r;
  in
  let l = Array.length a in
  if l > 1 then begin
    qsort 0 l;
    let mini = ref 0 in
    for i = 1 to (min l cutoff) - 1 do
      if cmp a.(i) a.(!mini) < 0 then mini := i;
    done;
    let e = a.(0) in a.(0) <- a.(!mini); a.(!mini) <- e;
    for i = 1 to l - 1 do
      let e = a.(i) in
      let j = ref (i - 1) in
      while cmp a.(!j) e > 0 do
        a.(!j + 1) <- a.(!j);
        decr j;
      done;
      a.(!j + 1) <- e;
    done;
  end;
;;

let cutoff = 1;;
let aquick_2a cmp a =
  let rec qsort l r =     (* ASSUMES r - l >= 2 *)
    let m = (l + r) / 2 in
    let al = a.(l) and am = a.(m) and ar = a.(r - 1) in
    let pivot = if cmp al am <= 0 then
                  if cmp am ar <= 0 then am
                  else if cmp al ar <= 0 then ar
                  else al
                else
                  if cmp al ar <= 0 then al
                  else if cmp am ar <= 0 then ar
                  else am
    in
    let p1 = ref l and p2 = ref l and p3 = ref r in
    while !p2 < !p3 do
      let e = a.(!p2) in
      let c = cmp e pivot in
      if c > 0 then begin
        decr p3;
        a.(!p2) <- a.(!p3);
        a.(!p3) <- e;
      end else if c < 0 then begin
        a.(!p2) <- a.(!p1);
        a.(!p1) <- e;
        incr p1;
        incr p2;
      end else begin
        incr p2;
      end;
    done;
    let len1 = !p1 - l and len2 = r - !p3 in
    if len1 > cutoff then
      if len2 > cutoff then begin
        if len1 < len2
        then (qsort l !p1; qsort !p3 r)
        else (qsort !p3 r; qsort l !p1)
      end else qsort l !p1
    else if len2 > cutoff then qsort !p3 r;
  in
  let l = Array.length a in
  if l > 1 then begin
    qsort 0 l;
    let mini = ref 0 in
    for i = 0 to (min l cutoff) - 1 do
      if cmp a.(i) a.(!mini) < 0 then mini := i;
    done;
    let e = a.(0) in a.(0) <- a.(!mini); a.(!mini) <- e;
    for i = 1 to l - 1 do
      let e = a.(i) in
      let j = ref (i - 1) in
      while cmp a.(!j) e > 0 do
        a.(!j + 1) <- a.(!j);
        decr j;
      done;
      a.(!j + 1) <- e;
    done;
  end;
;;

let cutoff = 2;;
let aquick_2b cmp a =
  let rec qsort l r =     (* ASSUMES r - l >= 2 *)
    let m = (l + r) / 2 in
    let al = a.(l) and am = a.(m) and ar = a.(r - 1) in
    let pivot = if cmp al am <= 0 then
                  if cmp am ar <= 0 then am
                  else if cmp al ar <= 0 then ar
                  else al
                else
                  if cmp al ar <= 0 then al
                  else if cmp am ar <= 0 then ar
                  else am
    in
    let p1 = ref l and p2 = ref l and p3 = ref r in
    while !p2 < !p3 do
      let e = a.(!p2) in
      let c = cmp e pivot in
      if c > 0 then begin
        decr p3;
        a.(!p2) <- a.(!p3);
        a.(!p3) <- e;
      end else if c < 0 then begin
        a.(!p2) <- a.(!p1);
        a.(!p1) <- e;
        incr p1;
        incr p2;
      end else begin
        incr p2;
      end;
    done;
    let len1 = !p1 - l and len2 = r - !p3 in
    if len1 > cutoff then
      if len2 > cutoff then begin
        if len1 < len2
        then (qsort l !p1; qsort !p3 r)
        else (qsort !p3 r; qsort l !p1)
      end else qsort l !p1
    else if len2 > cutoff then qsort !p3 r;
  in
  let l = Array.length a in
  if l > 1 then begin
    qsort 0 l;
    let mini = ref 0 in
    for i = 0 to (min l cutoff) - 1 do
      if cmp a.(i) a.(!mini) < 0 then mini := i;
    done;
    let e = a.(0) in a.(0) <- a.(!mini); a.(!mini) <- e;
    for i = 1 to l - 1 do
      let e = a.(i) in
      let j = ref (i - 1) in
      while cmp a.(!j) e > 0 do
        a.(!j + 1) <- a.(!j);
        decr j;
      done;
      a.(!j + 1) <- e;
    done;
  end;
;;

let cutoff = 3;;
let aquick_2c cmp a =
  let rec qsort l r =     (* ASSUMES r - l >= 2 *)
    let m = (l + r) / 2 in
    let al = a.(l) and am = a.(m) and ar = a.(r - 1) in
    let pivot = if cmp al am <= 0 then
                  if cmp am ar <= 0 then am
                  else if cmp al ar <= 0 then ar
                  else al
                else
                  if cmp al ar <= 0 then al
                  else if cmp am ar <= 0 then ar
                  else am
    in
    let p1 = ref l and p2 = ref l and p3 = ref r in
    while !p2 < !p3 do
      let e = a.(!p2) in
      let c = cmp e pivot in
      if c > 0 then begin
        decr p3;
        a.(!p2) <- a.(!p3);
        a.(!p3) <- e;
      end else if c < 0 then begin
        a.(!p2) <- a.(!p1);
        a.(!p1) <- e;
        incr p1;
        incr p2;
      end else begin
        incr p2;
      end;
    done;
    let len1 = !p1 - l and len2 = r - !p3 in
    if len1 > cutoff then
      if len2 > cutoff then begin
        if len1 < len2
        then (qsort l !p1; qsort !p3 r)
        else (qsort !p3 r; qsort l !p1)
      end else qsort l !p1
    else if len2 > cutoff then qsort !p3 r;
  in
  let l = Array.length a in
  if l > 1 then begin
    qsort 0 l;
    let mini = ref 0 in
    for i = 0 to (min l cutoff) - 1 do
      if cmp a.(i) a.(!mini) < 0 then mini := i;
    done;
    let e = a.(0) in a.(0) <- a.(!mini); a.(!mini) <- e;
    for i = 1 to l - 1 do
      let e = a.(i) in
      let j = ref (i - 1) in
      while cmp a.(!j) e > 0 do
        a.(!j + 1) <- a.(!j);
        decr j;
      done;
      a.(!j + 1) <- e;
    done;
  end;
;;

let cutoff = 4;;
let aquick_2d cmp a =
  let rec qsort l r =     (* ASSUMES r - l >= 2 *)
    let m = (l + r) / 2 in
    let al = a.(l) and am = a.(m) and ar = a.(r - 1) in
    let pivot = if cmp al am <= 0 then
                  if cmp am ar <= 0 then am
                  else if cmp al ar <= 0 then ar
                  else al
                else
                  if cmp al ar <= 0 then al
                  else if cmp am ar <= 0 then ar
                  else am
    in
    let p1 = ref l and p2 = ref l and p3 = ref r in
    while !p2 < !p3 do
      let e = a.(!p2) in
      let c = cmp e pivot in
      if c > 0 then begin
        decr p3;
        a.(!p2) <- a.(!p3);
        a.(!p3) <- e;
      end else if c < 0 then begin
        a.(!p2) <- a.(!p1);
        a.(!p1) <- e;
        incr p1;
        incr p2;
      end else begin
        incr p2;
      end;
    done;
    let len1 = !p1 - l and len2 = r - !p3 in
    if len1 > cutoff then
      if len2 > cutoff then begin
        if len1 < len2
        then (qsort l !p1; qsort !p3 r)
        else (qsort !p3 r; qsort l !p1)
      end else qsort l !p1
    else if len2 > cutoff then qsort !p3 r;
  in
  let l = Array.length a in
  if l > 1 then begin
    qsort 0 l;
    let mini = ref 0 in
    for i = 0 to (min l cutoff) - 1 do
      if cmp a.(i) a.(!mini) < 0 then mini := i;
    done;
    let e = a.(0) in a.(0) <- a.(!mini); a.(!mini) <- e;
    for i = 1 to l - 1 do
      let e = a.(i) in
      let j = ref (i - 1) in
      while cmp a.(!j) e > 0 do
        a.(!j + 1) <- a.(!j);
        decr j;
      done;
      a.(!j + 1) <- e;
    done;
  end;
;;

let cutoff = 5;;
let aquick_2e cmp a =
  let rec qsort l r =     (* ASSUMES r - l >= 2 *)
    let m = (l + r) / 2 in
    let al = a.(l) and am = a.(m) and ar = a.(r - 1) in
    let pivot = if cmp al am <= 0 then
                  if cmp am ar <= 0 then am
                  else if cmp al ar <= 0 then ar
                  else al
                else
                  if cmp al ar <= 0 then al
                  else if cmp am ar <= 0 then ar
                  else am
    in
    let p1 = ref l and p2 = ref l and p3 = ref r in
    while !p2 < !p3 do
      let e = a.(!p2) in
      let c = cmp e pivot in
      if c > 0 then begin
        decr p3;
        a.(!p2) <- a.(!p3);
        a.(!p3) <- e;
      end else if c < 0 then begin
        a.(!p2) <- a.(!p1);
        a.(!p1) <- e;
        incr p1;
        incr p2;
      end else begin
        incr p2;
      end;
    done;
    let len1 = !p1 - l and len2 = r - !p3 in
    if len1 > cutoff then
      if len2 > cutoff then begin
        if len1 < len2
        then (qsort l !p1; qsort !p3 r)
        else (qsort !p3 r; qsort l !p1)
      end else qsort l !p1
    else if len2 > cutoff then qsort !p3 r;
  in
  let l = Array.length a in
  if l > 1 then begin
    qsort 0 l;
    let mini = ref 0 in
    for i = 0 to (min l cutoff) - 1 do
      if cmp a.(i) a.(!mini) < 0 then mini := i;
    done;
    let e = a.(0) in a.(0) <- a.(!mini); a.(!mini) <- e;
    for i = 1 to l - 1 do
      let e = a.(i) in
      let j = ref (i - 1) in
      while cmp a.(!j) e > 0 do
        a.(!j + 1) <- a.(!j);
        decr j;
      done;
      a.(!j + 1) <- e;
    done;
  end;
;;

let cutoff = 6;;
let aquick_2f cmp a =
  let rec qsort l r =     (* ASSUMES r - l >= 2 *)
    let m = (l + r) / 2 in
    let al = a.(l) and am = a.(m) and ar = a.(r - 1) in
    let pivot = if cmp al am <= 0 then
                  if cmp am ar <= 0 then am
                  else if cmp al ar <= 0 then ar
                  else al
                else
                  if cmp al ar <= 0 then al
                  else if cmp am ar <= 0 then ar
                  else am
    in
    let p1 = ref l and p2 = ref l and p3 = ref r in
    while !p2 < !p3 do
      let e = a.(!p2) in
      let c = cmp e pivot in
      if c > 0 then begin
        decr p3;
        a.(!p2) <- a.(!p3);
        a.(!p3) <- e;
      end else if c < 0 then begin
        a.(!p2) <- a.(!p1);
        a.(!p1) <- e;
        incr p1;
        incr p2;
      end else begin
        incr p2;
      end;
    done;
    let len1 = !p1 - l and len2 = r - !p3 in
    if len1 > cutoff then
      if len2 > cutoff then begin
        if len1 < len2
        then (qsort l !p1; qsort !p3 r)
        else (qsort !p3 r; qsort l !p1)
      end else qsort l !p1
    else if len2 > cutoff then qsort !p3 r;
  in
  let l = Array.length a in
  if l > 1 then begin
    qsort 0 l;
    let mini = ref 0 in
    for i = 0 to (min l cutoff) - 1 do
      if cmp a.(i) a.(!mini) < 0 then mini := i;
    done;
    let e = a.(0) in a.(0) <- a.(!mini); a.(!mini) <- e;
    for i = 1 to l - 1 do
      let e = a.(i) in
      let j = ref (i - 1) in
      while cmp a.(!j) e > 0 do
        a.(!j + 1) <- a.(!j);
        decr j;
      done;
      a.(!j + 1) <- e;
    done;
  end;
;;

let cutoff = 7;;
let aquick_2g cmp a =
  let rec qsort l r =     (* ASSUMES r - l >= 2 *)
    let m = (l + r) / 2 in
    let al = a.(l) and am = a.(m) and ar = a.(r - 1) in
    let pivot = if cmp al am <= 0 then
                  if cmp am ar <= 0 then am
                  else if cmp al ar <= 0 then ar
                  else al
                else
                  if cmp al ar <= 0 then al
                  else if cmp am ar <= 0 then ar
                  else am
    in
    let p1 = ref l and p2 = ref l and p3 = ref r in
    while !p2 < !p3 do
      let e = a.(!p2) in
      let c = cmp e pivot in
      if c > 0 then begin
        decr p3;
        a.(!p2) <- a.(!p3);
        a.(!p3) <- e;
      end else if c < 0 then begin
        a.(!p2) <- a.(!p1);
        a.(!p1) <- e;
        incr p1;
        incr p2;
      end else begin
        incr p2;
      end;
    done;
    let len1 = !p1 - l and len2 = r - !p3 in
    if len1 > cutoff then
      if len2 > cutoff then begin
        if len1 < len2
        then (qsort l !p1; qsort !p3 r)
        else (qsort !p3 r; qsort l !p1)
      end else qsort l !p1
    else if len2 > cutoff then qsort !p3 r;
  in
  let l = Array.length a in
  if l > 1 then begin
    qsort 0 l;
    let mini = ref 0 in
    for i = 0 to (min l cutoff) - 1 do
      if cmp a.(i) a.(!mini) < 0 then mini := i;
    done;
    let e = a.(0) in a.(0) <- a.(!mini); a.(!mini) <- e;
    for i = 1 to l - 1 do
      let e = a.(i) in
      let j = ref (i - 1) in
      while cmp a.(!j) e > 0 do
        a.(!j + 1) <- a.(!j);
        decr j;
      done;
      a.(!j + 1) <- e;
    done;
  end;
;;

let cutoff = 1;;
let aquick_3a cmp a =
  let rec qsort l r =     (* ASSUMES r - l >= 2 *)
    let m = (l + r) / 2 in
    let al = a.(l) and am = a.(m) and ar = a.(r - 1) in
    let pivot = if cmp al am <= 0 then
                  if cmp am ar <= 0 then am
                  else if cmp al ar <= 0 then ar
                  else al
                else
                  if cmp al ar <= 0 then al
                  else if cmp am ar <= 0 then ar
                  else am
    in
    let p1 = ref l and p2 = ref l and p3 = ref r in
    while !p2 < !p3 do
      let e = a.(!p2) in
      let c = cmp e pivot in
      if c > 0 then begin
        decr p3;
        a.(!p2) <- a.(!p3);
        a.(!p3) <- e;
      end else if c < 0 then begin
        incr p2;
      end else begin
        a.(!p2) <- a.(!p1);
        a.(!p1) <- e;
        incr p1;
        incr p2;
      end
    done;
    while !p1 > l do
      decr p1;
      decr p2;
      let e = a.(!p1) in a.(!p1) <- a.(!p2); a.(!p2) <- e;
    done;
    let len1 = !p2 - l and len2 = r - !p3 in
    if len1 > cutoff then
      if len2 > cutoff then begin
        if len1 < len2
        then (qsort l !p2; qsort !p3 r)
        else (qsort !p3 r; qsort l !p2)
      end else qsort l !p2
    else if len2 > cutoff then qsort !p3 r;
  in
  let l = Array.length a in
  if l > 1 then begin
    qsort 0 l;
    let mini = ref 0 in
    for i = 0 to (min l cutoff) - 1 do
      if cmp a.(i) a.(!mini) < 0 then mini := i;
    done;
    let e = a.(0) in a.(0) <- a.(!mini); a.(!mini) <- e;
    for i = 1 to l - 1 do
      let e = a.(i) in
      let j = ref (i - 1) in
      while cmp a.(!j) e > 0 do
        a.(!j + 1) <- a.(!j);
        decr j;
      done;
      a.(!j + 1) <- e;
    done;
  end;
;;

let cutoff = 2;;
let aquick_3b cmp a =
  let rec qsort l r =     (* ASSUMES r - l >= 2 *)
    let m = (l + r) / 2 in
    let al = a.(l) and am = a.(m) and ar = a.(r - 1) in
    let pivot = if cmp al am <= 0 then
                  if cmp am ar <= 0 then am
                  else if cmp al ar <= 0 then ar
                  else al
                else
                  if cmp al ar <= 0 then al
                  else if cmp am ar <= 0 then ar
                  else am
    in
    let p1 = ref l and p2 = ref l and p3 = ref r in
    while !p2 < !p3 do
      let e = a.(!p2) in
      let c = cmp e pivot in
      if c > 0 then begin
        decr p3;
        a.(!p2) <- a.(!p3);
        a.(!p3) <- e;
      end else if c < 0 then begin
        incr p2;
      end else begin
        a.(!p2) <- a.(!p1);
        a.(!p1) <- e;
        incr p1;
        incr p2;
      end
    done;
    while !p1 > l do
      decr p1;
      decr p2;
      let e = a.(!p1) in a.(!p1) <- a.(!p2); a.(!p2) <- e;
    done;
    let len1 = !p2 - l and len2 = r - !p3 in
    if len1 > cutoff then
      if len2 > cutoff then begin
        if len1 < len2
        then (qsort l !p2; qsort !p3 r)
        else (qsort !p3 r; qsort l !p2)
      end else qsort l !p2
    else if len2 > cutoff then qsort !p3 r;
  in
  let l = Array.length a in
  if l > 1 then begin
    qsort 0 l;
    let mini = ref 0 in
    for i = 0 to (min l cutoff) - 1 do
      if cmp a.(i) a.(!mini) < 0 then mini := i;
    done;
    let e = a.(0) in a.(0) <- a.(!mini); a.(!mini) <- e;
    for i = 1 to l - 1 do
      let e = a.(i) in
      let j = ref (i - 1) in
      while cmp a.(!j) e > 0 do
        a.(!j + 1) <- a.(!j);
        decr j;
      done;
      a.(!j + 1) <- e;
    done;
  end;
;;

let cutoff = 3;;
let aquick_3c cmp a =
  let rec qsort l r =     (* ASSUMES r - l >= 2 *)
    let m = (l + r) / 2 in
    let al = a.(l) and am = a.(m) and ar = a.(r - 1) in
    let pivot = if cmp al am <= 0 then
                  if cmp am ar <= 0 then am
                  else if cmp al ar <= 0 then ar
                  else al
                else
                  if cmp al ar <= 0 then al
                  else if cmp am ar <= 0 then ar
                  else am
    in
    let p1 = ref l and p2 = ref l and p3 = ref r in
    while !p2 < !p3 do
      let e = a.(!p2) in
      let c = cmp e pivot in
      if c > 0 then begin
        decr p3;
        a.(!p2) <- a.(!p3);
        a.(!p3) <- e;
      end else if c < 0 then begin
        incr p2;
      end else begin
        a.(!p2) <- a.(!p1);
        a.(!p1) <- e;
        incr p1;
        incr p2;
      end
    done;
    while !p1 > l do
      decr p1;
      decr p2;
      let e = a.(!p1) in a.(!p1) <- a.(!p2); a.(!p2) <- e;
    done;
    let len1 = !p2 - l and len2 = r - !p3 in
    if len1 > cutoff then
      if len2 > cutoff then begin
        if len1 < len2
        then (qsort l !p2; qsort !p3 r)
        else (qsort !p3 r; qsort l !p2)
      end else qsort l !p2
    else if len2 > cutoff then qsort !p3 r;
  in
  let l = Array.length a in
  if l > 1 then begin
    qsort 0 l;
    let mini = ref 0 in
    for i = 0 to (min l cutoff) - 1 do
      if cmp a.(i) a.(!mini) < 0 then mini := i;
    done;
    let e = a.(0) in a.(0) <- a.(!mini); a.(!mini) <- e;
    for i = 1 to l - 1 do
      let e = a.(i) in
      let j = ref (i - 1) in
      while cmp a.(!j) e > 0 do
        a.(!j + 1) <- a.(!j);
        decr j;
      done;
      a.(!j + 1) <- e;
    done;
  end;
;;

let cutoff = 4;;
let aquick_3d cmp a =
  let rec qsort l r =     (* ASSUMES r - l >= 2 *)
    let m = (l + r) / 2 in
    let al = a.(l) and am = a.(m) and ar = a.(r - 1) in
    let pivot = if cmp al am <= 0 then
                  if cmp am ar <= 0 then am
                  else if cmp al ar <= 0 then ar
                  else al
                else
                  if cmp al ar <= 0 then al
                  else if cmp am ar <= 0 then ar
                  else am
    in
    let p1 = ref l and p2 = ref l and p3 = ref r in
    while !p2 < !p3 do
      let e = a.(!p2) in
      let c = cmp e pivot in
      if c > 0 then begin
        decr p3;
        a.(!p2) <- a.(!p3);
        a.(!p3) <- e;
      end else if c < 0 then begin
        incr p2;
      end else begin
        a.(!p2) <- a.(!p1);
        a.(!p1) <- e;
        incr p1;
        incr p2;
      end
    done;
    while !p1 > l do
      decr p1;
      decr p2;
      let e = a.(!p1) in a.(!p1) <- a.(!p2); a.(!p2) <- e;
    done;
    let len1 = !p2 - l and len2 = r - !p3 in
    if len1 > cutoff then
      if len2 > cutoff then begin
        if len1 < len2
        then (qsort l !p2; qsort !p3 r)
        else (qsort !p3 r; qsort l !p2)
      end else qsort l !p2
    else if len2 > cutoff then qsort !p3 r;
  in
  let l = Array.length a in
  if l > 1 then begin
    qsort 0 l;
    let mini = ref 0 in
    for i = 0 to (min l cutoff) - 1 do
      if cmp a.(i) a.(!mini) < 0 then mini := i;
    done;
    let e = a.(0) in a.(0) <- a.(!mini); a.(!mini) <- e;
    for i = 1 to l - 1 do
      let e = a.(i) in
      let j = ref (i - 1) in
      while cmp a.(!j) e > 0 do
        a.(!j + 1) <- a.(!j);
        decr j;
      done;
      a.(!j + 1) <- e;
    done;
  end;
;;

let cutoff = 5;;
let aquick_3e cmp a =
  let rec qsort l r =     (* ASSUMES r - l >= 2 *)
    let m = (l + r) / 2 in
    let al = a.(l) and am = a.(m) and ar = a.(r - 1) in
    let pivot = if cmp al am <= 0 then
                  if cmp am ar <= 0 then am
                  else if cmp al ar <= 0 then ar
                  else al
                else
                  if cmp al ar <= 0 then al
                  else if cmp am ar <= 0 then ar
                  else am
    in
    let p1 = ref l and p2 = ref l and p3 = ref r in
    while !p2 < !p3 do
      let e = a.(!p2) in
      let c = cmp e pivot in
      if c > 0 then begin
        decr p3;
        a.(!p2) <- a.(!p3);
        a.(!p3) <- e;
      end else if c < 0 then begin
        incr p2;
      end else begin
        a.(!p2) <- a.(!p1);
        a.(!p1) <- e;
        incr p1;
        incr p2;
      end
    done;
    while !p1 > l do
      decr p1;
      decr p2;
      let e = a.(!p1) in a.(!p1) <- a.(!p2); a.(!p2) <- e;
    done;
    let len1 = !p2 - l and len2 = r - !p3 in
    if len1 > cutoff then
      if len2 > cutoff then begin
        if len1 < len2
        then (qsort l !p2; qsort !p3 r)
        else (qsort !p3 r; qsort l !p2)
      end else qsort l !p2
    else if len2 > cutoff then qsort !p3 r;
  in
  let l = Array.length a in
  if l > 1 then begin
    qsort 0 l;
    let mini = ref 0 in
    for i = 0 to (min l cutoff) - 1 do
      if cmp a.(i) a.(!mini) < 0 then mini := i;
    done;
    let e = a.(0) in a.(0) <- a.(!mini); a.(!mini) <- e;
    for i = 1 to l - 1 do
      let e = a.(i) in
      let j = ref (i - 1) in
      while cmp a.(!j) e > 0 do
        a.(!j + 1) <- a.(!j);
        decr j;
      done;
      a.(!j + 1) <- e;
    done;
  end;
;;

let cutoff = 6;;
let aquick_3f cmp a =
  let rec qsort l r =     (* ASSUMES r - l >= 2 *)
    let m = (l + r) / 2 in
    let al = a.(l) and am = a.(m) and ar = a.(r - 1) in
    let pivot = if cmp al am <= 0 then
                  if cmp am ar <= 0 then am
                  else if cmp al ar <= 0 then ar
                  else al
                else
                  if cmp al ar <= 0 then al
                  else if cmp am ar <= 0 then ar
                  else am
    in
    let p1 = ref l and p2 = ref l and p3 = ref r in
    while !p2 < !p3 do
      let e = a.(!p2) in
      let c = cmp e pivot in
      if c > 0 then begin
        decr p3;
        a.(!p2) <- a.(!p3);
        a.(!p3) <- e;
      end else if c < 0 then begin
        incr p2;
      end else begin
        a.(!p2) <- a.(!p1);
        a.(!p1) <- e;
        incr p1;
        incr p2;
      end
    done;
    while !p1 > l do
      decr p1;
      decr p2;
      let e = a.(!p1) in a.(!p1) <- a.(!p2); a.(!p2) <- e;
    done;
    let len1 = !p2 - l and len2 = r - !p3 in
    if len1 > cutoff then
      if len2 > cutoff then begin
        if len1 < len2
        then (qsort l !p2; qsort !p3 r)
        else (qsort !p3 r; qsort l !p2)
      end else qsort l !p2
    else if len2 > cutoff then qsort !p3 r;
  in
  let l = Array.length a in
  if l > 1 then begin
    qsort 0 l;
    let mini = ref 0 in
    for i = 0 to (min l cutoff) - 1 do
      if cmp a.(i) a.(!mini) < 0 then mini := i;
    done;
    let e = a.(0) in a.(0) <- a.(!mini); a.(!mini) <- e;
    for i = 1 to l - 1 do
      let e = a.(i) in
      let j = ref (i - 1) in
      while cmp a.(!j) e > 0 do
        a.(!j + 1) <- a.(!j);
        decr j;
      done;
      a.(!j + 1) <- e;
    done;
  end;
;;

let cutoff = 7;;
let aquick_3g cmp a =
  let rec qsort l r =     (* ASSUMES r - l >= 2 *)
    let m = (l + r) / 2 in
    let al = a.(l) and am = a.(m) and ar = a.(r - 1) in
    let pivot = if cmp al am <= 0 then
                  if cmp am ar <= 0 then am
                  else if cmp al ar <= 0 then ar
                  else al
                else
                  if cmp al ar <= 0 then al
                  else if cmp am ar <= 0 then ar
                  else am
    in
    let p1 = ref l and p2 = ref l and p3 = ref r in
    while !p2 < !p3 do
      let e = a.(!p2) in
      let c = cmp e pivot in
      if c > 0 then begin
        decr p3;
        a.(!p2) <- a.(!p3);
        a.(!p3) <- e;
      end else if c < 0 then begin
        incr p2;
      end else begin
        a.(!p2) <- a.(!p1);
        a.(!p1) <- e;
        incr p1;
        incr p2;
      end
    done;
    while !p1 > l do
      decr p1;
      decr p2;
      let e = a.(!p1) in a.(!p1) <- a.(!p2); a.(!p2) <- e;
    done;
    let len1 = !p2 - l and len2 = r - !p3 in
    if len1 > cutoff then
      if len2 > cutoff then begin
        if len1 < len2
        then (qsort l !p2; qsort !p3 r)
        else (qsort !p3 r; qsort l !p2)
      end else qsort l !p2
    else if len2 > cutoff then qsort !p3 r;
  in
  let l = Array.length a in
  if l > 1 then begin
    qsort 0 l;
    let mini = ref 0 in
    for i = 0 to (min l cutoff) - 1 do
      if cmp a.(i) a.(!mini) < 0 then mini := i;
    done;
    let e = a.(0) in a.(0) <- a.(!mini); a.(!mini) <- e;
    for i = 1 to l - 1 do
      let e = a.(i) in
      let j = ref (i - 1) in
      while cmp a.(!j) e > 0 do
        a.(!j + 1) <- a.(!j);
        decr j;
      done;
      a.(!j + 1) <- e;
    done;
  end;
;;

let cutoff = 8;;
let aquick_3h cmp a =
  let rec qsort l r =     (* ASSUMES r - l >= 2 *)
    let m = (l + r) / 2 in
    let al = a.(l) and am = a.(m) and ar = a.(r - 1) in
    let pivot = if cmp al am <= 0 then
                  if cmp am ar <= 0 then am
                  else if cmp al ar <= 0 then ar
                  else al
                else
                  if cmp al ar <= 0 then al
                  else if cmp am ar <= 0 then ar
                  else am
    in
    let p1 = ref l and p2 = ref l and p3 = ref r in
    while !p2 < !p3 do
      let e = a.(!p2) in
      let c = cmp e pivot in
      if c > 0 then begin
        decr p3;
        a.(!p2) <- a.(!p3);
        a.(!p3) <- e;
      end else if c < 0 then begin
        incr p2;
      end else begin
        a.(!p2) <- a.(!p1);
        a.(!p1) <- e;
        incr p1;
        incr p2;
      end
    done;
    while !p1 > l do
      decr p1;
      decr p2;
      let e = a.(!p1) in a.(!p1) <- a.(!p2); a.(!p2) <- e;
    done;
    let len1 = !p2 - l and len2 = r - !p3 in
    if len1 > cutoff then
      if len2 > cutoff then begin
        if len1 < len2
        then (qsort l !p2; qsort !p3 r)
        else (qsort !p3 r; qsort l !p2)
      end else qsort l !p2
    else if len2 > cutoff then qsort !p3 r;
  in
  let l = Array.length a in
  if l > 1 then begin
    qsort 0 l;
    let mini = ref 0 in
    for i = 0 to (min l cutoff) - 1 do
      if cmp a.(i) a.(!mini) < 0 then mini := i;
    done;
    let e = a.(0) in a.(0) <- a.(!mini); a.(!mini) <- e;
    for i = 1 to l - 1 do
      let e = a.(i) in
      let j = ref (i - 1) in
      while cmp a.(!j) e > 0 do
        a.(!j + 1) <- a.(!j);
        decr j;
      done;
      a.(!j + 1) <- e;
    done;
  end;
;;

let cutoff = 9;;
let aquick_3i cmp a =
  let rec qsort l r =     (* ASSUMES r - l >= 2 *)
    let m = (l + r) / 2 in
    let al = a.(l) and am = a.(m) and ar = a.(r - 1) in
    let pivot = if cmp al am <= 0 then
                  if cmp am ar <= 0 then am
                  else if cmp al ar <= 0 then ar
                  else al
                else
                  if cmp al ar <= 0 then al
                  else if cmp am ar <= 0 then ar
                  else am
    in
    let p1 = ref l and p2 = ref l and p3 = ref r in
    while !p2 < !p3 do
      let e = a.(!p2) in
      let c = cmp e pivot in
      if c > 0 then begin
        decr p3;
        a.(!p2) <- a.(!p3);
        a.(!p3) <- e;
      end else if c < 0 then begin
        incr p2;
      end else begin
        a.(!p2) <- a.(!p1);
        a.(!p1) <- e;
        incr p1;
        incr p2;
      end
    done;
    while !p1 > l do
      decr p1;
      decr p2;
      let e = a.(!p1) in a.(!p1) <- a.(!p2); a.(!p2) <- e;
    done;
    let len1 = !p2 - l and len2 = r - !p3 in
    if len1 > cutoff then
      if len2 > cutoff then begin
        if len1 < len2
        then (qsort l !p2; qsort !p3 r)
        else (qsort !p3 r; qsort l !p2)
      end else qsort l !p2
    else if len2 > cutoff then qsort !p3 r;
  in
  let l = Array.length a in
  if l > 1 then begin
    qsort 0 l;
    let mini = ref 0 in
    for i = 0 to (min l cutoff) - 1 do
      if cmp a.(i) a.(!mini) < 0 then mini := i;
    done;
    let e = a.(0) in a.(0) <- a.(!mini); a.(!mini) <- e;
    for i = 1 to l - 1 do
      let e = a.(i) in
      let j = ref (i - 1) in
      while cmp a.(!j) e > 0 do
        a.(!j + 1) <- a.(!j);
        decr j;
      done;
      a.(!j + 1) <- e;
    done;
  end;
;;

let cutoff = 10;;
let aquick_3j cmp a =
  let rec qsort l r =     (* ASSUMES r - l >= 2 *)
    let m = (l + r) / 2 in
    let al = a.(l) and am = a.(m) and ar = a.(r - 1) in
    let pivot = if cmp al am <= 0 then
                  if cmp am ar <= 0 then am
                  else if cmp al ar <= 0 then ar
                  else al
                else
                  if cmp al ar <= 0 then al
                  else if cmp am ar <= 0 then ar
                  else am
    in
    let p1 = ref l and p2 = ref l and p3 = ref r in
    while !p2 < !p3 do
      let e = a.(!p2) in
      let c = cmp e pivot in
      if c > 0 then begin
        decr p3;
        a.(!p2) <- a.(!p3);
        a.(!p3) <- e;
      end else if c < 0 then begin
        incr p2;
      end else begin
        a.(!p2) <- a.(!p1);
        a.(!p1) <- e;
        incr p1;
        incr p2;
      end
    done;
    while !p1 > l do
      decr p1;
      decr p2;
      let e = a.(!p1) in a.(!p1) <- a.(!p2); a.(!p2) <- e;
    done;
    let len1 = !p2 - l and len2 = r - !p3 in
    if len1 > cutoff then
      if len2 > cutoff then begin
        if len1 < len2
        then (qsort l !p2; qsort !p3 r)
        else (qsort !p3 r; qsort l !p2)
      end else qsort l !p2
    else if len2 > cutoff then qsort !p3 r;
  in
  let l = Array.length a in
  if l > 1 then begin
    qsort 0 l;
    let mini = ref 0 in
    for i = 0 to (min l cutoff) - 1 do
      if cmp a.(i) a.(!mini) < 0 then mini := i;
    done;
    let e = a.(0) in a.(0) <- a.(!mini); a.(!mini) <- e;
    for i = 1 to l - 1 do
      let e = a.(i) in
      let j = ref (i - 1) in
      while cmp a.(!j) e > 0 do
        a.(!j + 1) <- a.(!j);
        decr j;
      done;
      a.(!j + 1) <- e;
    done;
  end;
;;

(************************************************************************)
(* Heap sort on arrays (top-down, ternary) *)

let aheap_1 cmp a =
  let l = ref (Array.length a) in
  let l3 = ref ((!l + 1) / 3) in   (* l3 is the first element without sons *)
  let maxson i =                   (* ASSUMES i < !l3 *)
    let i31 = i+i+i+1 in
    let x = ref i31 in
    if i31+2 < !l then begin
      if cmp a.(i31) a.(i31+1) < 0 then x := i31+1;
      if cmp a.(!x) a.(i31+2) < 0 then x := i31+2;
      !x
    end else begin
      if i31+1 < !l && cmp a.(i31) a.(i31+1) < 0
      then i31+1
      else i31
    end
  in
  let rec trickledown i e =    (* ASSUMES i < !l3 *)
    let j = maxson i in
    if cmp a.(j) e > 0 then begin
      a.(i) <- a.(j);
      if j < !l3 then trickledown j e else a.(j) <- e;
    end else begin
      a.(i) <- e;
    end;
  in
  for i = !l3 - 1 downto 0 do trickledown i a.(i); done;
  let m = ref (!l + 1 - 3 * !l3) in
  while !l > 2 do
    decr l;
    if !m = 0 then (m := 2; decr l3) else decr m;
    let e = a.(!l) in
    a.(!l) <- a.(0);
    trickledown 0 e;
  done;
  if !l > 1 then begin let e = a.(1) in a.(1) <- a.(0); a.(0) <- e; end;
;;

(************************************************************************)
(* Heap sort on arrays (top-down, binary) *)

(* FIXME essayer application partielle de trickledown (merge avec down) *)
(* FIXME essayer expanser maxson dans trickledown; supprimer l'exception. *)

let aheap_2 cmp a =
  let maxson l i e =
    let i21 = i + i + 1 in
    if i21 + 1 < l && cmp a.(i21) a.(i21+1) < 0
    then i21 + 1
    else if i21 < l then i21 else (a.(i) <- e; raise Exit)
  in
  let rec trickledown l i e =
    let j = maxson l i e in
    if cmp a.(j) e > 0 then begin
      a.(i) <- a.(j);
      trickledown l j e;
    end else begin
      a.(i) <- e;
    end;
  in
  let down l i e = try trickledown l i e with Exit -> () in
  let l = Array.length a in
  for i = l / 2 -1 downto 0 do down l i a.(i); done;
  for i = l - 1 downto 1 do
    let e = a.(i) in
    a.(i) <- a.(0);
    down i 0 e;
  done;
;;

(************************************************************************)
(* Heap sort on arrays (bottom-up, ternary) *)

exception Bottom of int;;

let aheap_3 cmp a =
  let maxson l i =
    let i31 = i+i+i+1 in
    let x = ref i31 in
    if i31+2 < l then begin
      if cmp a.(i31) a.(i31+1) < 0 then x := i31+1;
      if cmp a.(!x) a.(i31+2) < 0 then x := i31+2;
      !x
    end else
      if i31+1 < l && cmp a.(i31) a.(i31+1) < 0
      then i31+1
      else if i31 < l then i31 else raise (Bottom i)
  in
  let rec trickledown l i e =
    let j = maxson l i in
    if cmp a.(j) e > 0 then begin
      a.(i) <- a.(j);
      trickledown l j e;
    end else begin
      a.(i) <- e;
    end;
  in
  let rec trickle l i e = try trickledown l i e with Bottom i -> a.(i) <- e in
  let rec bubbledown l i =
    let j = maxson l i in
    a.(i) <- a.(j);
    bubbledown l j;
  in
  let bubble l i = try bubbledown l i with Bottom i -> i in
  let rec trickleup i e =
    let father = (i - 1) / 3 in
    assert (i <> father);
    if cmp a.(father) e < 0 then begin
      a.(i) <- a.(father);
      if father > 0 then trickleup father e else a.(0) <- e;
    end else begin
      a.(i) <- e;
    end;
  in
  let l = Array.length a in
  for i = (l + 1) / 3 - 1 downto 0 do trickle l i a.(i); done;
  for i = l - 1 downto 2 do
    let e = a.(i) in
    a.(i) <- a.(0);
    trickleup (bubble i 0) e;
  done;
  if l > 1 then (let e = a.(1) in a.(1) <- a.(0); a.(0) <- e);
;;

(************************************************************************)
(* Heap sort on arrays (bottom-up, binary) *)

let aheap_4 cmp a =
  let maxson l i =
    let i21 = i + i + 1 in
    if i21 + 1 < l && cmp a.(i21) a.(i21 + 1) < 0
    then i21 + 1
    else if i21 < l then i21 else raise (Bottom i)
  in
  let rec trickledown l i e =
    let j = maxson l i in
    if cmp a.(j) e > 0 then begin
      a.(i) <- a.(j);
      trickledown l j e;
    end else begin
      a.(i) <- e;
    end;
  in
  let trickle l i e = try trickledown l i e with Bottom i -> a.(i) <- e in
  let rec bubbledown l i =
    let j = maxson l i in
    a.(i) <- a.(j);
    bubbledown l j;
  in
  let bubble l i = try bubbledown l i with Bottom i -> i in
  let rec trickleup i e =
    let father = (i - 1) / 2 in
    assert (i <> father);
    if cmp a.(father) e < 0 then begin
      a.(i) <- a.(father);
      if father > 0 then trickleup father e else a.(0) <- e;
    end else begin
      a.(i) <- e;
    end;
  in
  let l = Array.length a in
  for i = l / 2 - 1 downto 0 do trickle l i a.(i); done;
  for i = l - 1 downto 2 do
    let e = a.(i) in
    a.(i) <- a.(0);
    trickleup (bubble i 0) e;
  done;
  if l > 1 then (let e = a.(1) in a.(1) <- a.(0); a.(0) <- e);
;;

(************************************************************************)
(* heap sort, top-down, ternary, recursive final loop *)

let aheap_5 cmp a =
  let maxson l i =                 (* ASSUMES i < (l+1)/3 *)
    let i31 = i+i+i+1 in
    let x = ref i31 in
    if i31+2 < l then begin
      if cmp a.(i31) a.(i31+1) < 0 then x := i31+1;
      if cmp a.(!x) a.(i31+2) < 0 then x := i31+2;
      !x
    end else begin
      if i31+1 < l && cmp a.(i31) a.(i31+1) < 0
      then i31+1
      else i31
    end
  in
  let rec trickledown l l3 i e =    (* ASSUMES i < l3 *)
    let j = maxson l i in
    if cmp a.(j) e > 0 then begin
      a.(i) <- a.(j);
      if j < l3 then trickledown l l3 j e else a.(j) <- e;
    end else begin
      a.(i) <- e;
    end;
  in
  let l = Array.length a in
  let l3 = (l + 1) / 3 in
  for i = l3 - 1 downto 0 do trickledown l l3 i a.(i); done;
  let rec loop0 l l3 =
    let e = a.(l) in
    a.(l) <- a.(0);
    trickledown l l3 0 e;
    loop2 (l-1) (l3-1);
  and loop1 l l3 =
    let e = a.(l) in
    a.(l) <- a.(0);
    trickledown l l3 0 e;
    loop0 (l-1) l3;
  and loop2 l l3 =
    if l > 1 then begin
      let e = a.(l) in
      a.(l) <- a.(0);
      trickledown l l3 0 e;
      loop1 (l-1) l3;
    end else begin
      let e = a.(1) in a.(1) <- a.(0); a.(0) <- e;
    end;
  in
  if l > 1 then
    match l + 1 - 3 * l3 with
    | 0 -> loop2 (l-1) (l3-1);
    | 1 -> loop0 (l-1) l3;
    | 2 -> loop1 (l-1) l3;
    | _ -> assert false;
;;

(************************************************************************)
(* heap sort, top-down, ternary, with exception *)

let aheap_6 cmp a =
  let maxson e l i =
    let i31 = i + i + i + 1 in
    let x = ref i31 in
    if i31+2 < l then begin
      if cmp a.(i31) a.(i31+1) < 0 then x := i31+1;
      if cmp a.(!x) a.(i31+2) < 0 then x := i31+2;
      !x
    end else begin
      if i31+1 < l && cmp a.(i31) a.(i31+1) < 0
      then i31+1
      else if i31 < l then i31 else (a.(i) <- e; raise Exit)
    end
  in
  let rec trickledown e l i =
    let j = maxson e l i in
    if cmp a.(j) e > 0 then begin
      a.(i) <- a.(j);
      trickledown e l j;
    end else begin
      a.(i) <- e;
    end;
  in
  let down e l i = try trickledown e l i with Exit -> (); in
  let l = Array.length a in
  for i = (l + 1) / 3 - 1 downto 0 do down a.(i) l i; done;
  for i = l - 1 downto 2 do
    let e = a.(i) in
    a.(i) <- a.(0);
    down e i 0;
  done;
  if l > 1 then (let e = a.(1) in a.(1) <- a.(0); a.(0) <- e);
;;

(* FIXME essayer cutoff pour heapsort *)

(************************************************************************)
(* Insertion sort with dichotomic search *)

let ainsertion_1 cmp a =
  let rec dicho l r e =
    if l = r then l else begin
      let m = (l + r) / 2 in
      if cmp a.(m) e <= 0
      then dicho (m+1) r e
      else dicho l m e
    end
  in
  for i = 1 to Array.length a - 1 do
    let e = a.(i) in
    let j = dicho 0 i e in
    Array.blit a j a (j + 1) (i - j);
    a.(j) <- e;
  done;
;;

(************************************************************************)
(* merge sort on lists via arrays *)

let array_to_list_in_place a =
  let l = Array.length a in
  let rec loop accu n p =
    if p <= 0 then accu else begin
      if p = n then begin
        Obj.truncate (Obj.repr a) p;
        loop (a.(p-1) :: accu) (n-1000) (p-1)
      end else begin
        loop (a.(p-1) :: accu) n (p-1)
      end
    end
  in
  loop [] l l
;;

let array_of_list l len =
  match l with
  | [] -> [| |]
  | h::t ->
      let a = Array.make len h in
      let rec loop i l =
        match l with
        | [] -> ()
        | h::t -> a.(i) <- h; loop (i+1) t
      in
      loop 1 t;
      a
;;

let lmerge_0a cmp l =
  let a = Array.of_list l in
  amerge_1e cmp a;
  array_to_list_in_place a
;;

let lmerge_0b cmp l =
  let len = List.length l in
  if len > 256 then Gc.minor ();
  let a = array_of_list l len in
  amerge_1e cmp a;
  array_to_list_in_place a
;;

let lshell_0 cmp l =
  let a = Array.of_list l in
  ashell_2 cmp a;
  array_to_list_in_place a
;;

let lquick_0 cmp l =
  let a = Array.of_list l in
  aquick_3f cmp a;
  array_to_list_in_place a
;;

(************************************************************************)
(* merge sort on arrays via lists *)

let amerge_0 cmp a =    (* cutoff is not yet used *)
  let l = lmerge_4e cmp (Array.to_list a) in
  let rec loop i = function
  | [] -> ()
  | h::t -> a.(i) <- h; loop (i + 1) t
  in
  loop 0 l
;;

(************************************************************************)

let lold = [
  "Sort.list", Sort.list, true;
  "lmerge_3", lmerge_3, false;
  "lmerge_4a", lmerge_4a, true;
];;

let lnew = [
  "List.stable_sort", List.stable_sort, true;

  "lmerge_0a", lmerge_0a, true;
  "lmerge_0b", lmerge_0b, true;
  "lshell_0", lshell_0, false;
  "lquick_0", lquick_0, false;

  "lmerge_1a", lmerge_1a, true;
  "lmerge_1b", lmerge_1b, true;
  "lmerge_1c", lmerge_1c, true;
  "lmerge_1d", lmerge_1d, true;

  "lmerge_4b", lmerge_4b, true;
  "lmerge_4c", lmerge_4c, true;
  "lmerge_4d", lmerge_4d, true;
  "lmerge_4e", lmerge_4e, true;

  "lmerge_5a", lmerge_5a, true;
  "lmerge_5b", lmerge_5b, true;
  "lmerge_5c", lmerge_5c, true;
  "lmerge_5d", lmerge_5d, true;
];;
let anew = [
  "Array.stable_sort", Array.stable_sort, true;
  "Array.sort", Array.sort, false;

  "amerge_0", amerge_0, true;

  "amerge_1a", amerge_1a, true;
  "amerge_1b", amerge_1b, true;
  "amerge_1c", amerge_1c, true;
  "amerge_1d", amerge_1d, true;
  "amerge_1e", amerge_1e, true;
  "amerge_1f", amerge_1f, true;
  "amerge_1g", amerge_1g, true;
  "amerge_1h", amerge_1h, true;
  "amerge_1i", amerge_1i, true;
  "amerge_1j", amerge_1j, true;

  "amerge_3a", amerge_3a, true;
  "amerge_3b", amerge_3b, true;
  "amerge_3c", amerge_3c, true;
  "amerge_3d", amerge_3d, true;
  "amerge_3e", amerge_3e, true;
  "amerge_3f", amerge_3f, true;
  "amerge_3g", amerge_3g, true;
  "amerge_3h", amerge_3h, true;
  "amerge_3i", amerge_3i, true;
  "amerge_3j", amerge_3j, true;

  "ashell_1", ashell_1, false;
  "ashell_2", ashell_2, false;
  "ashell_3", ashell_3, false;
  "ashell_4", ashell_4, false;

  "aquick_1a", aquick_1a, false;
  "aquick_1b", aquick_1b, false;
  "aquick_1c", aquick_1c, false;
  "aquick_1d", aquick_1d, false;
  "aquick_1e", aquick_1e, false;
  "aquick_1f", aquick_1f, false;
  "aquick_1g", aquick_1g, false;

  "aquick_2a", aquick_2a, false;
  "aquick_2b", aquick_2b, false;
  "aquick_2c", aquick_2c, false;
  "aquick_2d", aquick_2d, false;
  "aquick_2e", aquick_2e, false;
  "aquick_2f", aquick_2f, false;
  "aquick_2g", aquick_2g, false;

  "aquick_3a", aquick_3a, false;
  "aquick_3b", aquick_3b, false;
  "aquick_3c", aquick_3c, false;
  "aquick_3d", aquick_3d, false;
  "aquick_3e", aquick_3e, false;
  "aquick_3f", aquick_3f, false;
  "aquick_3g", aquick_3g, false;
  "aquick_3h", aquick_3h, false;
  "aquick_3i", aquick_3i, false;
  "aquick_3j", aquick_3j, false;

  "aheap_1", aheap_1, false;
  "aheap_2", aheap_2, false;
  "aheap_3", aheap_3, false;
  "aheap_4", aheap_4, false;
  "aheap_5", aheap_5, false;
  "aheap_6", aheap_6, false;

  "ainsertion_1", ainsertion_1, true;
];;

(************************************************************************)
(* main program *)

type mode = Test_std | Test | Bench1 | Bench2 | Bench3;;

let size = ref 22
and mem = ref 0
and mode = ref Test_std
and only = ref []
;;

let usage = "Usage: sorts [-size <table size>] [-mem <memory size>]\n\
          \032            [-seed <random seed>] [-test|-bench]"
;;

let options = [
  "-size", Arg.Int ((:=) size), "   Maximum size for benchmarks (default 22)";
  "-meg",Arg.Int ((:=) mem),"    How many megabytes to preallocate (default 0)";
  "-seed", Arg.Int ((:=) seed), "   PRNG seed (default 0)";
  "-teststd", Arg.Unit (fun () -> mode := Test_std), "   Test stdlib (default)";
  "-test", Arg.Unit (fun () -> mode := Test), "   Select test mode";
  "-bench1", Arg.Unit (fun () -> mode := Bench1), "  Select bench mode 1";
  "-bench2", Arg.Unit (fun () -> mode := Bench2), "  Select bench mode 2";
  "-bench3", Arg.Unit (fun () -> mode := Bench3), "  Select bench mode 3";
  "-fn", Arg.String (fun x -> only := x :: !only),
                         " <function>  Test/Bench this function (default all)";
];;
let anonymous x = raise (Arg.Bad ("unrecognised option "^x));;

let main () =
  Arg.parse options anonymous usage;

  Printf.printf "Command line arguments are:";
  for i = 1 to Array.length Sys.argv - 1 do
    Printf.printf " %s" Sys.argv.(i);
  done;
  Printf.printf "\n";

  ignore (String.create (1048576 * !mem));
  Gc.full_major ();
(*
  let a2l = Array.to_list in
  let l2ak x y = Array.of_list x in
  let id = fun x -> x in
  let fst x y = x in
  let snd x y = y in
*)
  let benchonly f x y z t =
    match !only with
    | [] -> f x y z t
    | l -> if List.mem y l then f x y z t
  in
  let testonly x1 x2 x3 x4 x5 x6 =
    match !only with
    | [] -> test x1 x2 x3 x4 x5 x6
    | l -> if List.mem x1 l then test x1 x2 x3 x4 x5 x6
  in

  match !mode with
  | Test_std -> begin
      testonly "List.sort" false List.sort List.sort lc lc;
      testonly "List.stable_sort" true List.stable_sort List.stable_sort lc lc;
      testonly "Array.sort" false Array.sort Array.sort ac ac;
      testonly "Array.stable_sort" true Array.stable_sort Array.stable_sort
               ac ac;
      printf "Number of tests failed: %d\n" !numfailed;
    end;
  | Test -> begin
      for i = 0 to List.length lold - 1 do
        let (name, f1, stable) = List.nth lold i in
        let (_, f2, _) = List.nth lold i in
        testonly name stable f1 f2 ll ll;
      done;
      testonly "Sort.array" false Sort.array Sort.array al al;
      for i = 0 to List.length lnew - 1 do
        let (name, f1, stable) = List.nth lnew i in
        let (_, f2, _) = List.nth lnew i in
        testonly name stable f1 f2 lc lc;
      done;
      for i = 0 to List.length anew - 1 do
        let (name, f1, stable) = List.nth anew i in
        let (_, f2, _) = List.nth anew i in
        testonly name stable f1 f2 ac ac;
      done;
      printf "Number of tests failed: %d\n" !numfailed;
    end;
  | Bench1 -> begin
      let ba = fun x y z -> benchonly bench1a !size x y z
      and bb = fun x y z -> benchonly bench1b !size x y z
      and bc = fun x y z -> benchonly bench1c !size x y z
      in
      for i = 0 to List.length lold - 1 do
        let (name, f, stable) = List.nth lold i in ba name f ll;
        let (name, f, stable) = List.nth lold i in bb name f ll;
        let (name, f, stable) = List.nth lold i in bc name f ll;
      done;
      ba "Sort.array" Sort.array al;
      bb "Sort.array" Sort.array al;
      bc "Sort.array" Sort.array al;
      for i = 0 to List.length lnew - 1 do
        let (name, f, stable) = List.nth lnew i in ba name f lc;
        let (name, f, stable) = List.nth lnew i in bb name f lc;
        let (name, f, stable) = List.nth lnew i in bc name f lc;
      done;
      for i = 0 to List.length anew - 1 do
        let (name, f, stable) = List.nth anew i in ba name f ac;
        let (name, f, stable) = List.nth anew i in bb name f ac;
        let (name, f, stable) = List.nth anew i in bc name f ac;
      done;
    end;
  | Bench2 -> begin
      let b = fun x y z -> benchonly bench2 !size x y z in
      for i = 0 to List.length lold - 1 do
        let (name, f, stable) = List.nth lold i in b name f ll;
      done;
      b "Sort.array" Sort.array al;
      for i = 0 to List.length lnew - 1 do
        let (name, f, stable) = List.nth lnew i in b name f lc;
      done;
      for i = 0 to List.length anew - 1 do
        let (name, f, stable) = List.nth anew i in b name f ac;
      done;
    end;
  | Bench3 -> begin
      let ba = fun x y z -> benchonly bench3a !size x y z
      and bb = fun x y z -> benchonly bench3b !size x y z
      and bc = fun x y z -> benchonly bench3c !size x y z
      in
      for i = 0 to List.length lold - 1 do
        let (name, f, stable) = List.nth lold i in ba name f ll;
        let (name, f, stable) = List.nth lold i in bb name f ll;
        let (name, f, stable) = List.nth lold i in bc name f ll;
      done;
      for i = 0 to List.length lnew - 1 do
        let (name, f, stable) = List.nth lnew i in ba name f lc;
        let (name, f, stable) = List.nth lnew i in bb name f lc;
        let (name, f, stable) = List.nth lnew i in bc name f lc;
      done;
    end;
;;

if not !Sys.interactive then Printexc.catch main ();;
