(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Tests for matchings on integers and characters *)

(* Dense integer switch *)

let f = function 1 -> 1 | 2 -> 2 | 3 -> 3 | 4 -> 4 | 5 -> 5 | 6 -> 6 | _ -> 0

(* Sparse integer switch *)

let g = function 303 -> 1 | 401 -> 2 | _ -> 0

(* Very sparse integer switch *)

let iszero = function 0 -> true | _ -> false

(* Simple matching on characters *)

let h = function
    'a' -> "a"
  | 'e' -> "e"
  | 'i' -> "i"
  | 'o' -> "o"
  | 'u' -> "u"
  | _ -> "?"

(* Matching with orpats *)

let k = function
    ' ' | '\t' | '\n' | '\r' -> "blk"
  | 'A'..'Z' | 'a'..'z' | '\192'..'\255' -> "letr"
  | '0'..'9' -> "dig"
  | '!'|'%'|'&'|'$'|'#'|'+'|'/'|':'|'<'|'='|'>'|'?'|'@'|'\\'|
             '~'|'^'|'|'|'*' -> "oper"
  | _ -> "othr"

(* Matching on arrays *)

let p = function [| x |] -> x | _ -> assert false

let q = function [| x |] -> x | _ -> 0

let r = function [| x |] -> x | _ -> 0.0

let l = function
    [||] -> 0
  | [|x|] -> x + 1
  | [|x;y|] -> x + y
  | [|x;y;z|] -> x + y + z
  | _ -> assert false

(* The test *)

open Printf

external string_create: int -> string = "caml_create_string"
external unsafe_chr: int -> char = "%identity"
external string_unsafe_set : string -> int -> char -> unit
                           = "%string_unsafe_set"

(* The following function is roughly equivalent to Char.escaped,
   except that it is locale-independent. *)
let escaped = function
  | '\'' -> "\\'"
  | '\\' -> "\\\\"
  | '\n' -> "\\n"
  | '\t' -> "\\t"
  | '\r' -> "\\r"
  | '\b' -> "\\b"
  | c ->
    if ((k c) <> "othr") && ((Char.code c) <= 191) then begin
      let s = string_create 1 in
      string_unsafe_set s 0 c;
      s
    end else begin
      let n = Char.code c in
      let s = string_create 4 in
      string_unsafe_set s 0 '\\';
      string_unsafe_set s 1 (unsafe_chr (48 + n / 100));
      string_unsafe_set s 2 (unsafe_chr (48 + (n / 10) mod 10));
      string_unsafe_set s 3 (unsafe_chr (48 + n mod 10));
      s
    end

let _ =
  for i = -5 to 10 do printf "f(%d) = %d\n" i (f i) done;
  List.iter (fun i -> printf "g(%d) = %d\n" i (g i))
            [0;300;303;305;400;401;402;999];
  for i = -2 to 2 do printf "iszero(%d) = %B\n" i (iszero i) done;
  for i = 97 to 126 do
    let c = Char.chr i in
    printf "h(%c) = %s\n" c (h c)
  done;
  for i = 0 to 255 do
    let c = Char.chr i in
    printf "\tk(%s) = %s" (escaped c) (k c)
  done;
  printf "\n";
  printf "p([|\"hello\"|]) = %s\n" (p [|"hello"|]);
  printf "p([|1.0|]) = %f\n" (p [|1.0|]);
  printf "q([|2|]) = %d\n" (q [|2|]);
  printf "r([|3.0|]) = %f\n" (r [|3.0|]);
  printf "l([||]) = %d\n" (l [||]);
  printf "l([|1|]) = %d\n" (l [|1|]);
  printf "l([|2;3|]) = %d\n" (l [|2;3|]);
  printf "l([|4;5;6|]) = %d\n" (l [|4;5;6|])

(* PR #5992 *)
(* Was segfaulting *)

let f = function
 | lazy (), _, {contents=None} -> 0
 | _, lazy (), {contents=Some x} -> 1

let s = ref None
let set_true = lazy (s := Some 1)
let set_false = lazy (s := None)

let () =
  let _r = try f (set_true, set_false, s) with Match_failure _ -> 2 in
  printf "PR#5992=Ok\n"

(* PR #5788, was giving wrong result 3 *)
exception Foo
exception Bar = Foo

let test e b =
  match e, b with
  | Foo, true -> 1
  | Bar, false -> 2
  | _, _ -> 3

let () =
  let r = test Bar false in
  if r = 2 then printf "PR#5788=Ok\n"

let test e b =
  match e, b with
  | Bar, false -> 0
  | (Foo|Bar), true -> 1
  | Foo, false -> 2
  | _, _ -> 3


let () =
  let r = test Foo false in
  if r = 0 then printf "PR#5788=Ok\n"


(* No string sharing PR#6322 *)
let test x = match x with
  | true -> "a"
  | false -> "a"

let () =
  let s1 = test true in
  let s2 = test false in
  s1.[0] <- 'p';
  if s1 <> s2 then printf "PR#6322=Ok\n%!"
