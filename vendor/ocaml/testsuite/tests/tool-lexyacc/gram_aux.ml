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

(* Auxiliaries for the parser. *)

open Syntax

let regexp_for_string s =
  let l = String.length s in
  if l = 0 then
    Epsilon
  else begin
    let re = ref(Characters [String.get s (l - 1)]) in
    for i = l - 2 downto 0 do
      re := Sequence(Characters [String.get s i], !re)
    done;
    !re
  end


let char_class c1 c2 =
  let cl = ref [] in
  for i = Char.code c2 downto Char.code c1 do
    cl := Char.chr i :: !cl
  done;
  !cl


let all_chars = char_class '\001' '\255'


let rec subtract l1 l2 =
  match l1 with
    [] -> []
  | a::l -> if List.mem a l2 then subtract l l2 else a :: subtract l l2
