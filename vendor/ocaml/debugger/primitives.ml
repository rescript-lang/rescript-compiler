(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          OCaml port by John Malecki and Xavier Leroy                *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(*********************** Basic functions and types *********************)

(*** Miscellaneous ***)
exception Out_of_range

let nothing _ = ()

(*** Operations on lists. ***)

(* Remove an element from a list *)
let except e l =
 let rec except_e = function
     [] -> []
   | elem::l -> if e = elem then l else elem::except_e l
 in except_e l

(* Position of an element in a list. Head of list has position 0. *)
let index a l =
 let rec index_rec i = function
     []  -> raise Not_found
  | b::l -> if a = b then i else index_rec (i + 1) l
 in index_rec 0 l

(* Return the `n' first elements of `l' *)
(* ### n l -> l' *)
let rec list_truncate =
  fun
    p0 p1 -> match (p0,p1) with (0, _)      -> []
  | (_, [])     -> []
  | (n, (a::l)) -> a::(list_truncate (n - 1) l)

(* Separe the `n' first elements of `l' and the others *)
(* ### n list -> (first, last) *)
let rec list_truncate2 =
  fun
    p0 p1 -> match (p0,p1) with (0, l) ->
      ([], l)
  | (_, []) ->
      ([], [])
  | (n, (a::l)) ->
      let (first, last) = (list_truncate2 (n - 1) l) in
        (a::first, last)

(* Replace x by y in list l *)
(* ### x y l -> l' *)
let list_replace x y =
  let rec repl =
    function
      [] -> []
    | a::l ->
        if a == x then y::l
        else a::(repl l)
  in repl

(*** Operations on strings. ***)

(* Remove blanks (spaces and tabs) at beginning and end of a string. *)
let is_space = function
  | ' ' | '\t' -> true | _ -> false

let string_trim s =
  let l = String.length s and i = ref 0 in
    while
      !i < l && is_space (String.get s !i)
    do
      incr i
    done;
    let j = ref (l - 1) in
      while
        !j >= !i && is_space (String.get s !j)
      do
        decr j
      done;
      String.sub s !i (!j - !i + 1)

(* isprefix s1 s2 returns true if s1 is a prefix of s2. *)

let isprefix s1 s2 =
  let l1 = String.length s1 and l2 = String.length s2 in
  (l1 = l2 && s1 = s2) || (l1 < l2 && s1 = String.sub s2 0 l1)

(* Split a string at the given delimiter char *)

let split_string sep str =
  let rec split i j =
    if j >= String.length str then
      if i >= j then [] else [String.sub str i (j-i)]
    else if str.[j] = sep then
      if i >= j
      then skip_sep (j+1)
      else String.sub str i (j-i) :: skip_sep (j+1)
    else
      split i (j+1)
  and skip_sep j =
    if j < String.length str && str.[j] = sep
    then skip_sep (j+1)
    else split j j
  in split 0 0

(*** I/O channels ***)

type io_channel = {
  io_in : in_channel;
  io_out : out_channel;
  io_fd : Unix.file_descr
  }

let io_channel_of_descr fd = {
  io_in = Unix.in_channel_of_descr fd;
  io_out = Unix.out_channel_of_descr fd;
  io_fd = fd
  }

let close_io io_channel =
  close_out_noerr io_channel.io_out;
  close_in_noerr io_channel.io_in;
;;

let std_io = {
  io_in = stdin;
  io_out = stdout;
  io_fd = Unix.stdin
  }
