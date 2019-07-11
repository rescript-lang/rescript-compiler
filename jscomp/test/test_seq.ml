(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

type key = string
type doc = string
type usage_msg = string
type anon_fun = string -> unit

type spec =
  | Unit of (unit -> unit) (* Call the function with unit argument *)
  | Bool of (bool -> unit) (* Call the function with a bool argument *)
  | Set of bool ref (* Set the reference to true *)
  | Clear of bool ref (* Set the reference to false *)
  | String of (string -> unit) (* Call the function with a string argument *)
  | Set_string of string ref (* Set the reference to the string argument *)
  | Int of (int -> unit) (* Call the function with an int argument *)
  | Set_int of int ref (* Set the reference to the int argument *)
  | Float of (float -> unit) (* Call the function with a float argument *)
  | Set_float of float ref (* Set the reference to the float argument *)
  | Tuple of spec list (* Take several arguments according to the spec list *)
  | Symbol of string list * (string -> unit)
  (* Take one of the symbols as argument and call the function with the symbol. *)
  | Rest of (string -> unit)

(* Stop interpreting keywords and call the function with each remaining
   argument *)

exception Bad of string
exception Help of string

type error =
  | Unknown of string
  | Wrong of string * string * string (* option, actual, expected *)
  | Missing of string
  | Message of string

exception Stop of error

(* used internally *)

open Printf

let rec assoc3 x l =
  match l with
  | [] -> raise Not_found
  | (y1, y2, y3) :: t when y1 = x -> y2
  | _ :: t -> assoc3 x t

(* let make_symlist prefix sep suffix l = *)
(* match l with *)
(* | [] -> "<none>" *)
(* | h::t -> (List.fold_left (fun x y -> x ^ sep ^ y) (prefix ^ h) t) ^ suffix *)
(* ;; *)

(* let print_spec buf (key, spec, doc) = *)
(* if String.length doc > 0 then *)
(* match spec with *)
(* | Symbol (l, _) -> *)
(* bprintf buf " %s %s%s\n" key (make_symlist "{" "|" "}" l) doc *)
(* | _ -> *)
(* bprintf buf " %s %s\n" key doc *)
(* ;; *)

let help_action () = raise (Stop (Unknown "-help"))

let v speclist =
  ignore (assoc3 "-help" speclist) ;
  []

let f g speclist = g (assoc3 "-help" speclist)

let add_help speclist =
  let add1 =
    try
      ignore (assoc3 "-help" speclist) ;
      []
    with Not_found ->
      [("-help", Unit help_action, " Display this list of options")]
  and add2 =
    try
      ignore (assoc3 "--help" speclist) ;
      []
    with Not_found ->
      [("--help", Unit help_action, " Display this list of options")] in
  speclist @ add1 @ add2

(** FIXME- not compatible with strict mode *)

(* let f x x = x *)
