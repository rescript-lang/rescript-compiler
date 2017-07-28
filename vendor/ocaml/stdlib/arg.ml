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
type anon_fun = (string -> unit)

type spec =
  | Unit of (unit -> unit)     (* Call the function with unit argument *)
  | Bool of (bool -> unit)     (* Call the function with a bool argument *)
  | Set of bool ref            (* Set the reference to true *)
  | Clear of bool ref          (* Set the reference to false *)
  | String of (string -> unit) (* Call the function with a string argument *)
  | Set_string of string ref   (* Set the reference to the string argument *)
  | Int of (int -> unit)       (* Call the function with an int argument *)
  | Set_int of int ref         (* Set the reference to the int argument *)
  | Float of (float -> unit)   (* Call the function with a float argument *)
  | Set_float of float ref     (* Set the reference to the float argument *)
  | Tuple of spec list         (* Take several arguments according to the
                                  spec list *)
  | Symbol of string list * (string -> unit)
                               (* Take one of the symbols as argument and
                                  call the function with the symbol. *)
  | Rest of (string -> unit)   (* Stop interpreting keywords and call the
                                  function with each remaining argument *)

exception Bad of string
exception Help of string

type error =
  | Unknown of string
  | Wrong of string * string * string  (* option, actual, expected *)
  | Missing of string
  | Message of string

exception Stop of error;; (* used internally *)

open Printf

let rec assoc3 x l =
  match l with
  | [] -> raise Not_found
  | (y1, y2, y3) :: t when y1 = x -> y2
  | _ :: t -> assoc3 x t
;;

let make_symlist prefix sep suffix l =
  match l with
  | [] -> "<none>"
  | h::t -> (List.fold_left (fun x y -> x ^ sep ^ y) (prefix ^ h) t) ^ suffix
;;

let print_spec buf (key, spec, doc) =
  if String.length doc > 0 then
    match spec with
    | Symbol (l, _) ->
        bprintf buf "  %s %s%s\n" key (make_symlist "{" "|" "}" l) doc
    | _ ->
        bprintf buf "  %s %s\n" key doc
;;

let help_action () = raise (Stop (Unknown "-help"));;

let add_help speclist =
  let add1 =
    try ignore (assoc3 "-help" speclist); []
    with Not_found ->
            ["-help", Unit help_action, " Display this list of options"]
  and add2 =
    try ignore (assoc3 "--help" speclist); []
    with Not_found ->
            ["--help", Unit help_action, " Display this list of options"]
  in
  speclist @ (add1 @ add2)
;;

let usage_b buf speclist errmsg =
  bprintf buf "%s\n" errmsg;
  List.iter (print_spec buf) (add_help speclist);
;;

let usage_string speclist errmsg =
  let b = Buffer.create 200 in
  usage_b b speclist errmsg;
  Buffer.contents b;
;;

let usage speclist errmsg =
  eprintf "%s" (usage_string speclist errmsg);
;;

let current = ref 0;;

let parse_argv_dynamic ?(current=current) argv speclist anonfun errmsg =
  let l = Array.length argv in
  let b = Buffer.create 200 in
  let initpos = !current in
  let stop error =
    let progname = if initpos < l then argv.(initpos) else "(?)" in
    begin match error with
      | Unknown "-help" -> ()
      | Unknown "--help" -> ()
      | Unknown s ->
          bprintf b "%s: unknown option '%s'.\n" progname s
      | Missing s ->
          bprintf b "%s: option '%s' needs an argument.\n" progname s
      | Wrong (opt, arg, expected) ->
          bprintf b "%s: wrong argument '%s'; option '%s' expects %s.\n"
                  progname arg opt expected
      | Message s ->
          bprintf b "%s: %s.\n" progname s
    end;
    usage_b b !speclist errmsg;
    if error = Unknown "-help" || error = Unknown "--help"
    then raise (Help (Buffer.contents b))
    else raise (Bad (Buffer.contents b))
  in
  incr current;
  while !current < l do
    let s = argv.(!current) in
    if String.length s >= 1 && s.[0] = '-' then begin
      let action =
        try assoc3 s !speclist
        with Not_found -> stop (Unknown s)
      in
      begin try
        let rec treat_action = function
        | Unit f -> f ();
        | Bool f when !current + 1 < l ->
            let arg = argv.(!current + 1) in
            begin try f (bool_of_string arg)
            with Invalid_argument "bool_of_string" ->
                   raise (Stop (Wrong (s, arg, "a boolean")))
            end;
            incr current;
        | Set r -> r := true;
        | Clear r -> r := false;
        | String f when !current + 1 < l ->
            f argv.(!current + 1);
            incr current;
        | Symbol (symb, f) when !current + 1 < l ->
            let arg = argv.(!current + 1) in
            if List.mem arg symb then begin
              f argv.(!current + 1);
              incr current;
            end else begin
              raise (Stop (Wrong (s, arg, "one of: "
                                          ^ (make_symlist "" " " "" symb))))
            end
        | Set_string r when !current + 1 < l ->
            r := argv.(!current + 1);
            incr current;
        | Int f when !current + 1 < l ->
            let arg = argv.(!current + 1) in
            begin try f (int_of_string arg)
            with Failure "int_of_string" ->
                   raise (Stop (Wrong (s, arg, "an integer")))
            end;
            incr current;
        | Set_int r when !current + 1 < l ->
            let arg = argv.(!current + 1) in
            begin try r := (int_of_string arg)
            with Failure "int_of_string" ->
                   raise (Stop (Wrong (s, arg, "an integer")))
            end;
            incr current;
        | Float f when !current + 1 < l ->
            let arg = argv.(!current + 1) in
            begin try f (float_of_string arg);
            with Failure "float_of_string" ->
                   raise (Stop (Wrong (s, arg, "a float")))
            end;
            incr current;
        | Set_float r when !current + 1 < l ->
            let arg = argv.(!current + 1) in
            begin try r := (float_of_string arg);
            with Failure "float_of_string" ->
                   raise (Stop (Wrong (s, arg, "a float")))
            end;
            incr current;
        | Tuple specs ->
            List.iter treat_action specs;
        | Rest f ->
            while !current < l - 1 do
              f argv.(!current + 1);
              incr current;
            done;
        | _ -> raise (Stop (Missing s))
        in
        treat_action action
      with Bad m -> stop (Message m);
         | Stop e -> stop e;
      end;
      incr current;
    end else begin
      (try anonfun s with Bad m -> stop (Message m));
      incr current;
    end;
  done;
;;

let parse_argv ?(current=current) argv speclist anonfun errmsg =
  parse_argv_dynamic ~current:current argv (ref speclist) anonfun errmsg;
;;

let parse l f msg =
  try
    parse_argv Sys.argv l f msg;
  with
  | Bad msg -> eprintf "%s" msg; exit 2;
  | Help msg -> printf "%s" msg; exit 0;
;;

let parse_dynamic l f msg =
  try
    parse_argv_dynamic Sys.argv l f msg;
  with
  | Bad msg -> eprintf "%s" msg; exit 2;
  | Help msg -> printf "%s" msg; exit 0;
;;

let second_word s =
  let len = String.length s in
  let rec loop n =
    if n >= len then len
    else if s.[n] = ' ' then loop (n+1)
    else n
  in
  try loop (String.index s ' ')
  with Not_found -> len
;;

let max_arg_len cur (kwd, spec, doc) =
  match spec with
  | Symbol _ -> max cur (String.length kwd)
  | _ -> max cur (String.length kwd + second_word doc)
;;

let add_padding len ksd =
  match ksd with
  | (_, _, "") ->
      (* Do not pad undocumented options, so that they still don't show up when
       * run through [usage] or [parse]. *)
      ksd
  | (kwd, (Symbol (l, _) as spec), msg) ->
      let cutcol = second_word msg in
      let spaces = String.make ((max 0 (len - cutcol)) + 3) ' ' in
      (kwd, spec, "\n" ^ spaces ^ msg)
  | (kwd, spec, msg) ->
      let cutcol = second_word msg in
      let kwd_len = String.length kwd in
      let diff = len - kwd_len - cutcol in
      if diff <= 0 then
        (kwd, spec, msg)
      else
        let spaces = String.make diff ' ' in
        let prefix = String.sub msg 0 cutcol in
        let suffix = String.sub msg cutcol (String.length msg - cutcol) in
        (kwd, spec, prefix ^ spaces ^ suffix)
;;

let align ?(limit=max_int) speclist =
  let completed = add_help speclist in
  let len = List.fold_left max_arg_len 0 completed in
  let len = min len limit in
  List.map (add_padding len) completed
;;
