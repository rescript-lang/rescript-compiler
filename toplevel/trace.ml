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

(* The "trace" facility *)

open Format
open Misc
open Longident
open Types
open Toploop

type codeptr = Obj.t

type traced_function =
  { path: Path.t;                       (* Name under which it is traced *)
    closure: Obj.t;                     (* Its function closure (patched) *)
    actual_code: codeptr;               (* Its original code pointer *)
    instrumented_fun: codeptr -> Obj.t -> Obj.t -> Obj.t }
                                        (* Printing function *)

let traced_functions = ref ([] : traced_function list)

(* Check if a function is already traced *)

let is_traced clos =
  let rec is_traced = function
      [] -> None
    | tf :: rem -> if tf.closure == clos then Some tf.path else is_traced rem
  in is_traced !traced_functions

(* Get or overwrite the code pointer of a closure *)

let get_code_pointer cls = Obj.field cls 0

let set_code_pointer cls ptr = Obj.set_field cls 0 ptr

(* Call a traced function (use old code pointer, but new closure as
   environment so that recursive calls are also traced).
   It is necessary to wrap Meta.invoke_traced_function in an ML function
   so that the RETURN at the end of the ML wrapper takes us to the
   code of the function. *)

let invoke_traced_function codeptr env arg =
  Meta.invoke_traced_function codeptr env arg

let print_label ppf l = if l <> "" then fprintf ppf "%s:" l

(* If a function returns a functional value, wrap it into a trace code *)

let rec instrument_result env name ppf clos_typ =
  match (Ctype.repr(Ctype.expand_head env clos_typ)).desc with
  | Tarrow(l, t1, t2, _) ->
      let starred_name =
        match name with
        | Lident s -> Lident(s ^ "*")
        | Ldot(lid, s) -> Ldot(lid, s ^ "*")
        | Lapply(l1, l2) -> fatal_error "Trace.instrument_result" in
      let trace_res = instrument_result env starred_name ppf t2 in
      (fun clos_val ->
        Obj.repr (fun arg ->
          if not !may_trace then
            (Obj.magic clos_val : Obj.t -> Obj.t) arg
          else begin
            may_trace := false;
            try
              fprintf ppf "@[<2>%a <--@ %a%a@]@."
                Printtyp.longident starred_name
                print_label l
                (print_value !toplevel_env arg) t1;
              may_trace := true;
              let res = (Obj.magic clos_val : Obj.t -> Obj.t) arg in
              may_trace := false;
              fprintf ppf "@[<2>%a -->@ %a@]@."
                Printtyp.longident starred_name
                (print_value !toplevel_env res) t2;
              may_trace := true;
              trace_res res
            with exn ->
              may_trace := false;
              fprintf ppf "@[<2>%a raises@ %a@]@."
                Printtyp.longident starred_name
                (print_value !toplevel_env (Obj.repr exn)) Predef.type_exn;
              may_trace := true;
              raise exn
          end))
  | _ -> (fun v -> v)

(* Same as instrument_result, but for a toplevel closure (modified in place) *)

exception Dummy
let _ = Dummy

let instrument_closure env name ppf clos_typ =
  match (Ctype.repr(Ctype.expand_head env clos_typ)).desc with
  | Tarrow(l, t1, t2, _) ->
      let trace_res = instrument_result env name ppf t2 in
      (fun actual_code closure arg ->
        if not !may_trace then begin
          try invoke_traced_function actual_code closure arg
          with Dummy -> assert false
          (* do not remove handler, prevents tail-call to invoke_traced_ *)
        end else begin
          may_trace := false;
          try
            fprintf ppf "@[<2>%a <--@ %a%a@]@."
              Printtyp.longident name
              print_label l
              (print_value !toplevel_env arg) t1;
            may_trace := true;
            let res = invoke_traced_function actual_code closure arg in
            may_trace := false;
            fprintf ppf "@[<2>%a -->@ %a@]@."
              Printtyp.longident name
              (print_value !toplevel_env res) t2;
            may_trace := true;
            trace_res res
          with exn ->
            may_trace := false;
            fprintf ppf "@[<2>%a raises@ %a@]@."
              Printtyp.longident name
              (print_value !toplevel_env (Obj.repr exn)) Predef.type_exn;
            may_trace := true;
            raise exn
        end)
  | _ -> assert false

(* Given the address of a closure, find its tracing info *)

let rec find_traced_closure clos = function
  | [] -> fatal_error "Trace.find_traced_closure"
  | f :: rem -> if f.closure == clos then f else find_traced_closure clos rem

(* Trace the application of an (instrumented) closure to an argument *)

let print_trace clos arg =
  let f = find_traced_closure clos !traced_functions in
  f.instrumented_fun f.actual_code clos arg
