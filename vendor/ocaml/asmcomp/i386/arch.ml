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

(* Machine-specific command-line options *)

let fast_math = ref false

let command_line_options =
  [ "-ffast-math", Arg.Set fast_math,
      " Inline trigonometric and exponential functions" ]

(* Specific operations for the Intel 386 processor *)

open Format

type addressing_mode =
    Ibased of string * int              (* symbol + displ *)
  | Iindexed of int                     (* reg + displ *)
  | Iindexed2 of int                    (* reg + reg + displ *)
  | Iscaled of int * int                (* reg * scale + displ *)
  | Iindexed2scaled of int * int        (* reg + reg * scale + displ *)

type specific_operation =
    Ilea of addressing_mode             (* Lea gives scaled adds *)
  | Istore_int of nativeint * addressing_mode * bool
                                        (* Store an integer constant *)
  | Istore_symbol of string * addressing_mode * bool (* Store a symbol *)
  | Ioffset_loc of int * addressing_mode (* Add a constant to a location *)
  | Ipush                               (* Push regs on stack *)
  | Ipush_int of nativeint              (* Push an integer constant *)
  | Ipush_symbol of string              (* Push a symbol *)
  | Ipush_load of addressing_mode       (* Load a scalar and push *)
  | Ipush_load_float of addressing_mode (* Load a float and push *)
  | Isubfrev | Idivfrev                 (* Reversed float sub and div *)
  | Ifloatarithmem of bool * float_operation * addressing_mode
                                        (* Float arith operation with memory *)
                                        (* bool: true=64 bits, false=32 *)
  | Ifloatspecial of string

and float_operation =
    Ifloatadd | Ifloatsub | Ifloatsubrev | Ifloatmul | Ifloatdiv | Ifloatdivrev

(* Sizes, endianness *)

let big_endian = false

let size_addr = 4
let size_int = 4
let size_float = 8

let allow_unaligned_access = true

(* Behavior of division *)

let division_crashes_on_overflow = true

(* Operations on addressing modes *)

let identity_addressing = Iindexed 0

let offset_addressing addr delta =
  match addr with
    Ibased(s, n) -> Ibased(s, n + delta)
  | Iindexed n -> Iindexed(n + delta)
  | Iindexed2 n -> Iindexed2(n + delta)
  | Iscaled(scale, n) -> Iscaled(scale, n + delta)
  | Iindexed2scaled(scale, n) -> Iindexed2scaled(scale, n + delta)

let num_args_addressing = function
    Ibased(s, n) -> 0
  | Iindexed n -> 1
  | Iindexed2 n -> 2
  | Iscaled(scale, n) -> 1
  | Iindexed2scaled(scale, n) -> 2

(* Printing operations and addressing modes *)

let print_addressing printreg addr ppf arg =
  match addr with
  | Ibased(s, 0) ->
      fprintf ppf "\"%s\"" s
  | Ibased(s, n) ->
      fprintf ppf "\"%s\" + %i" s n
  | Iindexed n ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a%s" printreg arg.(0) idx
  | Iindexed2 n ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a + %a%s" printreg arg.(0) printreg arg.(1) idx
  | Iscaled(scale, n) ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a  * %i%s" printreg arg.(0) scale idx
  | Iindexed2scaled(scale, n) ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a + %a * %i%s" printreg arg.(0) printreg arg.(1) scale idx

let print_specific_operation printreg op ppf arg =
  match op with
  | Ilea addr -> print_addressing printreg addr ppf arg
  | Istore_int(n, addr, is_assign) ->
      fprintf ppf "[%a] := %nd %s"
         (print_addressing printreg addr) arg n
         (if is_assign then "(assign)" else "(init)")
  | Istore_symbol(lbl, addr, is_assign) ->
      fprintf ppf "[%a] := \"%s\" %s"
         (print_addressing printreg addr) arg lbl
         (if is_assign then "(assign)" else "(init)")
  | Ioffset_loc(n, addr) ->
      fprintf ppf "[%a] +:= %i" (print_addressing printreg addr) arg n
  | Ipush ->
      fprintf ppf "push ";
      for i = 0 to Array.length arg - 1 do
        if i > 0 then fprintf ppf ", ";
        printreg ppf arg.(i)
      done
  | Ipush_int n ->
      fprintf ppf "push %s" (Nativeint.to_string n)
  | Ipush_symbol s ->
      fprintf ppf "push \"%s\"" s
  | Ipush_load addr ->
      fprintf ppf "push [%a]" (print_addressing printreg addr) arg
  | Ipush_load_float addr ->
      fprintf ppf "pushfloat [%a]" (print_addressing printreg addr) arg
  | Isubfrev ->
      fprintf ppf "%a -f(rev) %a" printreg arg.(0) printreg arg.(1)
  | Idivfrev ->
      fprintf ppf "%a /f(rev) %a" printreg arg.(0) printreg arg.(1)
  | Ifloatarithmem(double, op, addr) ->
      let op_name = function
      | Ifloatadd -> "+f"
      | Ifloatsub -> "-f"
      | Ifloatsubrev -> "-f(rev)"
      | Ifloatmul -> "*f"
      | Ifloatdiv -> "/f"
      | Ifloatdivrev -> "/f(rev)" in
      let long = if double then "float64" else "float32" in
      fprintf ppf "%a %s %s[%a]" printreg arg.(0) (op_name op) long
       (print_addressing printreg addr) (Array.sub arg 1 (Array.length arg - 1))
  | Ifloatspecial name ->
      fprintf ppf "%s " name;
      for i = 0 to Array.length arg - 1 do
        if i > 0 then fprintf ppf ", ";
        printreg ppf arg.(i)
      done

(* Stack alignment constraints *)

let stack_alignment =
  match Config.system with
  | "win32" -> 4     (* MSVC *)
  | _ -> 16
(* PR#6038: GCC and Clang seem to require 16-byte alignment nowadays,
   even if only MacOS X's ABI formally requires it *)
