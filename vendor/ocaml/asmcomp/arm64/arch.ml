(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                 Benedikt Meurer, University of Siegen                  *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2012 Benedikt Meurer.                                      *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Specific operations for the ARM processor, 64-bit mode *)

open Format

let command_line_options = []

(* Addressing modes *)

type addressing_mode =
  | Iindexed of int                     (* reg + displ *)
  | Ibased of string * int              (* global var + displ *)

(* We do not support the reg + shifted reg addressing mode, because
   what we really need is reg + shifted reg + displ,
   and this is decomposed in two instructions (reg + shifted reg -> tmp,
   then addressing tmp + displ). *)

(* Specific operations *)

type cmm_label = int
  (* Do not introduce a dependency to Cmm *)

type specific_operation =
  | Ifar_alloc of { words : int; label_after_call_gc : cmm_label option; }
  | Ifar_intop_checkbound of { label_after_error : cmm_label option; }
  | Ifar_intop_imm_checkbound of
      { bound : int; label_after_error : cmm_label option; }
  | Ishiftarith of arith_operation * int
  | Ishiftcheckbound of { shift : int; label_after_error : cmm_label option; }
  | Ifar_shiftcheckbound of
      { shift : int; label_after_error : cmm_label option; }
  | Imuladd       (* multiply and add *)
  | Imulsub       (* multiply and subtract *)
  | Inegmulf      (* floating-point negate and multiply *)
  | Imuladdf      (* floating-point multiply and add *)
  | Inegmuladdf   (* floating-point negate, multiply and add *)
  | Imulsubf      (* floating-point multiply and subtract *)
  | Inegmulsubf   (* floating-point negate, multiply and subtract *)
  | Isqrtf        (* floating-point square root *)
  | Ibswap of int (* endianness conversion *)

and arith_operation =
    Ishiftadd
  | Ishiftsub

let spacetime_node_hole_pointer_is_live_before = function
  | Ifar_alloc _ | Ifar_intop_checkbound _ | Ifar_intop_imm_checkbound _
  | Ishiftarith _ | Ishiftcheckbound _ | Ifar_shiftcheckbound _ -> false
  | Imuladd | Imulsub | Inegmulf | Imuladdf | Inegmuladdf | Imulsubf
  | Inegmulsubf | Isqrtf | Ibswap _ -> false

(* Sizes, endianness *)

let big_endian = false

let size_addr = 8
let size_int = 8
let size_float = 8

let allow_unaligned_access = false

(* Behavior of division *)

let division_crashes_on_overflow = false

(* Operations on addressing modes *)

let identity_addressing = Iindexed 0

let offset_addressing addr delta =
  match addr with
  | Iindexed n -> Iindexed(n + delta)
  | Ibased(s, n) -> Ibased(s, n + delta)

let num_args_addressing = function
  | Iindexed _ -> 1
  | Ibased _ -> 0

(* Printing operations and addressing modes *)

let print_addressing printreg addr ppf arg =
  match addr with
  | Iindexed n ->
      printreg ppf arg.(0);
      if n <> 0 then fprintf ppf " + %i" n
  | Ibased(s, 0) ->
      fprintf ppf "\"%s\"" s
  | Ibased(s, n) ->
      fprintf ppf "\"%s\" + %i" s n

let print_specific_operation printreg op ppf arg =
  match op with
  | Ifar_alloc { words; label_after_call_gc = _; } ->
    fprintf ppf "(far) alloc %i" words
  | Ifar_intop_checkbound _ ->
    fprintf ppf "%a (far) check > %a" printreg arg.(0) printreg arg.(1)
  | Ifar_intop_imm_checkbound { bound; _ } ->
    fprintf ppf "%a (far) check > %i" printreg arg.(0) bound
  | Ishiftarith(op, shift) ->
      let op_name = function
      | Ishiftadd -> "+"
      | Ishiftsub -> "-" in
      let shift_mark =
       if shift >= 0
       then sprintf "<< %i" shift
       else sprintf ">> %i" (-shift) in
      fprintf ppf "%a %s %a %s"
       printreg arg.(0) (op_name op) printreg arg.(1) shift_mark
  | Ishiftcheckbound { shift; _ } ->
      fprintf ppf "check %a >> %i > %a" printreg arg.(0) shift
        printreg arg.(1)
  | Ifar_shiftcheckbound { shift; _ } ->
      fprintf ppf
        "(far) check %a >> %i > %a" printreg arg.(0) shift printreg arg.(1)
  | Imuladd ->
      fprintf ppf "(%a * %a) + %a"
        printreg arg.(0)
        printreg arg.(1)
        printreg arg.(2)
  | Imulsub ->
      fprintf ppf "-(%a * %a) + %a"
        printreg arg.(0)
        printreg arg.(1)
        printreg arg.(2)
  | Inegmulf ->
      fprintf ppf "-f (%a *f %a)"
        printreg arg.(0)
        printreg arg.(1)
  | Imuladdf ->
      fprintf ppf "%a +f (%a *f %a)"
        printreg arg.(0)
        printreg arg.(1)
        printreg arg.(2)
  | Inegmuladdf ->
      fprintf ppf "(-f %a) -f (%a *f %a)"
        printreg arg.(0)
        printreg arg.(1)
        printreg arg.(2)
  | Imulsubf ->
      fprintf ppf "%a -f (%a *f %a)"
        printreg arg.(0)
        printreg arg.(1)
        printreg arg.(2)
  | Inegmulsubf ->
      fprintf ppf "(-f %a) +f (%a *f %a)"
        printreg arg.(0)
        printreg arg.(1)
        printreg arg.(2)
  | Isqrtf ->
      fprintf ppf "sqrtf %a"
        printreg arg.(0)
  | Ibswap n ->
      fprintf ppf "bswap%i %a" n
        printreg arg.(0)
