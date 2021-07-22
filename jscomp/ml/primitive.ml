(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Description of primitive functions *)

open Misc
open Parsetree

type boxed_integer = Pnativeint | Pint32 | Pint64

type native_repr =
  | Same_as_ocaml_repr

type description =
  { prim_name: string;         (* Name of primitive  or C function *)
    prim_arity: int;           (* Number of arguments *)
    prim_alloc: bool;          (* Does it allocates or raise? *)
    prim_native_name: string;  (* Name of C function for the nat. code gen. *)
    prim_native_repr_args: native_repr list;
    prim_native_repr_res: native_repr }

let coerce : (description -> description -> bool) ref = 
  ref (fun 
        (p1 : description) (p2 : description) ->
        p1 = p2
      )



let rec make_native_repr_args arity x =
  if arity = 0 then
    []
  else
    x :: make_native_repr_args (arity - 1) x

let simple ~name ~arity ~alloc =
  {prim_name = name;
   prim_arity = arity;
   prim_alloc = alloc;
   prim_native_name = "";
   prim_native_repr_args = make_native_repr_args arity Same_as_ocaml_repr;
   prim_native_repr_res = Same_as_ocaml_repr}

let make ~name ~alloc ~native_name ~native_repr_args ~native_repr_res =
  {prim_name = name;
   prim_arity = List.length native_repr_args;
   prim_alloc = alloc;
   prim_native_name = native_name;
   prim_native_repr_args = native_repr_args;
   prim_native_repr_res = native_repr_res}

let parse_declaration valdecl ~native_repr_args ~native_repr_res =
  let arity = List.length native_repr_args in
  let name, native_name =
    match valdecl.pval_prim with
    | name :: name2 :: _ -> (name, name2)
    | name :: _ -> (name, "")
    | [] ->
        fatal_error "Primitive.parse_declaration"
  in
  {prim_name = name;
   prim_arity = arity;
   prim_alloc = true;
   prim_native_name = native_name;
   prim_native_repr_args = native_repr_args;
   prim_native_repr_res = native_repr_res}

open Outcometree

let print p osig_val_decl =
  let prims =
    if p.prim_native_name <> "" then
      [p.prim_name; p.prim_native_name]
    else
      [p.prim_name]
  in
  { osig_val_decl with
    oval_prims = prims;
    oval_attributes = [] }

let native_name p =
  if p.prim_native_name <> ""
  then p.prim_native_name
  else p.prim_name

let byte_name p =
  p.prim_name

