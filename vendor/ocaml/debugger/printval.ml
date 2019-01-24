(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Jerome Vouillon, projet Cristal, INRIA Rocquencourt          *)
(*           OCaml port by John Malecki and Xavier Leroy                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* To print values *)

open Format
open Parser_aux
open Path
open Types

(* To name printed and ellipsed values *)

let named_values =
  (Hashtbl.create 29 : (int, Debugcom.Remote_value.t * type_expr) Hashtbl.t)
let next_name = ref 1

let reset_named_values () =
  Hashtbl.clear named_values;
  next_name := 1

let name_value v ty =
  let name = !next_name in
  incr next_name;
  Hashtbl.add named_values name (v, ty);
  name

let find_named_value name =
  Hashtbl.find named_values name

let check_depth depth obj ty =
  if depth <= 0 then begin
    let n = name_value obj ty in
    Some (Outcometree.Oval_stuff ("$" ^ string_of_int n))
  end else None

module EvalPath =
  struct
    type valu = Debugcom.Remote_value.t
    exception Error
    let rec eval_path env = function
      Pident id ->
        begin try
          Debugcom.Remote_value.global (Symtable.get_global_position id)
        with Symtable.Error _ ->
          raise Error
        end
    | Pdot(root, _fieldname, pos) ->
        let v = eval_path env root in
        if not (Debugcom.Remote_value.is_block v)
        then raise Error
        else Debugcom.Remote_value.field v pos
    | Papply _ ->
        raise Error
    let same_value = Debugcom.Remote_value.same
  end

module Printer = Genprintval.Make(Debugcom.Remote_value)(EvalPath)

let install_printer path ty _ppf fn =
  Printer.install_printer path ty
    (fun ppf remote_val ->
       try
         fn ppf (Obj.repr (Debugcom.Remote_value.obj remote_val))
       with
         Debugcom.Marshalling_error ->
           fprintf ppf "<cannot fetch remote object>")

let remove_printer = Printer.remove_printer

let max_printer_depth = ref 20
let max_printer_steps = ref 300

let print_exception ppf obj =
  let t = Printer.outval_of_untyped_exception obj in
  !Oprint.out_value ppf t

let print_value max_depth env obj (ppf : Format.formatter) ty =
  let t =
    Printer.outval_of_value !max_printer_steps max_depth
      check_depth env obj ty in
  !Oprint.out_value ppf t

let print_named_value max_depth exp env obj ppf ty =
  let print_value_name ppf = function
  | E_ident lid ->
      Printtyp.longident ppf lid
  | E_name n ->
      fprintf ppf "$%i" n
  | _ ->
      let n = name_value obj ty in
      fprintf ppf "$%i" n in
  Printtyp.reset_and_mark_loops ty;
  fprintf ppf "@[<2>%a:@ %a@ =@ %a@]@."
  print_value_name exp
  Printtyp.type_expr ty
  (print_value max_depth env obj) ty
