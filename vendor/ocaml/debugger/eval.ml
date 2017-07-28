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

open Misc
open Path
open Instruct
open Types
open Parser_aux

type error =
    Unbound_identifier of Ident.t
  | Not_initialized_yet of Path.t
  | Unbound_long_identifier of Longident.t
  | Unknown_name of int
  | Tuple_index of type_expr * int * int
  | Array_index of int * int
  | List_index of int * int
  | String_index of string * int * int
  | Wrong_item_type of type_expr * int
  | Wrong_label of type_expr * string
  | Not_a_record of type_expr
  | No_result

exception Error of error

let abstract_type =
  Btype.newgenty (Tconstr (Pident (Ident.create "<abstr>"), [], ref Mnil))

let rec path event = function
    Pident id ->
      if Ident.global id then
        try
          Debugcom.Remote_value.global (Symtable.get_global_position id)
        with Symtable.Error _ -> raise(Error(Unbound_identifier id))
      else
        begin match event with
          Some ev ->
            begin try
              let pos = Ident.find_same id ev.ev_compenv.ce_stack in
              Debugcom.Remote_value.local (ev.ev_stacksize - pos)
            with Not_found ->
            try
              let pos = Ident.find_same id ev.ev_compenv.ce_heap in
              Debugcom.Remote_value.from_environment pos
            with Not_found ->
              raise(Error(Unbound_identifier id))
            end
        | None ->
            raise(Error(Unbound_identifier id))
        end
  | Pdot(root, fieldname, pos) ->
      let v = path event root in
      if not (Debugcom.Remote_value.is_block v) then
        raise(Error(Not_initialized_yet root));
      Debugcom.Remote_value.field v pos
  | Papply(p1, p2) ->
      fatal_error "Eval.path: Papply"

let rec expression event env = function
    E_ident lid ->
      begin try
        let (p, valdesc) = Env.lookup_value lid env in
        (begin match valdesc.val_kind with
           Val_ivar (_, cl_num) ->
             let (p0, _) =
               Env.lookup_value (Longident.Lident ("self-" ^ cl_num)) env
             in
             let v = path event p0 in
             let i = path event p in
             Debugcom.Remote_value.field v (Debugcom.Remote_value.obj i)
         | _ ->
             path event p
         end,
         Ctype.correct_levels valdesc.val_type)
      with Not_found ->
        raise(Error(Unbound_long_identifier lid))
      end
  | E_result ->
      begin match event with
        Some {ev_kind = Event_after ty; ev_typsubst = subst}
        when !Frames.current_frame = 0 ->
          (Debugcom.Remote_value.accu(), Subst.type_expr subst ty)
      | _ ->
          raise(Error(No_result))
      end
  | E_name n ->
      begin try
        Printval.find_named_value n
      with Not_found ->
        raise(Error(Unknown_name n))
      end
  | E_item(arg, n) ->
      let (v, ty) = expression event env arg in
      begin match (Ctype.repr(Ctype.expand_head_opt env ty)).desc with
        Ttuple ty_list ->
          if n < 1 || n > List.length ty_list
          then raise(Error(Tuple_index(ty, List.length ty_list, n)))
          else (Debugcom.Remote_value.field v (n-1), List.nth ty_list (n-1))
      | Tconstr(path, [ty_arg], _) when Path.same path Predef.path_array ->
          let size = Debugcom.Remote_value.size v in
          if n >= size
          then raise(Error(Array_index(size, n)))
          else (Debugcom.Remote_value.field v n, ty_arg)
      | Tconstr(path, [ty_arg], _) when Path.same path Predef.path_list ->
          let rec nth pos v =
            if not (Debugcom.Remote_value.is_block v) then
              raise(Error(List_index(pos, n)))
            else if pos = n then
              (Debugcom.Remote_value.field v 0, ty_arg)
            else
              nth (pos + 1) (Debugcom.Remote_value.field v 1)
          in nth 0 v
      | Tconstr(path, [], _) when Path.same path Predef.path_string ->
          let s = (Debugcom.Remote_value.obj v : string) in
          if n >= String.length s
          then raise(Error(String_index(s, String.length s, n)))
          else (Debugcom.Remote_value.of_int(Char.code s.[n]),
                Predef.type_char)
      | _ ->
          raise(Error(Wrong_item_type(ty, n)))
      end
  | E_field(arg, lbl) ->
      let (v, ty) = expression event env arg in
      begin match (Ctype.repr(Ctype.expand_head_opt env ty)).desc with
        Tconstr(path, args, _) ->
          let tydesc = Env.find_type path env in
          begin match tydesc.type_kind with
            Type_record(lbl_list, repr) ->
              let (pos, ty_res) =
                find_label lbl env ty path tydesc 0 lbl_list in
              (Debugcom.Remote_value.field v pos, ty_res)
          | _ -> raise(Error(Not_a_record ty))
          end
      | _ -> raise(Error(Not_a_record ty))
      end

and find_label lbl env ty path tydesc pos = function
    [] ->
      raise(Error(Wrong_label(ty, lbl)))
  | {ld_id; ld_type} :: rem ->
      if Ident.name ld_id = lbl then begin
        let ty_res =
          Btype.newgenty(Tconstr(path, tydesc.type_params, ref Mnil))
        in
        (pos,
         try Ctype.apply env [ty_res] ld_type [ty] with Ctype.Cannot_apply ->
           abstract_type)
      end else
        find_label lbl env ty path tydesc (pos + 1) rem

(* Error report *)

open Format

let report_error ppf = function
  | Unbound_identifier id ->
      fprintf ppf "@[Unbound identifier %s@]@." (Ident.name id)
  | Not_initialized_yet path ->
      fprintf ppf
        "@[The module path %a is not yet initialized.@ \
           Please run program forward@ \
           until its initialization code is executed.@]@."
      Printtyp.path path
  | Unbound_long_identifier lid ->
      fprintf ppf "@[Unbound identifier %a@]@." Printtyp.longident lid
  | Unknown_name n ->
      fprintf ppf "@[Unknown value name $%i@]@." n
  | Tuple_index(ty, len, pos) ->
      Printtyp.reset_and_mark_loops ty;
      fprintf ppf
        "@[Cannot extract field number %i from a %i-tuple of type@ %a@]@."
        pos len Printtyp.type_expr ty
  | Array_index(len, pos) ->
      fprintf ppf
        "@[Cannot extract element number %i from an array of length %i@]@."
        pos len
  | List_index(len, pos) ->
      fprintf ppf
        "@[Cannot extract element number %i from a list of length %i@]@."
        pos len
  | String_index(s, len, pos) ->
      fprintf ppf
        "@[Cannot extract character number %i@ \
           from the following string of length %i:@ %S@]@."
        pos len s
  | Wrong_item_type(ty, pos) ->
      fprintf ppf
        "@[Cannot extract item number %i from a value of type@ %a@]@."
        pos Printtyp.type_expr ty
  | Wrong_label(ty, lbl) ->
      fprintf ppf
        "@[The record type@ %a@ has no label named %s@]@."
        Printtyp.type_expr ty lbl
  | Not_a_record ty ->
      fprintf ppf
        "@[The type@ %a@ is not a record type@]@." Printtyp.type_expr ty
  | No_result ->
      fprintf ppf "@[No result available at current program event@]@."
