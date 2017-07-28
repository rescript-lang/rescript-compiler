(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(* Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt*)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* To print values *)

open Misc
open Format
open Longident
open Path
open Types
open Outcometree

module type OBJ =
  sig
    type t
    val obj : t -> 'a
    val is_block : t -> bool
    val tag : t -> int
    val size : t -> int
    val field : t -> int -> t
  end

module type EVALPATH =
  sig
    type valu
    val eval_path: Env.t -> Path.t -> valu
    exception Error
    val same_value: valu -> valu -> bool
  end

type ('a, 'b) gen_printer =
  | Zero of 'b
  | Succ of ('a -> ('a, 'b) gen_printer)

module type S =
  sig
    type t
    val install_printer :
          Path.t -> Types.type_expr -> (formatter -> t -> unit) -> unit
    val install_generic_printer :
           Path.t -> Path.t ->
           (int -> (int -> t -> Outcometree.out_value,
                    t -> Outcometree.out_value) gen_printer) ->
           unit
    val install_generic_printer' :
           Path.t -> Path.t ->
           (formatter -> t -> unit,
            formatter -> t -> unit) gen_printer ->
           unit
    val remove_printer : Path.t -> unit
    val outval_of_untyped_exception : t -> Outcometree.out_value
    val outval_of_value :
          int -> int ->
          (int -> t -> Types.type_expr -> Outcometree.out_value option) ->
          Env.t -> t -> type_expr -> Outcometree.out_value
  end

module Make(O : OBJ)(EVP : EVALPATH with type valu = O.t) = struct

    type t = O.t

    module ObjTbl = Hashtbl.Make(struct
        type t = O.t
        let equal = (==)
        let hash x =
          try
            Hashtbl.hash x
          with exn -> 0
      end)


    (* Given an exception value, we cannot recover its type,
       hence we cannot print its arguments in general.
       Here, we do a feeble attempt to print
       integer, string and float arguments... *)
    let outval_of_untyped_exception_args obj start_offset =
      if O.size obj > start_offset then begin
        let list = ref [] in
        for i = start_offset to O.size obj - 1 do
          let arg = O.field obj i in
          if not (O.is_block arg) then
            list := Oval_int (O.obj arg : int) :: !list
               (* Note: this could be a char or a constant constructor... *)
          else if O.tag arg = Obj.string_tag then
            list :=
              Oval_string (String.escaped (O.obj arg : string)) :: !list
          else if O.tag arg = Obj.double_tag then
            list := Oval_float (O.obj arg : float) :: !list
          else
            list := Oval_constr (Oide_ident "_", []) :: !list
        done;
        List.rev !list
      end
      else []

    let outval_of_untyped_exception bucket =
      if O.tag bucket <> 0 then
        Oval_constr (Oide_ident (O.obj (O.field bucket 0) : string), [])
      else
      let name = (O.obj(O.field(O.field bucket 0) 0) : string) in
      let args =
        if (name = "Match_failure"
            || name = "Assert_failure"
            || name = "Undefined_recursive_module")
        && O.size bucket = 2
        && O.tag(O.field bucket 1) = 0
        then outval_of_untyped_exception_args (O.field bucket 1) 0
        else outval_of_untyped_exception_args bucket 1 in
      Oval_constr (Oide_ident name, args)

    (* The user-defined printers. Also used for some builtin types. *)

    type printer =
      | Simple of Types.type_expr * (O.t -> Outcometree.out_value)
      | Generic of Path.t * (int -> (int -> O.t -> Outcometree.out_value,
                                     O.t -> Outcometree.out_value) gen_printer)

    let printers = ref ([
      ( Pident(Ident.create "print_int"),
        Simple (Predef.type_int,
                (fun x -> Oval_int (O.obj x : int))) );
      ( Pident(Ident.create "print_float"),
        Simple (Predef.type_float,
                (fun x -> Oval_float (O.obj x : float))) );
      ( Pident(Ident.create "print_char"),
        Simple (Predef.type_char,
                (fun x -> Oval_char (O.obj x : char))) );
      ( Pident(Ident.create "print_string"),
        Simple (Predef.type_string,
                (fun x -> Oval_string (O.obj x : string))) );
      ( Pident(Ident.create "print_int32"),
        Simple (Predef.type_int32,
                (fun x -> Oval_int32 (O.obj x : int32))) );
      ( Pident(Ident.create "print_nativeint"),
        Simple (Predef.type_nativeint,
                (fun x -> Oval_nativeint (O.obj x : nativeint))) );
      ( Pident(Ident.create "print_int64"),
        Simple (Predef.type_int64,
                (fun x -> Oval_int64 (O.obj x : int64)) ))
    ] : (Path.t * printer) list)

    let exn_printer ppf path =
      fprintf ppf "<printer %a raised an exception>" Printtyp.path path

    let out_exn path =
      Oval_printer (fun ppf -> exn_printer ppf path)

    let install_printer path ty fn =
      let print_val ppf obj =
        try fn ppf obj with exn -> exn_printer ppf path in
      let printer obj = Oval_printer (fun ppf -> print_val ppf obj) in
      printers := (path, Simple (ty, printer)) :: !printers

    let install_generic_printer function_path constr_path fn =
      printers := (function_path, Generic (constr_path, fn))  :: !printers

    let install_generic_printer' function_path ty_path fn =
      let rec build gp depth =
        match gp with
        | Zero fn ->
            let out_printer obj =
              let printer ppf =
                try fn ppf obj with _ -> exn_printer ppf function_path in
              Oval_printer printer in
            Zero out_printer
        | Succ fn ->
            let print_val fn_arg =
              let print_arg ppf o =
                !Oprint.out_value ppf (fn_arg (depth+1) o) in
              build (fn print_arg) depth in
            Succ print_val in
      printers := (function_path, Generic (ty_path, build fn)) :: !printers

    let remove_printer path =
      let rec remove = function
      | [] -> raise Not_found
      | ((p, _) as printer) :: rem ->
          if Path.same p path then rem else printer :: remove rem in
      printers := remove !printers

    (* Print a constructor or label, giving it the same prefix as the type
       it comes from. Attempt to omit the prefix if the type comes from
       a module that has been opened. *)

    let tree_of_qualified lookup_fun env ty_path name =
      match ty_path with
      | Pident id ->
          Oide_ident name
      | Pdot(p, s, pos) ->
          if try
               match (lookup_fun (Lident name) env).desc with
               | Tconstr(ty_path', _, _) -> Path.same ty_path ty_path'
               | _ -> false
             with Not_found -> false
          then Oide_ident name
          else Oide_dot (Printtyp.tree_of_path p, name)
      | Papply(p1, p2) ->
          Printtyp.tree_of_path ty_path

    let tree_of_constr =
      tree_of_qualified
        (fun lid env -> (Env.lookup_constructor lid env).cstr_res)

    and tree_of_label =
      tree_of_qualified (fun lid env -> (Env.lookup_label lid env).lbl_res)

    (* An abstract type *)

    let abstract_type =
      Ctype.newty (Tconstr (Pident (Ident.create "abstract"), [], ref Mnil))

    (* The main printing function *)

    let outval_of_value max_steps max_depth check_depth env obj ty =

      let printer_steps = ref max_steps in

      let nested_values = ObjTbl.create 8 in
      let nest_gen err f depth obj ty =
        let repr = obj in
        if not (O.is_block repr) then
          f depth obj ty
        else
          if ObjTbl.mem nested_values repr then
            err
          else begin
            ObjTbl.add nested_values repr ();
            let ret = f depth obj ty in
            ObjTbl.remove nested_values repr;
            ret
          end
      in

      let nest f = nest_gen (Oval_stuff "<cycle>") f in

      let rec tree_of_val depth obj ty =
        decr printer_steps;
        if !printer_steps < 0 || depth < 0 then Oval_ellipsis
        else begin
        try
          find_printer depth env ty obj
        with Not_found ->
          match (Ctype.repr ty).desc with
          | Tvar _ | Tunivar _ ->
              Oval_stuff "<poly>"
          | Tarrow(_, ty1, ty2, _) ->
              Oval_stuff "<fun>"
          | Ttuple(ty_list) ->
              Oval_tuple (tree_of_val_list 0 depth obj ty_list)
          | Tconstr(path, [ty_arg], _)
            when Path.same path Predef.path_list ->
              if O.is_block obj then
                match check_depth depth obj ty with
                  Some x -> x
                | None ->
                    let rec tree_of_conses tree_list depth obj ty_arg =
                      if !printer_steps < 0 || depth < 0 then
                        Oval_ellipsis :: tree_list
                      else if O.is_block obj then
                        let tree =
                          nest tree_of_val (depth - 1) (O.field obj 0) ty_arg
                        in
                        let next_obj = O.field obj 1 in
                        nest_gen (Oval_stuff "<cycle>" :: tree :: tree_list)
                          (tree_of_conses (tree :: tree_list))
                          depth next_obj ty_arg
                      else tree_list
                    in
                    Oval_list (List.rev (tree_of_conses [] depth obj ty_arg))
              else
                Oval_list []
          | Tconstr(path, [ty_arg], _)
            when Path.same path Predef.path_array ->
              let length = O.size obj in
              if length > 0 then
                match check_depth depth obj ty with
                  Some x -> x
                | None ->
                    let rec tree_of_items tree_list i =
                      if !printer_steps < 0 || depth < 0 then
                        Oval_ellipsis :: tree_list
                      else if i < length then
                        let tree =
                          nest tree_of_val (depth - 1) (O.field obj i) ty_arg
                        in
                        tree_of_items (tree :: tree_list) (i + 1)
                      else tree_list
                    in
                    Oval_array (List.rev (tree_of_items [] 0))
              else
                Oval_array []
          | Tconstr (path, [ty_arg], _)
            when Path.same path Predef.path_lazy_t ->
             let obj_tag = O.tag obj in
             (* Lazy values are represented in three possible ways:

                1. a lazy thunk that is not yet forced has tag
                   Obj.lazy_tag

                2. a lazy thunk that has just been forced has tag
                   Obj.forward_tag; its first field is the forced
                   result, which we can print

                3. when the GC moves a forced trunk with forward_tag,
                   or when a thunk is directly created from a value,
                   we get a third representation where the value is
                   directly exposed, without the Obj.forward_tag
                   (if its own tag is not ambiguous, that is neither
                   lazy_tag nor forward_tag)

                Note that using Lazy.is_val and Lazy.force would be
                unsafe, because they use the Obj.* functions rather
                than the O.* functions of the functor argument, and
                would thus crash if called from the toplevel
                (debugger/printval instantiates Genprintval.Make with
                an Obj module talking over a socket).
              *)
             if obj_tag = Obj.lazy_tag then Oval_stuff "<lazy>"
             else begin
                 let forced_obj =
                   if obj_tag = Obj.forward_tag then O.field obj 0 else obj
                 in
                 (* calling oneself recursively on forced_obj risks
                    having a false positive for cycle detection;
                    indeed, in case (3) above, the value is stored
                    as-is instead of being wrapped in a forward
                    pointer. It means that, for (lazy "foo"), we have
                      forced_obj == obj
                    and it is easy to wrongly print (lazy <cycle>) in such
                    a case (PR#6669).

                    Unfortunately, there is a corner-case that *is*
                    a real cycle: using -rectypes one can define
                      let rec x = lazy x
                    which creates a Forward_tagged block that points to
                    itself. For this reason, we still "nest"
                    (detect head cycles) on forward tags.
                  *)
                 let v =
                   if obj_tag = Obj.forward_tag
                   then nest tree_of_val depth forced_obj ty_arg
                   else      tree_of_val depth forced_obj ty_arg
                 in
                 Oval_constr (Oide_ident "lazy", [v])
               end
          | Tconstr(path, ty_list, _) -> begin
              try
                let decl = Env.find_type path env in
                match decl with
                | {type_kind = Type_abstract; type_manifest = None} ->
                    Oval_stuff "<abstr>"
                | {type_kind = Type_abstract; type_manifest = Some body} ->
                    tree_of_val depth obj
                      (try Ctype.apply env decl.type_params body ty_list with
                         Ctype.Cannot_apply -> abstract_type)
                | {type_kind = Type_variant constr_list} ->
                    let tag =
                      if O.is_block obj
                      then Cstr_block(O.tag obj)
                      else Cstr_constant(O.obj obj) in
                    let {cd_id;cd_args;cd_res} =
                      Datarepr.find_constr_by_tag tag constr_list in
                    let type_params =
                      match cd_res with
                        Some t ->
                          begin match (Ctype.repr t).desc with
                            Tconstr (_,params,_) ->
                              params
                          | _ -> assert false end
                      | None -> decl.type_params
                    in
                    let ty_args =
                      List.map
                        (function ty ->
                           try Ctype.apply env type_params ty ty_list with
                             Ctype.Cannot_apply -> abstract_type)
                        cd_args in
                    tree_of_constr_with_args (tree_of_constr env path)
                                 (Ident.name cd_id) 0 depth obj ty_args
                | {type_kind = Type_record(lbl_list, rep)} ->
                    begin match check_depth depth obj ty with
                      Some x -> x
                    | None ->
                        let rec tree_of_fields pos = function
                          | [] -> []
                          | {ld_id; ld_type} :: remainder ->
                              let ty_arg =
                                try
                                  Ctype.apply env decl.type_params ld_type
                                    ty_list
                                with
                                  Ctype.Cannot_apply -> abstract_type in
                              let name = Ident.name ld_id in
                              (* PR#5722: print full module path only
                                 for first record field *)
                              let lid =
                                if pos = 0 then tree_of_label env path name
                                else Oide_ident name
                              and v =
                                nest tree_of_val (depth - 1) (O.field obj pos)
                                  ty_arg
                              in
                              (lid, v) :: tree_of_fields (pos + 1) remainder
                        in
                        Oval_record (tree_of_fields 0 lbl_list)
                    end
                | {type_kind = Type_open} ->
                    tree_of_extension path depth obj
              with
                Not_found ->                (* raised by Env.find_type *)
                  Oval_stuff "<abstr>"
              | Datarepr.Constr_not_found -> (* raised by find_constr_by_tag *)
                  Oval_stuff "<unknown constructor>"
              end
          | Tvariant row ->
              let row = Btype.row_repr row in
              if O.is_block obj then
                let tag : int = O.obj (O.field obj 0) in
                let rec find = function
                  | (l, f) :: fields ->
                      if Btype.hash_variant l = tag then
                        match Btype.row_field_repr f with
                        | Rpresent(Some ty) | Reither(_,[ty],_,_) ->
                            let args =
                              nest tree_of_val (depth - 1) (O.field obj 1) ty
                            in
                              Oval_variant (l, Some args)
                        | _ -> find fields
                      else find fields
                  | [] -> Oval_stuff "<variant>" in
                find row.row_fields
              else
                let tag : int = O.obj obj in
                let rec find = function
                  | (l, _) :: fields ->
                      if Btype.hash_variant l = tag then
                        Oval_variant (l, None)
                      else find fields
                  | [] -> Oval_stuff "<variant>" in
                find row.row_fields
          | Tobject (_, _) ->
              Oval_stuff "<obj>"
          | Tsubst ty ->
              tree_of_val (depth - 1) obj ty
          | Tfield(_, _, _, _) | Tnil | Tlink _ ->
              fatal_error "Printval.outval_of_value"
          | Tpoly (ty, _) ->
              tree_of_val (depth - 1) obj ty
          | Tpackage _ ->
              Oval_stuff "<module>"
        end

      and tree_of_val_list start depth obj ty_list =
        let rec tree_list i = function
          | [] -> []
          | ty :: ty_list ->
              let tree = nest tree_of_val (depth - 1) (O.field obj i) ty in
              tree :: tree_list (i + 1) ty_list in
      tree_list start ty_list

      and tree_of_constr_with_args
             tree_of_cstr cstr_name start depth obj ty_args =
        let lid = tree_of_cstr cstr_name in
        let args = tree_of_val_list start depth obj ty_args in
        Oval_constr (lid, args)

    and tree_of_extension type_path depth bucket =
      let slot =
        if O.tag bucket <> 0 then bucket
        else O.field bucket 0
      in
      let name = (O.obj(O.field slot 0) : string) in
      let lid = Longident.parse name in
      try
        (* Attempt to recover the constructor description for the exn
           from its name *)
        let cstr = Env.lookup_constructor lid env in
        let path =
          match cstr.cstr_tag with
            Cstr_extension(p, _) -> p
            | _ -> raise Not_found
        in
        (* Make sure this is the right exception and not an homonym,
           by evaluating the exception found and comparing with the
           identifier contained in the exception bucket *)
        if not (EVP.same_value slot (EVP.eval_path env path))
        then raise Not_found;
        tree_of_constr_with_args
           (fun x -> Oide_ident x) name 1 depth bucket cstr.cstr_args
      with Not_found | EVP.Error ->
        match check_depth depth bucket ty with
          Some x -> x
        | None when Path.same type_path Predef.path_exn->
            outval_of_untyped_exception bucket
        | None ->
            Oval_stuff "<extension>"

    and find_printer depth env ty =
      let rec find = function
      | [] -> raise Not_found
      | (name, Simple (sch, printer)) :: remainder ->
          if Ctype.moregeneral env false sch ty
          then printer
          else find remainder
      | (name, Generic (path, fn)) :: remainder ->
          begin match (Ctype.expand_head env ty).desc with
          | Tconstr (p, args, _) when Path.same p path ->
              begin try apply_generic_printer path (fn depth) args
              with _ -> (fun obj -> out_exn path) end
          | _ -> find remainder end in
      find !printers

    and apply_generic_printer path printer args =
      match (printer, args) with
      | (Zero fn, []) -> (fun (obj : O.t)-> try fn obj with _ -> out_exn path)
      | (Succ fn, arg :: args) ->
          let printer = fn (fun depth obj -> tree_of_val depth obj arg) in
          apply_generic_printer path printer args
      | _ ->
          (fun obj ->
            let printer ppf =
              fprintf ppf "<internal error: incorrect arity for '%a'>"
                Printtyp.path path in
            Oval_printer printer)


    in nest tree_of_val max_depth obj ty

end
