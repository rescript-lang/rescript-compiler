(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type attr =  Parsetree.attribute
type t =  attr list

type ('a,'b) st =
  { get : 'a option ;
    set : 'b option }


let process_method_attributes_rev (attrs : t) =
  List.fold_left (fun (st,acc) (({txt ; loc}, payload) as attr : attr) ->

      match txt  with
      | "bs.get" (* [@@bs.get{null; undefined}]*)
        ->
        let result =
          List.fold_left
            (fun
              (null, undefined)
              (({txt ; loc}, opt_expr) : Ast_payload.action) ->
              match txt with
              | "null" ->
                (match opt_expr with
                 | None -> true
                 | Some e ->
                   Ast_payload.assert_bool_lit e), undefined

              |  "undefined" ->
                null,
                (match opt_expr with
                 | None ->  true
                 | Some e ->
                   Ast_payload.assert_bool_lit e)
              | "nullable" ->
                begin match opt_expr with
                  | None -> true, true
                  | Some e ->
                    let v = Ast_payload.assert_bool_lit e in
                    v,v
                end
              | _ -> Bs_syntaxerr.err loc Unsupported_predicates
            ) (false, false)
            (Ast_payload.ident_or_record_as_config loc payload)  in

        ({st with get = Some result}, acc  )

      | "bs.set"
        ->
        let result =
          List.fold_left
            (fun st (({txt ; loc}, opt_expr) : Ast_payload.action) ->
               if txt =  "no_get" then
                 match opt_expr with
                 | None -> `No_get
                 | Some e ->
                   if Ast_payload.assert_bool_lit e then
                     `No_get
                   else `Get
               else Bs_syntaxerr.err loc Unsupported_predicates
            ) `Get (Ast_payload.ident_or_record_as_config loc payload)  in
        (* properties -- void
              [@@bs.set{only}]
        *)
        {st with set = Some result }, acc
      | _ ->
        (st, attr::acc  )
    ) ( {get = None ; set = None}, []) attrs


let process_attributes_rev (attrs : t) =
  List.fold_left (fun (st, acc) (({txt; loc}, _) as attr : attr) ->
      match txt, st  with
      | "bs", (`Nothing | `Uncurry)
        ->
        `Uncurry, acc
      | "bs.this", (`Nothing | `Meth_callback)
        ->  `Meth_callback, acc
      | "bs.meth",  (`Nothing | `Method)
        -> `Method, acc
      | "bs", _
      | "bs.this", _
        -> Bs_syntaxerr.err loc Conflict_bs_bs_this_bs_meth
      | _ , _ ->
        st, attr::acc
    ) ( `Nothing, []) attrs

let process_pexp_fun_attributes_rev (attrs : t) =
  List.fold_left (fun (st, acc) (({txt; loc}, _) as attr : attr) ->
      match txt, st  with
      | "bs.open", (`Nothing | `Exn)
        ->
        `Exn, acc

      | _ , _ ->
        st, attr::acc
    ) ( `Nothing, []) attrs

let process_bs attrs =
  List.fold_left (fun (st, acc) (({txt; loc}, _) as attr : attr) ->
      match txt, st  with
      | "bs", _
        ->
        `Has, acc
      | _ , _ ->
        st, attr::acc
    ) ( `Nothing, []) attrs

let process_external attrs =
  List.exists (fun (({txt; }, _)  : attr) ->
      if Ext_string.starts_with txt "bs." then true
      else false
    ) attrs


type derive_attr = {
  explict_nonrec : bool;
  bs_deriving : Ast_payload.action list option
}

let process_derive_type attrs : derive_attr * t =
  List.fold_left
    (fun (st, acc)
      (({txt ; loc}, payload  as attr): attr)  ->
      match  st, txt  with
      |  {bs_deriving = None}, "bs.deriving"
        ->
        {st with
         bs_deriving = Some
             (Ast_payload.ident_or_record_as_config loc payload)}, acc
      | {bs_deriving = Some _}, "bs.deriving"
        ->
        Bs_syntaxerr.err loc Duplicated_bs_deriving

      | _ , _ ->
        let st =
          if txt = "nonrec" then
            { st with explict_nonrec = true }
          else st in
        st, attr::acc
    ) ( {explict_nonrec = false; bs_deriving = None }, []) attrs

let iter_process_derive_type attrs =
  let st = ref {explict_nonrec = false; bs_deriving = None } in
  List.iter
    (fun
      (({txt ; loc}, payload  as attr): attr)  ->
      match  txt  with
      |  "bs.deriving"
        ->
        let ost = !st in
        (match ost with
         | {bs_deriving = None } ->
           Bs_ast_invariant.mark_used_bs_attribute attr ;
           st :=
             {ost with
              bs_deriving = Some
                  (Ast_payload.ident_or_record_as_config loc payload)}
         | {bs_deriving = Some _} ->
           Bs_syntaxerr.err loc Duplicated_bs_deriving)

      | "nonrec" ->
        st :=
          { !st with explict_nonrec = true }
      (* non bs attribute, no need to mark its use *)
      | _ -> ()
    )  attrs;
  !st


let process_bs_string_int_unwrap_uncurry attrs =
  List.fold_left
    (fun (st,attrs)
      (({txt ; loc}, (payload : _ ) ) as attr : attr)  ->
      match  txt, st  with
      | "bs.string", (`Nothing | `String)
        -> `String, attrs
      | "bs.int", (`Nothing | `Int)
        ->  `Int, attrs
      | "bs.ignore", (`Nothing | `Ignore)
        -> `Ignore, attrs
      | "bs.unwrap", (`Nothing | `Unwrap)
        -> `Unwrap, attrs
      | "bs.uncurry", `Nothing
        ->
        `Uncurry (Ast_payload.is_single_int payload), attrs
      (* Don't allow duplicated [bs.uncurry] since
         it may introduce inconsistency in arity
      *)
      | "bs.int", _
      | "bs.string", _
      | "bs.ignore", _
      | "bs.unwrap", _
        ->
        Bs_syntaxerr.err loc Conflict_attributes
      | _ , _ -> st, (attr :: attrs )
    ) (`Nothing, []) attrs


let iter_process_bs_string_as  (attrs : t) : string option =
  let st = ref None in
  List.iter
    (fun
      (({txt ; loc}, payload ) as attr : attr)  ->
      match  txt with
      | "bs.as"
        ->
        if !st = None then
          match Ast_payload.is_single_string payload with
          | None ->
            Bs_syntaxerr.err loc Expect_string_literal
          | Some  (v,_dec) ->
            Bs_ast_invariant.mark_used_bs_attribute attr ;
            st:= Some v
        else
          Bs_syntaxerr.err loc Duplicated_bs_as
      | _  -> ()
    ) attrs;
  !st

let iter_process_bs_int_as  attrs =
  let st = ref None in
  List.iter
    (fun
      (({txt ; loc}, payload ) as attr : attr)  ->
      match  txt with
      | "bs.as"
        ->
        if !st =  None then
          match Ast_payload.is_single_int payload with
          | None ->
            Bs_syntaxerr.err loc Expect_int_literal
          | Some  _ as v->
            Bs_ast_invariant.mark_used_bs_attribute attr ;
            st := v
        else
          Bs_syntaxerr.err loc Duplicated_bs_as
      | _  -> ()
    ) attrs; !st


let iter_process_bs_string_or_int_as attrs =
  let st = ref None in
  List.iter
    (fun
      (({txt ; loc}, payload ) as attr : attr)  ->
      match  txt with
      | "bs.as"
        ->
        if !st = None then
          (Bs_ast_invariant.mark_used_bs_attribute attr ;
           match Ast_payload.is_single_int payload with
           | None ->
             begin match Ast_payload.is_single_string payload with
               | Some (s,None) ->
                 st := Some (`Str (s))
               | Some (s, Some "json") ->
                 st := Some (`Json_str s )
               | None | Some (_, Some _) ->
                 Bs_syntaxerr.err loc Expect_int_or_string_or_json_literal

             end
           | Some   v->
             st := (Some (`Int v))
          )
        else
          Bs_syntaxerr.err loc Duplicated_bs_as
      | _ -> ()

    ) attrs;
  !st

let bs : attr
  =  {txt = "bs" ; loc = Location.none}, Ast_payload.empty

let is_bs (attr : attr) =
  match attr with
  | {Location.txt = "bs"; _}, _ -> true
  | _ -> false

let bs_this : attr
  =  {txt = "bs.this" ; loc = Location.none}, Ast_payload.empty

let bs_method : attr
  =  {txt = "bs.meth"; loc = Location.none}, Ast_payload.empty

let bs_obj : attr
  =  {txt = "bs.obj"; loc = Location.none}, Ast_payload.empty

let bs_get : attr
  =  {txt = "bs.get"; loc = Location.none}, Ast_payload.empty

let bs_set : attr
  =  {txt = "bs.set"; loc = Location.none}, Ast_payload.empty

