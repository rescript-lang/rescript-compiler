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
              if txt =  "null" then 
                (match opt_expr with 
                 | None -> true
                 | Some e -> 
                   Ast_payload.assert_bool_lit e), undefined

              else if txt = "undefined" then 
                null, 
                (match opt_expr with
                 | None ->  true
                 | Some e -> 
                   Ast_payload.assert_bool_lit e)

              else Bs_syntaxerr.err loc Unsupported_predicates
            ) (false, false) 
            (Ast_payload.record_as_config_and_process loc payload)  in 

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
            ) `Get (Ast_payload.record_as_config_and_process loc payload)  in 
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
  bs_deriving : [`Has_deriving of Ast_payload.action list | `Nothing ]
}

let process_derive_type attrs =
  List.fold_left 
    (fun (st, acc) 
      (({txt ; loc}, payload  as attr): attr)  ->
      match  st, txt  with
      |  {bs_deriving = `Nothing}, "bs.deriving"
        ->
        {st with
         bs_deriving = `Has_deriving 
             (Ast_payload.ident_or_record_as_config loc payload)}, acc 
      | {bs_deriving = `Has_deriving _}, "bs.deriving"
        -> 
        Bs_syntaxerr.err loc Duplicated_bs_deriving

      | _ , _ ->
        let st = 
          if txt = "nonrec" then 
            { st with explict_nonrec = true }
          else st in 
        st, attr::acc
    ) ( {explict_nonrec = false; bs_deriving = `Nothing }, []) attrs



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

let process_bs_string_as  attrs = 
  List.fold_left 
    (fun (st, attrs)
      (({txt ; loc}, payload ) as attr : attr)  ->
      match  txt, st  with
      | "bs.as", None
        ->
        begin match Ast_payload.is_single_string payload with 
          | None -> 
            Bs_syntaxerr.err loc Expect_string_literal
          | Some  (v,dec) ->  ( Some v, attrs)  
        end
      | "bs.as",  _ 
        -> 
        Bs_syntaxerr.err loc Duplicated_bs_as 
      | _ , _ -> (st, attr::attrs) 
    ) (None, []) attrs

let process_bs_int_as  attrs = 
  List.fold_left 
    (fun (st, attrs)
      (({txt ; loc}, payload ) as attr : attr)  ->
      match  txt, st  with
      | "bs.as", None
        ->
        begin match Ast_payload.is_single_int payload with 
          | None -> 
            Bs_syntaxerr.err loc Expect_int_literal
          | Some  _ as v->  (v, attrs)  
        end
      | "bs.as",  _ 
        -> 
        Bs_syntaxerr.err loc Duplicated_bs_as
      | _ , _ -> (st, attr::attrs) 
    ) (None, []) attrs

let process_bs_string_or_int_as attrs = 
  List.fold_left 
    (fun (st, attrs)
      (({txt ; loc}, payload ) as attr : attr)  ->
      match  txt, st  with
      | "bs.as", None
        ->
        begin match Ast_payload.is_single_int payload with 
          | None -> 
            begin match Ast_payload.is_single_string payload with 
              | Some (s,None) -> (Some (`Str (s)), attrs)
              | Some (s, Some "json") -> (Some (`Json_str s ), attrs)
              | None | Some (_, Some _) -> 
                Bs_syntaxerr.err loc Expect_int_or_string_or_json_literal

            end
          | Some   v->  (Some (`Int v), attrs)  
        end
      | "bs.as",  _ 
        -> 
        Bs_syntaxerr.err loc Duplicated_bs_as
      | _ , _ -> (st, attr::attrs) 
    ) (None, []) attrs

let bs : attr
  =  {txt = "bs" ; loc = Location.none}, Ast_payload.empty
let bs_this : attr
  =  {txt = "bs.this" ; loc = Location.none}, Ast_payload.empty

let bs_method : attr 
  =  {txt = "bs.meth"; loc = Location.none}, Ast_payload.empty


let warn_unused_attributes attrs = 
  if attrs <> [] then 
    List.iter (fun (({txt; loc}, _) : Parsetree.attribute) -> 
        Bs_warnings.warn_unused_attribute loc txt 
      ) attrs
