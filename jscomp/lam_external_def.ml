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



type external_module_name = 
  { bundle : string ; 
    bind_name : string option
  }
type 'a external_module = {
  txt : 'a ;
  external_module_name : external_module_name option;
}


type js_call = { 
  splice : bool ;
  qualifiers : string list;
  name : string;
}

type js_send = { 
  splice : bool ; 
  name : string 
} (* we know it is a js send, but what will happen if you pass an ocaml objct *)

type js_val = { 
  name : string ;
  external_module_name : external_module_name option;
  
} 

type js_new = {  name : string }
type js_set = { name : string }
type js_get = { name : string }

type ffi = 
  | Obj_create 
  | Js_global of js_val 
  | Js_global_as_var of  external_module_name
  | Js_call of js_call external_module
  | Js_send of js_send
  | Js_new of js_new external_module
  | Js_set of js_set
  | Js_get of js_get
  | Js_get_index
  | Js_set_index

type t  = 
  | Bs of Parsetree.core_type * ffi
  | Normal 
  (* When it's normal, it is handled as normal c functional ffi call *)


type prim = Types.type_expr option Primitive.description

let check_external_module_name ?loc x = 
  match x with 
  | {bundle = ""; _ } | {bind_name = Some ""} -> 
    Location.raise_errorf ?loc "empty name encountered"
  | _ -> ()
let check_external_module_name_opt ?loc x = 
  match x with 
  | None -> ()
  | Some v -> check_external_module_name ?loc v 


let check_ffi ?loc ffi = 
  match ffi with 
  | Js_global {name = ""} 
  | Js_send {name = ""}
  | Js_set {name = ""}
  | Js_get {name = ""}
    -> Location.raise_errorf ?loc "empty name encountered"
  | Js_global _ | Js_send _ | Js_set _ | Js_get _  
  | Obj_create 
  | Js_get_index | Js_set_index 
    -> ()

  | Js_global_as_var external_module_name 
    -> check_external_module_name external_module_name
  | Js_new {external_module_name ; txt = {name ; _}}
  | Js_call {external_module_name ; txt = {name ; _}}
    -> 
    check_external_module_name_opt ?loc external_module_name ; 
    if name = "" then
      Location.raise_errorf ?loc "empty name in externals"



(** 
   [@@bs.module "react"]
   [@@bs.module "react"]
   ---
   [@@bs.module "@" "react"]
   [@@bs.module "@" "react"]

   They should have the same module name 

   TODO: we should emit an warning if we bind 
   two external files to the same module name
*)



let handle_attributes ({prim_attributes ; prim_name} as _prim  : prim )
  : Location.t option * ffi  = 
  let qualifiers = ref [] in
  let call_name = ref None in
  let external_module_name  = ref None in
  let is_obj =  ref false in
  let js_val = ref `None in
  let js_val_of_module = ref `None in 
  let js_send = ref `None in
  let js_set = ref `None in
  let js_get = ref `None in
  let js_set_index = ref false in 
  let js_get_index = ref false in

  let js_splice = ref false in
  let start_loc : Location.t option ref = ref None in
  let finish_loc = ref None in
  let js_new = ref None in
  let () = 
    prim_attributes |> List.iter
      (fun ((( x : string Asttypes.loc ), pay_load) : Parsetree.attribute) -> 
         (if !start_loc = None  then 
            start_loc := Some x.loc 
         ); 
         (finish_loc := Some x.loc);
         match x.txt with  (* TODO: Check duplicate attributes *)
         | "bs.val"
           (* can be generalized into 
              {[
                [@@bs.val]
              ]}
              and combined with 
              {[
                [@@bs.value] [@@bs.module]
              ]}
           *)
           -> 
           begin  match Ast_payload.is_single_string pay_load with
             | Some name -> 
               js_val := `Value name 
             | None -> 
               js_val := `Value prim_name
               (* we can report error here ... *)
           end
         | "bs.val_of_module" 
           (* {[ [@@bs.val_of_module]]}
           *)
           -> 
           js_val_of_module := 
             `Value ({bundle = prim_name ; bind_name = Ast_payload.is_single_string pay_load})
         |"bs.splice"
           -> 
           js_splice := true

         |"bs.send" 
           ->
           begin match Ast_payload.is_single_string pay_load with 
             | Some name -> js_send := `Value name
             | None -> js_send := `Value prim_name
           end
         | "bs.set"
           ->
           begin match Ast_payload.is_single_string pay_load with
             | Some name -> js_set := `Value name
             | None -> js_set := `Value prim_name
           end
         | "bs.get"
           ->
           begin match Ast_payload.is_single_string pay_load with
             | Some name -> js_get := `Value name
             | None -> js_get := `Value prim_name
           end

         | "bs.call"
           (*TODO: check duplicate attributes, at least we should give a warning
             [@@bs.call "xx"] [@@bs.call]
           *)
           ->
           begin match Ast_payload.is_single_string pay_load with 
             | Some name -> call_name :=  Some (x.loc, name)
             | None -> call_name := Some(x.loc, prim_name)
           end
         | "bs.module" -> 
           begin match Ast_payload.is_string_or_strings pay_load with 
             | `Single name ->
               external_module_name:= Some ({ bundle =  name; bind_name = None})
             | `Some [bundle;bind_name] -> 
               external_module_name := 
                 Some ({bundle ; bind_name = Some bind_name})
             | `Some _ -> ()
             | `None -> () (* should emit a warning instead *)
           end

         | "bs.new" -> 
           begin match Ast_payload.is_single_string pay_load with 
             | Some x -> js_new := Some x 
             | None -> js_new := Some prim_name
           end
         | "bs.set_index" 
           -> js_set_index := true
         | "bs.get_index"
           -> js_get_index := true
         |"bs.obj"
           -> 
           is_obj := true
         | _ ->  () (* ignore *)
      ) in
  let loc : Location.t option  = 
    match !start_loc, !finish_loc  with
    | None, None -> None 
    | Some {loc_start;_}, Some{loc_end; _} -> Some {loc_start; loc_end; loc_ghost = false}
    | _ -> assert false in
  loc, 
  if !is_obj then Obj_create 
  else if !js_get_index then
    Js_get_index
  else if !js_set_index then 
    Js_set_index
  else 
    begin match !js_val_of_module with 
      | `Value v -> Js_global_as_var v 
      | `None -> 
        begin match !call_name, !js_val, !js_send, !js_new, !js_set, !js_get  with 
          | Some (_,fn),
            `None, `None, _, `None, `None -> 
            Js_call { txt = { splice = !js_splice; qualifiers = !qualifiers; name = fn};
                      external_module_name = !external_module_name}
          | None, `Value name, `None ,_, `None, `None  ->  
            Js_global {name = name; external_module_name = !external_module_name}
          | None, `None, `Value name, _, `None, `None   -> 
            Js_send {splice = !js_splice; name }
          | None, `None, `None, Some name, `None, `None  -> 
            Js_new { txt = { name  };
                     external_module_name = ! external_module_name}
          |  None, `None, `None, None, `Value name, `None 
            -> Js_set { name}
          |  None, `None, `None, None, `None,  `Value name
            -> Js_get {name} (* TODO, we should also have index *)
          |  None, `None, `None, None, `None, `None -> Normal 
          | _ -> 
            Location.raise_errorf ?loc "Ill defined attribute"
        end
    end
(* Given label, type and the argument --> encode it into 
   javascript meaningful value 
   -- check whether splice or not for the last element
*)
    (*
      special treatment to None for [bs.call] as well
      None --> null or undefined 
      Some -> original value
      unit -->
     *)
