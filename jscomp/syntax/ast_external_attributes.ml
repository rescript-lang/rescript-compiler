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

type pipe = bool 
type js_call = { 
  splice : bool ;
  name : string;
}

type js_send = { 
  splice : bool ; 
  name : string ;
  pipe : bool   
} (* we know it is a js send, but what will happen if you pass an ocaml objct *)

type js_val = string external_module 



type arg_type = Ast_core_type.arg_type
type arg_label = Ast_core_type.arg_label

type arg_kind = 
  {
    arg_type : arg_type;
    arg_label : arg_label
  }

type ffi = 
  | Obj_create of arg_label list
  | Js_global of js_val 
  | Js_module_as_var of  external_module_name
  | Js_module_as_fn of external_module_name
  | Js_module_as_class of external_module_name             
  | Js_call of js_call external_module
  | Js_send of js_send
    (* Note how we encode it will have a semantic difference 
    *)
  | Js_new of js_val
  | Js_set of string
  | Js_get of string
  | Js_get_index
  | Js_set_index



let valid_js_char =
  let a = Array.init 256 (fun i ->
    let c = Char.chr i in
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' || c = '$'
  ) in
  (fun c -> Array.unsafe_get a (Char.code c))

let valid_first_js_char = 
  let a = Array.init 256 (fun i ->
    let c = Char.chr i in
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || c = '$'
  ) in
  (fun c -> Array.unsafe_get a (Char.code c))

(** Approximation could be improved *)
let valid_ident (s : string) =
  let len = String.length s in
  len > 0 && valid_js_char s.[0] && valid_first_js_char s.[0] &&
  (let module E = struct exception E end in
   try
     for i = 1 to len - 1 do
       if not (valid_js_char (String.unsafe_get s i)) then
         raise E.E         
     done ;
     true     
   with E.E -> false )  
  
let valid_global_name ?loc txt =
  if not (valid_ident txt) then
    let v = Ext_string.split_by ~keep_empty:true (fun x -> x = '.') txt in
    List.iter
      (fun s ->
         if not (valid_ident s) then
           Location.raise_errorf ?loc "Not a valid  name %s"  txt
      ) v      

let valid_method_name ?loc txt =         
  if not (valid_ident txt) then
    Location.raise_errorf ?loc "Not a valid  name %s"  txt



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
  | Js_global {txt} -> valid_global_name ?loc  txt 
  | Js_send {name } 
  | Js_set  name
  | Js_get name
    ->  valid_method_name ?loc name
  | Obj_create _ -> ()
  | Js_get_index | Js_set_index 
    -> ()

  | Js_module_as_var external_module_name
  | Js_module_as_fn external_module_name
  | Js_module_as_class external_module_name             
    -> check_external_module_name external_module_name
  | Js_new {external_module_name ; txt = name}
  | Js_call {external_module_name ; txt = {name ; _}}
    -> 
    check_external_module_name_opt ?loc external_module_name ;
    valid_global_name ?loc name     


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
type bundle_source =
  [`Nm_payload of string
  |`Nm_external of string
  | `Nm_val of string      
  ]  

let string_of_bundle_source (x : bundle_source) =
  match x with
  | `Nm_payload x
  | `Nm_external x
  | `Nm_val x -> x   
type name_source =
  [ bundle_source  
  | `Nm_na

  ]
type st = 
  { val_name : name_source;
    external_module_name : external_module_name option;
    module_as_val : external_module_name option;
    val_send : name_source ;
    val_send_pipe : [`Nm_na | `Type of Ast_core_type.t ];    
    splice : bool ; (* mutable *)
    set_index : bool; (* mutable *)
    get_index : bool;
    new_name : name_source ;
    call_name : name_source ;
    set_name : name_source ;
    get_name : name_source ;
    mk_obj : bool ;

  }

let init_st = 
  {
    val_name = `Nm_na; 
    external_module_name = None ;
    module_as_val = None;
    val_send = `Nm_na;
    val_send_pipe = `Nm_na;    
    splice = false;
    set_index = false;
    get_index = false;
    new_name = `Nm_na;
    call_name = `Nm_na;
    set_name = `Nm_na ;
    get_name = `Nm_na ;
    mk_obj = false ; 

  }

type t  = 
  | Bs of arg_kind list  * arg_type * ffi
  | Normal 
  (* When it's normal, it is handled as normal c functional ffi call *)

let bs_external = "BS_EXTERN:" ^ Js_config.version

let bs_external_length = (String.length bs_external)

let is_bs_external_prefix s = 
  Ext_string.starts_with s bs_external

let to_string  t = 
  bs_external ^ Marshal.to_string t []
let unsafe_from_string s = 
    Marshal.from_string  s bs_external_length 
let from_string s : t  = 
  if is_bs_external_prefix s then 
    Marshal.from_string  s (String.length bs_external)
  else Ext_pervasives.failwithf ~loc:__LOC__
      "compiler version mismatch, please do a clean build" 


let handle_attributes 
    (loc : Bs_loc.t)
    (pval_prim : string ) 
    (type_annotation : Parsetree.core_type)
    (prim_attributes : Ast_attributes.t) (prim_name : string)
  : Ast_core_type.t * string * t =
  let prim_name_or_pval_prim =
    if String.length prim_name = 0 then  `Nm_val pval_prim
    else  `Nm_external prim_name  (* need check name *)
  in    
  let name_from_payload_or_prim payload = 
    match Ast_payload.is_single_string payload with 
    | Some  val_name ->  `Nm_payload val_name
    | None ->  prim_name_or_pval_prim
    in 
    let result_type_ty, arg_types_ty =
      Ast_core_type.list_of_arrow type_annotation in
    let st = 
      List.fold_left 
        (fun st
          (({txt ; loc}, payload) : Ast_attributes.attr) 
          ->
            (* can be generalized into 
               {[
                 [@@bs.val]
               ]}
               and combined with 
               {[
                 [@@bs.value] [@@bs.module]
               ]}
            *)

            begin match txt with 
              | "bs.val" ->  
                begin match arg_types_ty with 
                | [] -> 
                  {st with val_name = name_from_payload_or_prim payload}
                | _ -> 
                  {st with call_name = name_from_payload_or_prim payload}
                end
              | "bs.module" -> 
                begin match Ast_payload.assert_strings loc payload with 
                  | [name] ->
                    {st with external_module_name =
                               Some {bundle=name; bind_name = None}}
                  | [bundle;bind_name] -> 
                    {st with external_module_name =
                               Some {bundle; bind_name = Some bind_name}}
                  | [] ->
                    { st with
                      module_as_val = 
                        Some
                          { bundle =
                              string_of_bundle_source
                                (prim_name_or_pval_prim :> bundle_source) ;
                            bind_name = Some pval_prim}
                    }
                  | _  -> Location.raise_errorf ~loc "Illegal attributes"
                end
              | "bs.splice" -> {st with splice = true}
              | "bs.send" -> 
                { st with val_send = name_from_payload_or_prim payload}
              | "bs.send.pipe"
                ->
                { st with val_send_pipe = `Type (Ast_payload.as_core_type loc payload)}                
              | "bs.set" -> 
                {st with set_name = name_from_payload_or_prim payload}
              | "bs.get" -> {st with get_name = name_from_payload_or_prim payload}

              | "bs.new" -> {st with new_name = name_from_payload_or_prim payload}
              | "bs.set_index" -> {st with set_index = true}
              | "bs.get_index"-> {st with get_index = true}
              | "bs.obj" -> {st with mk_obj = true}
              | "bs.type"
              | _ -> st (* TODO: warning*)
            end
        )
         init_st prim_attributes in 

    let aux ty : arg_type = 
      if Ast_core_type.is_array ty then Array
      else if Ast_core_type.is_unit ty then Unit
      else (Ast_core_type.string_type ty :> arg_type) in
    let translate_arg_type =
      (fun (label, ty) -> 
         { arg_label = Ast_core_type.label_name label ;
           arg_type =  aux ty 
         }) in      
    let arg_types = 
      List.map translate_arg_type arg_types_ty in
    let result_type = aux result_type_ty in
    let object_type = ref None in     
    let ffi = 
      match st with 
      | {mk_obj = true;

         val_name = `Nm_na; 
         external_module_name = None ;
         module_as_val = None;
         val_send = `Nm_na;
         val_send_pipe = `Nm_na;    
         splice = false;
         new_name = `Nm_na;
         call_name = `Nm_na;
         set_name = `Nm_na ;
         get_name = `Nm_na ;
         get_index = false ;
        } -> 
        let labels = List.map (function
          | {arg_type = Unit ; arg_label = (Empty as l)}
            -> l 
          | {arg_label = Label name } -> 
            Label (Lam_methname.translate ~loc name)            
          | {arg_label = Optional name} 
            -> Optional (Lam_methname.translate ~loc name)
          | _ -> Location.raise_errorf ~loc "expect label, optional, or unit here" )
          arg_types in
        if String.length prim_name <> 0 then 
          Location.raise_errorf ~loc "[@@bs.obj] expect external names to be empty string";
        Obj_create labels(* Need fetch label here, for better error message *)
      | {mk_obj = true; _}
        ->
        Location.raise_errorf ~loc "conflict attributes found"                
      | {set_index = true;

         val_name = `Nm_na; 
         external_module_name = None ;
         module_as_val = None;
         val_send = `Nm_na;
         val_send_pipe = `Nm_na;    
         splice = false;
         get_index = false;
         new_name = `Nm_na;
         call_name = `Nm_na;
         set_name = `Nm_na ;
         get_name = `Nm_na ;
         mk_obj = false ; 

        } 
        ->
        if String.length prim_name <> 0 then 
          Location.raise_errorf ~loc "[@@bs.set_index] expect external names to be empty string";
        begin match arg_types with 
        | [_obj; _v ; _value] 
          -> 
          Js_set_index
        | _ -> Location.raise_errorf ~loc "Ill defined attribute [@@bs.set_index](arity of 3)"
        end
      | {set_index = true; _}
        ->
        Location.raise_errorf ~loc "conflict attributes found"        
        
      | {get_index = true;

         val_name = `Nm_na; 
         external_module_name = None ;
         module_as_val = None;
         val_send = `Nm_na;
         val_send_pipe = `Nm_na;    
         
         splice = false;
         new_name = `Nm_na;
         call_name = `Nm_na;
         set_name = `Nm_na ;
         get_name = `Nm_na ;
         mk_obj = false ; 
        } ->
        if String.length prim_name <> 0 then 
          Location.raise_errorf ~loc "[@@bs.get_index] expect external names to be empty string";
        begin match arg_types with 
        | [_obj; _v ] -> 
          Js_get_index
        | _ -> Location.raise_errorf ~loc "Ill defined attribute [@@bs.get_index] (arity of 2)"
        end
      | {get_index = true; _}
        -> Location.raise_errorf ~loc "conflict attributes found"        
      | {module_as_val = Some v ;

         get_index = false;
         val_name ;
         new_name ;
         (*TODO: a better way to avoid breaking existing code,
           we need tell the difference from 
           {[
             1. [@@bs.val "x"]
             2. external x : .. "x" [@@bs.val ]
             3. external x : .. ""  [@@bs.val]
           ]}
         *)         
         external_module_name = None ;
         val_send = `Nm_na;
         val_send_pipe = `Nm_na;    
         
         splice = false;
         call_name = `Nm_na;
         set_name = `Nm_na ;
         get_name = `Nm_na ;
         mk_obj = false ;          
        } ->
        begin match arg_types_ty, new_name, val_name  with         
          | [], `Nm_na,  _ -> Js_module_as_var v
          | _, `Nm_na, _ -> Js_module_as_fn v
          | _, #bundle_source, #bundle_source ->
            Location.raise_errorf ~loc "conflict attributes found"
          | _, (`Nm_val _ | `Nm_external _) , `Nm_na
            -> Js_module_as_class v
          | _, `Nm_payload _ , `Nm_na
            ->
            Location.raise_errorf ~loc
              "conflict attributes found: (bs.new should not carry payload here)"
                 
        end
      | {module_as_val = Some _}
        -> Location.raise_errorf ~loc "conflict attributes found" 
      | {call_name = (`Nm_val name | `Nm_external name | `Nm_payload name) ;
         splice; 
         external_module_name;

         val_name = `Nm_na ;
         module_as_val = None;
         val_send = `Nm_na ;
         val_send_pipe = `Nm_na;    
         
         set_index = false;
         get_index = false;
         new_name = `Nm_na;
         set_name = `Nm_na ;
         get_name = `Nm_na 
        } -> 
        Js_call {txt = {splice; name}; external_module_name}
      | {call_name = #bundle_source } 
        -> Location.raise_errorf ~loc "conflict attributes found"

      | {val_name = (`Nm_val name | `Nm_external name | `Nm_payload name);
         external_module_name;

         call_name = `Nm_na ;
         module_as_val = None;
         val_send = `Nm_na ;
         val_send_pipe = `Nm_na;    
         set_index = false;
         get_index = false;
         new_name = `Nm_na;
         set_name = `Nm_na ;
         get_name = `Nm_na 

        } 
        -> 
        Js_global {txt = name; external_module_name}
      | {val_name = #bundle_source }
        -> Location.raise_errorf ~loc "conflict attributes found"
      | {splice ;
         external_module_name = (Some _ as external_module_name);

         val_name = `Nm_na ;         
         call_name = `Nm_na ;
         module_as_val = None;
         val_send = `Nm_na ;
         val_send_pipe = `Nm_na;             
         set_index = false;
         get_index = false;
         new_name = `Nm_na;
         set_name = `Nm_na ;
         get_name = `Nm_na ;

        }
        ->
        let name = string_of_bundle_source prim_name_or_pval_prim in
        begin match arg_types with
          | [] -> Js_global {txt = name; external_module_name}
          | _ -> Js_call {txt = {splice; name}; external_module_name}                     
        end        

      | {val_send = (`Nm_val name | `Nm_external name | `Nm_payload name); 
         splice;
         val_send_pipe = `Nm_na;
         val_name = `Nm_na  ;
         call_name = `Nm_na ;
         module_as_val = None;
         set_index = false;
         get_index = false;
         new_name = `Nm_na;
         set_name = `Nm_na ;
         get_name = `Nm_na ;
         external_module_name = None ;
        } -> 
        begin match arg_types with 
        | _self :: _args -> 
          Js_send {splice ; name; pipe = false}
        | _ ->
          Location.raise_errorf ~loc "Ill defined attribute [@@bs.send] (at least one argument)"
        end
      | {val_send = #bundle_source} 
        -> Location.raise_errorf ~loc "conflict attributes found"

      | {val_send_pipe = `Type typ; 
         splice = (false as splice);
         val_send = `Nm_na;
         val_name = `Nm_na  ;
         call_name = `Nm_na ;
         module_as_val = None;
         set_index = false;
         get_index = false;
         new_name = `Nm_na;
         set_name = `Nm_na ;
         get_name = `Nm_na ;
         external_module_name = None ;
        } -> 
        begin match arg_types with 
          | _self :: _args ->
            object_type := Some typ ;            
            Js_send {splice  ;
                   name = string_of_bundle_source prim_name_or_pval_prim;
                   pipe = true}
        | _ ->
          Location.raise_errorf ~loc "Ill defined attribute [@@bs.send] (at least one argument)"
        end
      | {val_send_pipe = `Type _ } 
        -> Location.raise_errorf ~loc "conflict attributes found"

      | {new_name = (`Nm_val name | `Nm_external name | `Nm_payload name);
         external_module_name;

         val_name = `Nm_na  ;
         call_name = `Nm_na ;
         module_as_val = None;
         set_index = false;
         get_index = false;
         val_send = `Nm_na ;
         val_send_pipe = `Nm_na;             
         set_name = `Nm_na ;
         get_name = `Nm_na 
        } 
        -> Js_new {txt =name; external_module_name}
      | {new_name = #bundle_source }
        -> Location.raise_errorf ~loc "conflict attributes found"

      | {set_name = (`Nm_val name | `Nm_external name | `Nm_payload name);

         val_name = `Nm_na  ;
         call_name = `Nm_na ;
         module_as_val = None;
         set_index = false;
         get_index = false;
         val_send = `Nm_na ;
         val_send_pipe = `Nm_na;             
         new_name = `Nm_na ;
         get_name = `Nm_na ;
         external_module_name = None
        } 
        -> 
        begin match arg_types with 
        | [_obj; _v] -> 
          Js_set name 
        | _ -> Location.raise_errorf ~loc "Ill defined attribute [@@bs.set] (two args required)"
        end
      | {set_name = #bundle_source}
        -> Location.raise_errorf ~loc "conflict attributes found"

      | {get_name = (`Nm_val name | `Nm_external name | `Nm_payload name);

         val_name = `Nm_na  ;
         call_name = `Nm_na ;
         module_as_val = None;
         set_index = false;
         get_index = false;
         val_send = `Nm_na ;
         val_send_pipe = `Nm_na;             
         new_name = `Nm_na ;
         set_name = `Nm_na ;
         external_module_name = None
        }
        ->
        begin match arg_types with 
        | [_ ] -> Js_get name
        | _ ->
          Location.raise_errorf ~loc "Ill defined attribute [@@bs.get] (only one argument)"
        end
      | {get_name = #bundle_source}
        -> Location.raise_errorf ~loc "conflict attributes found"
      | _ ->  Location.raise_errorf ~loc "Illegal attribute found"  in
    check_ffi ~loc ffi;
    (match ffi, result_type_ty with
     | Obj_create arg_labels ,  {ptyp_desc = Ptyp_any; _}
       ->
       let result =
         Ast_core_type.make_obj ~loc (
         List.fold_right2  (fun arg label acc ->
           match arg, label with
           | (_, ty), Ast_core_type.Label s
             -> (s , [], ty) :: acc                 
           | (_, ty), Optional s
             ->
             begin match (ty : Ast_core_type.t) with
               | {ptyp_desc =
                    Ptyp_constr({txt =
                                   Ldot (Lident "*predef*", "option") },
                                [ty])}
                 ->                
                 (s, [], Ast_comb.to_undefined_type loc ty) :: acc
               | _ -> assert false                 
             end                 
           | (_, _), Ast_core_type.Empty -> acc                
           ) arg_types_ty arg_labels [])  in
       Ast_core_type.replace_result type_annotation result
     | Js_send {pipe = true }, _ ->
       begin match !object_type with       
         | Some obj ->
           Ast_core_type.replace_result type_annotation
             (Ast_helper.Typ.arrow ~loc "" obj result_type_ty)
         | None -> assert false
       end           
     | _, _ -> type_annotation
    ) ,

    (* TODO: document *)    
    (match ffi , prim_name with
     | Obj_create _ , _ -> prim_name
     | _ , "" -> pval_prim
     | _, _ -> prim_name),
    (match !object_type with
    |None ->      
      Bs(arg_types, result_type,  ffi)
    | Some obj ->
      Bs(arg_types @ [translate_arg_type ("", obj) ], result_type,  ffi)
    )


let handle_attributes_as_string 
    pval_loc
    pval_prim 
    typ attrs v = 
  let pval_type, prim_name, ffi = 
    handle_attributes pval_loc pval_prim typ attrs v  in
  pval_type, [prim_name; to_string ffi]
    

