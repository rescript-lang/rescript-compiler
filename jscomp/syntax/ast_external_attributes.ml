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

type pipe = bool 
type js_call = { 
  name : string;
  external_module_name : external_module_name option;
  splice : bool 
}

type js_send = { 
  name : string ;
  splice : bool ; 
  pipe : pipe   
} (* we know it is a js send, but what will happen if you pass an ocaml objct *)

type js_global_val = {
  name : string ; 
  external_module_name : external_module_name option
  }

type js_new_val = {
  name : string ; 
  external_module_name : external_module_name option;
  splice : bool ;
}

type js_module_as_fn = 
  { external_module_name : external_module_name;
    splice : bool 
  }

type arg_type = Ast_core_type.arg_type
type arg_label = Ast_core_type.arg_label

type arg_kind = 
  {
    arg_type : arg_type;
    arg_label : arg_label
  }


type ffi = 
  | Obj_create of arg_label list
  | Js_global of js_global_val 
  | Js_module_as_var of  external_module_name
  | Js_module_as_fn of js_module_as_fn
  | Js_module_as_class of external_module_name             
  | Js_call of js_call 
  | Js_send of js_send
  | Js_new of js_new_val
  | Js_set of string
  | Js_get of string
  | Js_get_index
  | Js_set_index

let name_of_ffi ffi =
  match ffi with 
  | Js_get_index -> "[@@bs.get_index]"
  | Js_set_index -> "[@@bs.set_index]"
  | Js_get s -> Printf.sprintf "[@@bs.get %S]" s 
  | Js_set s -> Printf.sprintf "[@@bs.set %S]" s 
  | Js_call v  -> Printf.sprintf "[@@bs.val %S]" v.name
  | Js_send v  -> Printf.sprintf "[@@bs.send %S]" v.name
  | Js_module_as_fn v  -> Printf.sprintf "[@@bs.val %S]" v.external_module_name.bundle
  | Js_new v  -> Printf.sprintf "[@@bs.new %S]" v.name                    
  | Js_module_as_class v
    -> Printf.sprintf "[@@bs.module] %S " v.bundle
  | Js_module_as_var v
    -> 
      Printf.sprintf "[@@bs.module] %S " v.bundle
  | Js_global v 
    -> 
      Printf.sprintf "[@@bs.val] %S " v.name                    
  | Obj_create _ -> 
    Printf.sprintf "[@@bs.obj]"
type t  = 
  | Bs of arg_kind list  * Ast_core_type.arg_type * ffi
  | Normal 
  (* When it's normal, it is handled as normal c functional ffi call *)



let get_arg_type ({ptyp_desc; ptyp_attributes; ptyp_loc = loc} as ptyp : Ast_core_type.t) : 
  arg_type * Ast_core_type.t  = 
    match Ast_attributes.process_bs_string_int ptyp_attributes, ptyp_desc with 
    | (`String, ptyp_attributes),  Ptyp_variant ( row_fields, Closed, None)
      -> 
      let case, result, row_fields  = 
        (List.fold_right (fun tag (nullary, acc, row_fields) -> 
             match nullary, tag with 
             | (`Nothing | `Null), 
               Parsetree.Rtag (label, attrs, true,  [])
               -> 
               begin match Ast_attributes.process_bs_string_as attrs with 
                 | Some name, new_attrs  -> 
                   `Null, ((Ext_pervasives.hash_variant label, name) :: acc ), 
                   Parsetree.Rtag(label, new_attrs, true, []) :: row_fields

                 | None, _ -> 
                   `Null, ((Ext_pervasives.hash_variant label, label) :: acc ), 
                   tag :: row_fields
               end
             | (`Nothing | `NonNull), Parsetree.Rtag(label, attrs, false, ([ _ ] as vs)) 
               -> 
               begin match Ast_attributes.process_bs_string_as attrs with 
                 | Some name, new_attrs -> 
                   `NonNull, ((Ext_pervasives.hash_variant label, name) :: acc),
                   Parsetree.Rtag (label, new_attrs, false, vs) :: row_fields
                 | None, _ -> 
                   `NonNull, ((Ext_pervasives.hash_variant label, label) :: acc),
                   (tag :: row_fields)
               end
             | _ -> Location.raise_errorf ~loc "Not a valid string type"
           ) row_fields (`Nothing, [], [])) in 
       (match case with 
        | `Nothing -> Location.raise_errorf ~loc "Not a valid string type"
        | `Null -> NullString result 
        | `NonNull -> NonNullString result) , 
       {ptyp with ptyp_desc = Ptyp_variant(row_fields, Closed, None);
        ptyp_attributes ;
       }
    | (`String, _),  _ -> Location.raise_errorf ~loc "Not a valid string type"

    | (`Ignore, ptyp_attributes), _  -> 
      (Ignore, {ptyp with ptyp_attributes})
    | (`Int , ptyp_attributes),  Ptyp_variant ( row_fields, Closed, None) -> 
      let _, acc, rev_row_fields = 
        (List.fold_left 
           (fun (i,acc, row_fields) rtag -> 
              match rtag with 
              | Parsetree.Rtag (label, attrs, true,  [])
                -> 
                  begin match Ast_attributes.process_bs_int_as attrs with 
                  | Some i, new_attrs -> 
                    i + 1, ((Ext_pervasives.hash_variant label , i):: acc ), 
                    Parsetree.Rtag (label, new_attrs, true, []) :: row_fields
                  | None, _ -> 
                    i + 1 , ((Ext_pervasives.hash_variant label , i):: acc ), rtag::row_fields
                  end

              | _ -> Location.raise_errorf ~loc "Not a valid string type"
           ) (0, [],[]) row_fields) in 
      Int (List.rev acc),
      {ptyp with 
       ptyp_desc = Ptyp_variant(List.rev rev_row_fields, Closed, None );
       ptyp_attributes
      }
      
    | (`Int, _), _ -> Location.raise_errorf ~loc "Not a valid string type"
    | (`Nothing, ptyp_attributes),  ptyp_desc ->
      begin match ptyp_desc with
        | Ptyp_constr ({txt = Lident "bool"}, [])
          -> 
          Bs_warnings.prerr_warning loc Unsafe_ffi_bool_type;
          Nothing
        | Ptyp_constr ({txt = Lident "unit"}, [])
          -> Unit 
        | Ptyp_constr ({txt = Lident "array"}, [_])
          -> Array
        | Ptyp_variant _ ->
          Bs_warnings.prerr_warning loc Unsafe_poly_variant_type;
          Nothing           
        | _ ->
          Nothing           
      end, ptyp


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
  | Js_global {name} -> valid_global_name ?loc  name
  | Js_send {name } 
  | Js_set  name
  | Js_get name
    ->  valid_method_name ?loc name
  | Obj_create _ -> ()
  | Js_get_index | Js_set_index 
    -> ()

  | Js_module_as_var external_module_name
  | Js_module_as_fn {external_module_name; _}
  | Js_module_as_class external_module_name             
    -> check_external_module_name external_module_name
  | Js_new {external_module_name ;  name}
  | Js_call {external_module_name ;  name ; _}
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
    val_send_pipe : Ast_core_type.t option;    
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
    val_send_pipe = None;    
    splice = false;
    set_index = false;
    get_index = false;
    new_name = `Nm_na;
    call_name = `Nm_na;
    set_name = `Nm_na ;
    get_name = `Nm_na ;
    mk_obj = false ; 

  }


let bs_external = "BS:" ^ Js_config.version
let bs_external_length = String.length bs_external

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

let process_external_attributes 
    no_arguments 
    (prim_name_or_pval_prim: [< bundle_source ] as 'a)
    pval_prim
    prim_attributes =
  let name_from_payload_or_prim payload : name_source =
    match Ast_payload.is_single_string payload with
    | Some  val_name ->  `Nm_payload val_name
    | None ->  (prim_name_or_pval_prim :> name_source)
  in
  List.fold_left 
    (fun (st, attrs)
      (({txt ; loc}, payload) as attr : Ast_attributes.attr) 
      ->
        if Ext_string.starts_with txt "bs." then
          begin match txt with 
            | "bs.val" ->  
              if no_arguments then
                {st with val_name = name_from_payload_or_prim payload}
              else 
                {st with call_name = name_from_payload_or_prim payload}

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
              { st with val_send_pipe = Some (Ast_payload.as_core_type loc payload)}                
            | "bs.set" -> 
              {st with set_name = name_from_payload_or_prim payload}
            | "bs.get" -> {st with get_name = name_from_payload_or_prim payload}

            | "bs.new" -> {st with new_name = name_from_payload_or_prim payload}
            | "bs.set_index" -> {st with set_index = true}
            | "bs.get_index"-> {st with get_index = true}
            | "bs.obj" -> {st with mk_obj = true}
            | _ -> (Bs_warnings.warn_unused_attribute loc txt; st)
          end, attrs
        else (st , attr :: attrs)
    )
    (init_st, []) prim_attributes 


let list_of_arrow (ty : Parsetree.core_type) = 
  let rec aux (ty : Parsetree.core_type) acc = 
    match ty.ptyp_desc with 
    | Ptyp_arrow(label,t1,t2) -> 
      aux t2 ((label,t1,ty.ptyp_attributes,ty.ptyp_loc) ::acc)
    | Ptyp_poly(_, ty) -> (* should not happen? *)
      Location.raise_errorf ~loc:ty.ptyp_loc "Unhandled poly type"
    | return_type -> ty, List.rev acc
  in aux ty []

(** Note that the passed [type_annotation] is already processed by visitor pattern before 
*)
let handle_attributes 
    (loc : Bs_loc.t)
    (pval_prim : string ) 
    (type_annotation : Parsetree.core_type)
    (prim_attributes : Ast_attributes.t) (prim_name : string)
  : Ast_core_type.t * string * t * Ast_attributes.t =
  let prim_name_or_pval_prim =
    if String.length prim_name = 0 then  `Nm_val pval_prim
    else  `Nm_external prim_name  (* need check name *)
  in    
  let result_type, arg_types_ty =
    list_of_arrow type_annotation in
  let result_type_spec, new_result_type  = get_arg_type result_type in
  let (st, left_attrs) = 
    process_external_attributes 
      (arg_types_ty = [])
      prim_name_or_pval_prim pval_prim prim_attributes in 

  let splice = st.splice in 
  let arg_type_specs, new_arg_types_ty, arg_type_specs_length   = 
    List.fold_right 
      (fun (label,ty,attr,loc) (arg_type_specs, arg_types, i) -> 
         let spec, new_ty = get_arg_type ty in
         (if i = 0 && splice  then
           match spec with 
           | Array  -> ()
           | _ ->  Location.raise_errorf ~loc "[@@bs.splice] expect last type to array");
         ({ arg_label = Ast_core_type.label_name label ; 
            arg_type = spec 
          } :: arg_type_specs,
          (label, new_ty,attr,loc) :: arg_types,
          i + 1)
      ) arg_types_ty 
      (match st with
       | {val_send_pipe = Some obj} ->      
         let spec, new_ty = get_arg_type obj in 
         [{ arg_label = Empty ; 
           arg_type = spec
          }],
         ["", new_ty, [], obj.ptyp_loc]
         ,0
       | {val_send_pipe = None } -> [],[], 0) in 


  let ffi = 
    match st with 
    | { mk_obj = true;
        val_name = `Nm_na; 
        external_module_name = None ;
        module_as_val = None;
        val_send = `Nm_na;
        val_send_pipe = None;    
        splice = false;
        new_name = `Nm_na;
        call_name = `Nm_na;
        set_name = `Nm_na ;
        get_name = `Nm_na ;
        get_index = false ;
      } ->
      if String.length prim_name <> 0 then 
        Location.raise_errorf ~loc "[@@bs.obj] expect external names to be empty string";
      Obj_create (List.map (function
          | {arg_label = (Empty as l) ; arg_type = Unit  }
            -> l 
          | {arg_label = Empty ; arg_type = _ }
            -> Location.raise_errorf ~loc "expect label, optional, or unit here"
          | {arg_label = (Label _) ; arg_type = (Ignore | Unit) ; }
            -> Empty
          | {arg_label = Label name ; arg_type = (Nothing | Array)} -> 
            Label (Lam_methname.translate ~loc name)            
          | {arg_label = Label l ; arg_type = (NullString _ | NonNullString _ | Int _ ) }
            -> Location.raise_errorf ~loc 
                 "bs.obj label %s does not support such arg type" l
          | {arg_label = Optional name ; arg_type = (Nothing | Array | Unit | Ignore)} 
            -> Optional (Lam_methname.translate ~loc name)
          | {arg_label = Optional l ; arg_type = (NullString _ | NonNullString _ | Int _)} 
            -> Location.raise_errorf ~loc 
                 "bs.obj optional %s does not support such arg type" l )
          arg_type_specs)(* Need fetch label here, for better error message *)
    | {mk_obj = true; _}
      ->
      Location.raise_errorf ~loc "conflict attributes found"                
    | {set_index = true;

       val_name = `Nm_na; 
       external_module_name = None ;
       module_as_val = None;
       val_send = `Nm_na;
       val_send_pipe = None;    
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
      if arg_type_specs_length = 3 then 
          Js_set_index
      else 
        Location.raise_errorf ~loc "Ill defined attribute [@@bs.set_index](arity of 3)"

    | {set_index = true; _}
      ->
      Location.raise_errorf ~loc "conflict attributes found"        

    | {get_index = true;

       val_name = `Nm_na; 
       external_module_name = None ;
       module_as_val = None;
       val_send = `Nm_na;
       val_send_pipe = None;    

       splice = false;
       new_name = `Nm_na;
       call_name = `Nm_na;
       set_name = `Nm_na ;
       get_name = `Nm_na ;
       mk_obj = false ; 
      } ->
      if String.length prim_name <> 0 then 
        Location.raise_errorf ~loc "[@@bs.get_index] expect external names to be empty string";
      if arg_type_specs_length = 2 then 
          Js_get_index
      else Location.raise_errorf ~loc "Ill defined attribute [@@bs.get_index] (arity of 2)"

    | {get_index = true; _}
      -> Location.raise_errorf ~loc "conflict attributes found"        
    | {module_as_val = Some external_module_name ;

       get_index = false;
       val_name ;
       new_name ;
       (*TODO: a better way to avoid breaking existing code,
         we need tell the difference from 
         {[
           1. [@@bs.val "x"]
           2. external x : .. "x" [@@bs.val ]
           3. external x : .. ""  [@@bs.val]    ]}
                                                *)         
      external_module_name = None ;
      val_send = `Nm_na;
      val_send_pipe = None;    
      splice ;
      call_name = `Nm_na;
      set_name = `Nm_na ;
      get_name = `Nm_na ;
      mk_obj = false ;} ->
   begin match arg_types_ty, new_name, val_name  with         
    | [], `Nm_na,  _ -> Js_module_as_var external_module_name
    | _, `Nm_na, _ -> Js_module_as_fn {splice; external_module_name }
    | _, #bundle_source, #bundle_source ->
      Location.raise_errorf ~loc "conflict attributes found"
    | _, (`Nm_val _ | `Nm_external _) , `Nm_na
      -> Js_module_as_class external_module_name
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
    val_send_pipe = None;    

    set_index = false;
    get_index = false;
    new_name = `Nm_na;
    set_name = `Nm_na ;
    get_name = `Nm_na 
   } -> 
   Js_call {splice; name; external_module_name}
 | {call_name = #bundle_source } 
   -> Location.raise_errorf ~loc "conflict attributes found"

 | {val_name = (`Nm_val name | `Nm_external name | `Nm_payload name);
    external_module_name;

    call_name = `Nm_na ;
    module_as_val = None;
    val_send = `Nm_na ;
    val_send_pipe = None;    
    set_index = false;
    get_index = false;
    new_name = `Nm_na;
    set_name = `Nm_na ;
    get_name = `Nm_na 

   } 
   -> 
   Js_global { name; external_module_name}
 | {val_name = #bundle_source }
   -> Location.raise_errorf ~loc "conflict attributes found"
 | {splice ;
    external_module_name = (Some _ as external_module_name);

    val_name = `Nm_na ;         
    call_name = `Nm_na ;
    module_as_val = None;
    val_send = `Nm_na ;
    val_send_pipe = None;             
    set_index = false;
    get_index = false;
    new_name = `Nm_na;
    set_name = `Nm_na ;
    get_name = `Nm_na ;

   }
   ->
   let name = string_of_bundle_source prim_name_or_pval_prim in
   if arg_type_specs_length  = 0 then
     Js_global { name; external_module_name}
   else  Js_call {splice; name; external_module_name}                     
 | {val_send = (`Nm_val name | `Nm_external name | `Nm_payload name); 
    splice;
    val_send_pipe = None;
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
   if arg_type_specs_length > 0 then 
       Js_send {splice ; name; pipe = false}
   else 
       Location.raise_errorf ~loc "Ill defined attribute [@@bs.send] (at least one argument)"
 | {val_send = #bundle_source} 
   -> Location.raise_errorf ~loc "conflict attributes found"

 | {val_send_pipe = Some typ; 
    (* splice = (false as splice); *)
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
   (** can be one argument *)
   Js_send {splice  ;
            name = string_of_bundle_source prim_name_or_pval_prim;
            pipe = true}

 | {val_send_pipe = Some _ } 
   -> Location.raise_errorf ~loc "conflict attributes found"

 | {new_name = (`Nm_val name | `Nm_external name | `Nm_payload name);
    external_module_name;

    val_name = `Nm_na  ;
    call_name = `Nm_na ;
    module_as_val = None;
    set_index = false;
    get_index = false;
    val_send = `Nm_na ;
    val_send_pipe = None;             
    set_name = `Nm_na ;
    get_name = `Nm_na ;
    splice 
   } 
   -> Js_new {name; external_module_name; splice}
 | {new_name = #bundle_source }
   -> Location.raise_errorf ~loc "conflict attributes found"

 | {set_name = (`Nm_val name | `Nm_external name | `Nm_payload name);

    val_name = `Nm_na  ;
    call_name = `Nm_na ;
    module_as_val = None;
    set_index = false;
    get_index = false;
    val_send = `Nm_na ;
    val_send_pipe = None;             
    new_name = `Nm_na ;
    get_name = `Nm_na ;
    external_module_name = None
   } 
   -> 
   if arg_type_specs_length = 2 then 
       Js_set name 
   else  Location.raise_errorf ~loc "Ill defined attribute [@@bs.set] (two args required)"

 | {set_name = #bundle_source}
   -> Location.raise_errorf ~loc "conflict attributes found"

 | {get_name = (`Nm_val name | `Nm_external name | `Nm_payload name);

    val_name = `Nm_na  ;
    call_name = `Nm_na ;
    module_as_val = None;
    set_index = false;
    get_index = false;
    val_send = `Nm_na ;
    val_send_pipe = None;             
    new_name = `Nm_na ;
    set_name = `Nm_na ;
    external_module_name = None
   }
   ->
   if arg_type_specs_length = 1 then  
     Js_get name
   else 
       Location.raise_errorf ~loc "Ill defined attribute [@@bs.get] (only one argument)"
 | {get_name = #bundle_source}
   -> Location.raise_errorf ~loc "conflict attributes found"
 | _ ->  Location.raise_errorf ~loc "Illegal attribute found"  in
  begin 
    check_ffi ~loc ffi;
    (match ffi, new_result_type with
     | Obj_create arg_labels ,  {ptyp_desc = Ptyp_any; _}
       ->
       (* special case: 
          {[ external f : int -> string -> _ = "" ]}
       *)
       let result =
         Ast_core_type.make_obj ~loc (
           List.fold_right2  (fun arg label acc ->
               match arg, label with
               | (_, ty, _,_), Ast_core_type.Label s
                 -> (s , [], ty) :: acc                 
               | (_, ty, _,_), Optional s
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
               | (_, _, _,_), Ast_core_type.Empty -> acc                
             ) arg_types_ty arg_labels [])  in

       List.fold_right (fun (label,ty,attrs,loc) acc -> 
           Ast_helper.Typ.arrow ~loc  ~attrs label ty acc 
           ) new_arg_types_ty result 

       (* Ast_core_type.replace_result type_annotation result *)
     | _  ->
       List.fold_right (fun (label,ty,attrs,loc) acc -> 
           Ast_helper.Typ.arrow ~loc  ~attrs label ty acc 
           ) new_arg_types_ty new_result_type
    ) ,
    prim_name,
    (Bs(arg_type_specs, result_type_spec,  ffi)), left_attrs
  end

let handle_attributes_as_string 
    pval_loc
    pval_prim 
    (typ : Ast_core_type.t) attrs v = 
  let pval_type, prim_name, ffi, processed_attrs  = 
    handle_attributes pval_loc pval_prim typ attrs v  in
  pval_type, [prim_name; to_string ffi], processed_attrs
    
let pval_prim_of_labels labels = 
  let encoding = 
    let (arg_kinds, vs) = 
      List.fold_right 
        (fun {Asttypes.loc ; txt } (arg_kinds,v)
          ->
            let arg_label =  Ast_core_type.Label (Lam_methname.translate ~loc txt) in
            {arg_type = Nothing ; 
             arg_label  } :: arg_kinds, arg_label :: v
        )
        labels ([],[]) in 
    to_string @@
    Bs (arg_kinds , Nothing, Obj_create vs) in 
  [""; encoding]

