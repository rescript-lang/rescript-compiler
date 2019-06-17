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


[@@@ocaml.warning "+9"]
(* record pattern match complete checker*)

type field = 
  | No_fields
  | Valid_fields
  | Invalid_field

let variant_can_bs_unwrap_fields (row_fields : Parsetree.row_field list) : bool =
  let validity =
    Ext_list.fold_left row_fields No_fields      
      begin fun st row ->
        match st, row with
        | (* we've seen no fields or only valid fields so far *)
          (No_fields | Valid_fields),
          (* and this field has one constructor arg that we can unwrap to *)
          Rtag (label, attrs, false, ([ _ ]))
          ->
          Valid_fields
        | (* otherwise, this field or a previous field was invalid *)
          _ ->
          Invalid_field
      end
  in
  validity = Valid_fields 

(*
  TODO: [nolabel] is only used once turn Nothing into Unit, refactor later
*)
let spec_of_ptyp 
    (nolabel : bool) (ptyp : Parsetree.core_type) : External_arg_spec.attr = 
  let ptyp_desc = ptyp.ptyp_desc in
  match Ast_attributes.iter_process_bs_string_int_unwrap_uncurry ptyp.ptyp_attributes with
  | `String ->
    begin match ptyp_desc with
      | Ptyp_variant ( row_fields, Closed, None)
        ->
        Ast_polyvar.map_row_fields_into_strings ptyp.ptyp_loc row_fields
      | _ ->
        Bs_syntaxerr.err ptyp.ptyp_loc Invalid_bs_string_type
    end
  | `Ignore ->
    Ignore
  | `Int ->
    begin match ptyp_desc with
      | Ptyp_variant ( row_fields, Closed, None) ->
        let int_lists =
          Ast_polyvar.map_row_fields_into_ints ptyp.ptyp_loc row_fields in
        Int int_lists
      | _ -> Bs_syntaxerr.err ptyp.ptyp_loc Invalid_bs_int_type
    end
  | `Unwrap ->
    begin match ptyp_desc with
      | Ptyp_variant (row_fields, Closed, _)
        when variant_can_bs_unwrap_fields row_fields ->
        Unwrap
      | _ ->
        Bs_syntaxerr.err ptyp.ptyp_loc Invalid_bs_unwrap_type
    end
  | `Uncurry opt_arity ->
    let real_arity =  Ast_core_type.get_uncurry_arity ptyp in
    (begin match opt_arity, real_arity with
       | Some arity, `Not_function ->
         Fn_uncurry_arity arity
       | None, `Not_function  ->
         Bs_syntaxerr.err ptyp.ptyp_loc Canot_infer_arity_by_syntax
       | None, `Arity arity  ->
         Fn_uncurry_arity arity
       | Some arity, `Arity n ->
         if n <> arity then
           Bs_syntaxerr.err ptyp.ptyp_loc (Inconsistent_arity (arity,n))
         else Fn_uncurry_arity arity
     end)
  | `Nothing ->
    begin match ptyp_desc with
      | Ptyp_constr ({txt = Lident "unit"; _}, [])
        -> if nolabel then Extern_unit else  Nothing
      | Ptyp_variant _ ->
        Bs_warnings.prerr_bs_ffi_warning ptyp.ptyp_loc Unsafe_poly_variant_type;
        Nothing
      | _ ->
        Nothing
    end
(* is_optional = false 
*)
let refine_arg_type ~(nolabel:bool) (ptyp : Ast_core_type.t) 
  : Ast_core_type.t * External_arg_spec.attr = 
  if Ast_core_type.is_any ptyp then (* (_[@bs.as ])*)
    let ptyp_attrs = ptyp.ptyp_attributes in
    let result = Ast_attributes.iter_process_bs_string_or_int_as ptyp_attrs in
    (* when ppx start dropping attributes
       we should warn, there is a trade off whether
       we should warn dropped non bs attribute or not
    *)
    Bs_ast_invariant.warn_discarded_unused_attributes ptyp_attrs;
    match result with
    |  None ->
      Bs_syntaxerr.err ptyp.ptyp_loc Invalid_underscore_type_in_external
    | Some (`Int i) ->
      Ast_literal.type_int ~loc:ptyp.ptyp_loc (), Arg_cst(External_arg_spec.cst_int i)
    | Some (`Str i)->
      Ast_literal.type_string ~loc:ptyp.ptyp_loc (), Arg_cst (External_arg_spec.cst_string i)
    | Some (`Json_str s) ->
      Ast_literal.type_string ~loc:ptyp.ptyp_loc (), Arg_cst (External_arg_spec.cst_json ptyp.ptyp_loc s)
  else (* ([`a|`b] [@bs.string]) *)
    ptyp, spec_of_ptyp nolabel ptyp   

let get_basic_type_from_option_label (ptyp_arg : Ast_core_type.t) =     
#if OCAML_VERSION =~ "<4.03.0" then
      match ptyp_arg.ptyp_desc with 
      | Ptyp_constr (_, [ty]) -> ty  (*optional*)
      | _ -> assert false
#else    
      ptyp_arg 
#end      
  
(** Given the type of argument, process its [bs.] attribute and new type,
    The new type is currently used to reconstruct the external type
    and result type in [@@bs.obj]
    They are not the same though, for example
    {[
      external f : hi:([ `hi | `lo ] [@bs.string]) -> unit -> _ = "" [@@bs.obj]
    ]}
    The result type would be [ hi:string ]
*)
let get_opt_arg_type
    ~(nolabel : bool)
    (ptyp_arg : Ast_core_type.t) :
  External_arg_spec.attr  =
  let ptyp = get_basic_type_from_option_label ptyp_arg in 
  if Ast_core_type.is_any ptyp then (* (_[@bs.as ])*)
    (* extenral f : ?x:_ -> y:int -> _ = "" [@@bs.obj] is not allowed *)
    Bs_syntaxerr.err ptyp.ptyp_loc Invalid_underscore_type_in_external;
  (* ([`a|`b] [@bs.string]) *)    
  spec_of_ptyp nolabel ptyp



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
  [`Nm_payload of string (* from payload [@@bs.val "xx" ]*)
  |`Nm_external of string (* from "" in external *)
  | `Nm_val of string   (* from function name *)
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




type external_desc =
  { val_name : name_source;
    external_module_name : External_ffi_types.external_module_name option;
    module_as_val : External_ffi_types.external_module_name option;
    val_send : name_source ;
    val_send_pipe : Ast_core_type.t option;
    splice : bool ; (* mutable *)
    scopes : string list ;
    set_index : bool; (* mutable *)
    get_index : bool;
    new_name : name_source ;
    call_name : name_source ;
    set_name : name_source ;
    get_name : name_source ;

    mk_obj : bool ;
    return_wrapper : External_ffi_types.return_wrapper ;

  }

let init_st =
  {
    val_name = `Nm_na;
    external_module_name = None ;
    module_as_val = None;
    val_send = `Nm_na;
    val_send_pipe = None;
    splice = false;
    scopes = [];
    set_index = false;
    get_index = false;
    new_name = `Nm_na;
    call_name = `Nm_na;
    set_name = `Nm_na ;
    get_name = `Nm_na ;
    mk_obj = false ;
    return_wrapper = Return_unset;

  }


let return_wrapper loc (txt : string) : External_ffi_types.return_wrapper =
  match txt with
  | "undefined_to_opt" -> Return_undefined_to_opt
  | "null_to_opt" -> Return_null_to_opt
  | "nullable"
  | "null_undefined_to_opt" -> Return_null_undefined_to_opt
  | "identity" -> Return_identity
  | _ ->
    Bs_syntaxerr.err loc Not_supported_directive_in_bs_return


(* The processed attributes will be dropped *)
let parse_external_attributes
    (no_arguments : bool)   
    (prim_name_check : string)
    (prim_name_or_pval_prim: bundle_source )
    (prim_attributes : Ast_attributes.t) : Ast_attributes.t * external_desc =

  (* shared by `[@@bs.val]`, `[@@bs.send]`,
     `[@@bs.set]`, `[@@bs.get]` , `[@@bs.new]`
     `[@@bs.send.pipe]` does not use it
  *)
  let name_from_payload_or_prim_check 
      ~loc 
      (payload : Parsetree.payload) : name_source =
    let name_source =   
      match payload with
      | PStr [] ->
        (prim_name_or_pval_prim :> name_source)
      (* It is okay to have [@@bs.val] without payload *)
      | _ ->
        begin match Ast_payload.is_single_string payload with
          | Some  (val_name, _) ->  `Nm_payload val_name
          | None ->
            Location.raise_errorf ~loc "Invalid payload"
        end
    in 
    (match name_source with 
    | `Nm_val s -> Bs_warnings.warn_fragile_external_name loc s
    | _ -> ()
    );
    name_source
  in
  Ext_list.fold_left prim_attributes ([], init_st) 
    (fun (attrs, st) (({txt ; loc}, payload) as attr )
      ->
        if txt = Literals.gentype_import then 
          let bundle = 
              "./" ^ Ext_path.chop_extension_if_any  
                (Filename.basename (Js_config.get_current_file ())) ^ ".gen"
            in 
            attr::attrs, 
            {st with external_module_name = Some { bundle; module_bind_name = Phint_nothing}}          
        else if Ext_string.starts_with txt "bs." then
           attrs, begin match txt with
            | "bs.val" ->
              let name_source = name_from_payload_or_prim_check ~loc payload in 
              if no_arguments then
                {st with val_name = name_source}
              else
                {st with call_name = name_source}

            | "bs.module" ->
              begin match Ast_payload.assert_strings loc payload with
                | [bundle] ->
                  {st with external_module_name =
                             Some {bundle; module_bind_name = Phint_nothing}}
                | [bundle;bind_name] ->
                  {st with external_module_name =
                             Some {bundle; module_bind_name = Phint_name bind_name}}
                | [] ->
                  { st with
                    module_as_val =
                      Some
                        { bundle =
                            string_of_bundle_source
                              (prim_name_or_pval_prim :> bundle_source) ;
                          module_bind_name = Phint_nothing}
                  }
                | _  ->
                  Bs_syntaxerr.err loc Illegal_attribute
              end
            | "bs.scope" ->
              begin match Ast_payload.assert_strings loc payload with
                | [] ->
                  Bs_syntaxerr.err loc Illegal_attribute
                (* We need err on empty scope, so we can tell the difference
                   between unset/set
                *)
                | scopes ->  { st with scopes = scopes }
              end
            | "bs.splice" | "bs.variadic" -> {st with splice = true}
            | "bs.send" ->
              let name_source = name_from_payload_or_prim_check ~loc payload in 
              { st with val_send = name_source}
            | "bs.send.pipe"
              ->
              { st with val_send_pipe = Some (Ast_payload.as_core_type loc payload)}
            | "bs.set" ->
              let name_source = name_from_payload_or_prim_check ~loc  payload in 
              {st with set_name = name_source}
            | "bs.get" -> 
              let name_source = name_from_payload_or_prim_check ~loc payload in 
              {st with get_name = name_source}

            | "bs.new" ->   
              let name_source = name_from_payload_or_prim_check ~loc payload in 
              {st with new_name = name_source}
            | "bs.set_index" -> 
              if String.length prim_name_check <> 0 then 
                Location.raise_errorf ~loc "[@@bs.set_index] expect external names to be empty string";
              {st with set_index = true}
            | "bs.get_index"->               
              if String.length prim_name_check <> 0 then
                Location.raise_errorf ~loc "[@@bs.get_index] expect external names to be empty string";
              {st with get_index = true}
            | "bs.obj" -> {st with mk_obj = true}
            | "bs.return" ->
              let actions =
                Ast_payload.ident_or_record_as_config loc payload in
              begin match actions with
                | [ ({txt; _ },None) ] ->
                  { st with return_wrapper = return_wrapper loc txt}
                | _ ->
                  Bs_syntaxerr.err loc Not_supported_directive_in_bs_return
              end
            | _ -> (Location.prerr_warning loc (Bs_unused_attribute txt); st)
          end
        else attr :: attrs, st
    )
    


let rec has_bs_uncurry (attrs : Ast_attributes.t) = 
  Ext_list.exists_fst attrs (fun x -> x.txt = "bs.uncurry")


let check_return_wrapper
    loc (wrapper : External_ffi_types.return_wrapper)
    result_type =
  match wrapper with
  | Return_identity -> wrapper
  | Return_unset  ->
    if Ast_core_type.is_unit result_type then
      Return_replaced_with_unit
    else
      wrapper
  | Return_undefined_to_opt
  | Return_null_to_opt
  | Return_null_undefined_to_opt
    ->
    if Ast_core_type.is_user_option result_type then
      wrapper
    else
      Bs_syntaxerr.err loc Expect_opt_in_bs_return_to_opt
  | Return_replaced_with_unit ->
    assert false (* Not going to happen from user input*)



type response = {
  pval_type : Parsetree.core_type ; 
  pval_prim : string list ; 
  pval_attributes : Parsetree.attributes;
  no_inline_cross_module : bool 
}



let process_obj 
    (loc : Location.t)
    (st : external_desc) 
    (prim_name : string)   
    (arg_types_ty : Ast_compatible.param_type list)
    (result_type : Ast_core_type.t)
  : Parsetree.core_type *  External_ffi_types.t 
  = 
  match st with
  | {
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
    return_wrapper = Return_unset ;
    set_index = false ;
    mk_obj = _;
    scopes = [];
    (* wrapper does not work with [bs.obj]
       TODO: better error message *)
  } ->
    if String.length prim_name <> 0 then
      Location.raise_errorf ~loc "[@@bs.obj] expect external names to be empty string";
    let arg_kinds, new_arg_types_ty, result_types =
      Ext_list.fold_right arg_types_ty ( [], [], [])
        (fun param_type ( arg_labels, (arg_types : Ast_compatible.param_type list), result_types) ->
           let arg_label = Ast_compatible.convert param_type.label in
           let ty  = param_type.ty in 
           let new_arg_label, new_arg_types,  output_tys =
             match arg_label with
             | Nolabel ->
               let new_ty, arg_type = refine_arg_type ~nolabel:true  ty in
               if arg_type = Extern_unit then
                 External_arg_spec.empty_kind arg_type, 
                 {param_type with ty = new_ty}::arg_types, result_types
               else
                 Location.raise_errorf ~loc "expect label, optional, or unit here"
             | Labelled name ->
               let new_ty, arg_type = refine_arg_type ~nolabel:false  ty in
               begin match arg_type with
                 | Ignore ->
                   External_arg_spec.empty_kind arg_type,
                   {param_type with ty = new_ty}::arg_types, result_types
                 | Arg_cst  i  ->
                   let s = Lam_methname.translate ~loc name in
                   {arg_label = External_arg_spec.label s (Some i);
                    arg_type },
                   arg_types, (* ignored in [arg_types], reserved in [result_types] *)
                   ((name , [], new_ty) :: result_types)
                 | Nothing  ->
                   let s = (Lam_methname.translate ~loc name) in
                   {arg_label = External_arg_spec.label s None ; arg_type },
                   {param_type with ty = new_ty}::arg_types,
                   ((name , [], new_ty) :: result_types)
                 | Int _  ->
                   let s = Lam_methname.translate ~loc name in
                   {arg_label = External_arg_spec.label s None; arg_type},
                   {param_type with ty = new_ty}::arg_types,
                   ((name, [], Ast_literal.type_int ~loc ()) :: result_types)
                 | NullString _ ->
                   let s = Lam_methname.translate ~loc name in
                   {arg_label = External_arg_spec.label s None; arg_type},
                   {param_type with ty = new_ty }::arg_types,
                   ((name, [], Ast_literal.type_string ~loc ()) :: result_types)
                 | Fn_uncurry_arity _ ->
                   Location.raise_errorf ~loc
                     "The combination of [@@bs.obj], [@@bs.uncurry] is not supported yet"
                 | Extern_unit -> assert false
                 | NonNullString _
                   ->
                   Location.raise_errorf ~loc
                     "bs.obj label %s does not support such arg type" name
                 | Unwrap ->
                   Location.raise_errorf ~loc
                     "bs.obj label %s does not support [@bs.unwrap] arguments" name
               end
             | Optional name ->
               let arg_type = get_opt_arg_type ~nolabel:false  ty in
               begin match arg_type with
                 | Ignore ->
                   External_arg_spec.empty_kind arg_type,
                   param_type::arg_types, result_types
                 | Nothing ->
                   let s = (Lam_methname.translate ~loc name) in
                   {arg_label = External_arg_spec.optional s; arg_type},
                   param_type :: arg_types,
                   ( (name, [], Ast_comb.to_undefined_type loc (get_basic_type_from_option_label ty)) ::  result_types)
                 | Int _  ->
                   let s = Lam_methname.translate ~loc name in
                   {arg_label = External_arg_spec.optional s ; arg_type },
                   param_type :: arg_types,
                   ((name, [], Ast_comb.to_undefined_type loc @@ Ast_literal.type_int ~loc ()) :: result_types)
                 | NullString _  ->
                   let s = Lam_methname.translate ~loc name in
                   {arg_label = External_arg_spec.optional s ; arg_type },
                   param_type::arg_types,
                   ((name, [], Ast_comb.to_undefined_type loc @@ Ast_literal.type_string ~loc ()) :: result_types)
                 | Arg_cst _
                   ->
                   Location.raise_errorf ~loc "bs.as is not supported with optional yet"
                 | Fn_uncurry_arity _ ->
                   Location.raise_errorf ~loc
                     "The combination of [@@bs.obj], [@@bs.uncurry] is not supported yet"
                 | Extern_unit   -> assert false
                 | NonNullString _
                   ->
                   Location.raise_errorf ~loc
                     "bs.obj label %s does not support such arg type" name
                 | Unwrap ->
                   Location.raise_errorf ~loc
                     "bs.obj label %s does not support [@bs.unwrap] arguments" name
               end
           in
           new_arg_label::arg_labels,
           new_arg_types,
           output_tys) in

    let result =
      if Ast_core_type.is_any  result_type then
        Ast_core_type.make_obj ~loc result_types
      else
        fst (refine_arg_type ~nolabel:true result_type) 
        (* result type can not be labeled *)
    in
    Ast_compatible.mk_fn_type new_arg_types_ty result,
    External_ffi_types.Ffi_obj_create arg_kinds
  | _ -> Location.raise_errorf ~loc "Attribute found that conflicts with [@@bs.obj]"


let external_desc_of_non_obj 
    (loc : Location.t) 
    (st : external_desc) 
    (prim_name_or_pval_prim : bundle_source)
    (arg_type_specs_length : int) 
    arg_types_ty 
    (arg_type_specs : External_arg_spec.t list) : External_ffi_types.external_spec =
  match st with
  | {set_index = true;
     val_name = `Nm_na;
     external_module_name = None ;
     module_as_val = None;
     val_send = `Nm_na;
     val_send_pipe = None;
     splice = false;
     scopes ;
     get_index = false;
     new_name = `Nm_na;
     call_name = `Nm_na;
     set_name = `Nm_na ;
     get_name = `Nm_na ;

     return_wrapper = _;
     mk_obj = _ ;

    }
    ->
    if arg_type_specs_length = 3 then
      Js_set_index {js_set_index_scopes = scopes}
    else
      Location.raise_errorf ~loc "Ill defined attribute [@@bs.set_index](arity of 3)"
  | {set_index = true; _} ->
    Bs_syntaxerr.err loc (Conflict_ffi_attribute "Attribute found that conflicts with [@@bs.set_index]")
  | {get_index = true;
     val_name = `Nm_na;
     external_module_name = None ;
     module_as_val = None;
     val_send = `Nm_na;
     val_send_pipe = None;

     splice = false;
     scopes ;
     new_name = `Nm_na;
     call_name = `Nm_na;
     set_name = `Nm_na ;
     get_name = `Nm_na ;
     set_index = false;
     mk_obj;
     return_wrapper ;
    } ->
    if arg_type_specs_length = 2 then
      Js_get_index {js_get_index_scopes = scopes}
    else Location.raise_errorf ~loc
        "Ill defined attribute [@@bs.get_index] (arity expected 2 : while %d)" arg_type_specs_length

  | {get_index = true; _} ->
    Bs_syntaxerr.err loc (Conflict_ffi_attribute "Attribute found that conflicts with [@@bs.get_index]")
  | {module_as_val = Some external_module_name ;

     get_index = false;
     val_name ;
     new_name ;

     external_module_name = None ;
     val_send = `Nm_na;
     val_send_pipe = None;
     scopes = []; (* module as var does not need scopes *)
     splice;
     call_name = `Nm_na;
     set_name = `Nm_na ;
     get_name = `Nm_na ;
     set_index = false;
     return_wrapper = _;
     mk_obj = _ ;
    } ->
    begin match arg_types_ty, new_name, val_name  with
      | [], `Nm_na,  _ -> Js_module_as_var external_module_name
      | _, `Nm_na, _ -> Js_module_as_fn {splice; external_module_name }
      | _, #bundle_source, #bundle_source ->
        Bs_syntaxerr.err loc (Conflict_ffi_attribute "Attribute found that conflicts with [@@bs.module].")

      | _, (`Nm_val _ | `Nm_external _) , `Nm_na
        -> Js_module_as_class external_module_name
      | _, `Nm_payload _ , `Nm_na
        ->
        Location.raise_errorf ~loc
          "Incorrect FFI attribute found: (bs.new should not carry a payload here)"
    end
  | {module_as_val = Some x; _} ->
    Bs_syntaxerr.err loc (Conflict_ffi_attribute "Attribute found that conflicts with [@@bs.module].")
  | {call_name = (`Nm_val name | `Nm_external name | `Nm_payload name) ;
     splice;
     scopes ;
     external_module_name;

     val_name = `Nm_na ;
     module_as_val = None;
     val_send = `Nm_na ;
     val_send_pipe = None;

     set_index = false;
     get_index = false;
     new_name = `Nm_na;
     set_name = `Nm_na ;
     get_name = `Nm_na ;
     mk_obj = _ ;
     return_wrapper = _ ;
    } ->
    Js_call {splice; name; external_module_name; scopes }
  | {call_name = #bundle_source ; _ }
    ->
    Bs_syntaxerr.err loc (Conflict_ffi_attribute "Attribute found that conflicts with [@@bs.val]")
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
     get_name = `Nm_na;
     mk_obj = _;
     return_wrapper = _;
     splice = false ;
     scopes ;
    }
    -> (* 
    if no_arguments -->
          {[
            external ff : int = "" [@@bs.val]
          ]}
       *)
    Js_var { name; external_module_name; scopes}
  | {val_name = #bundle_source ; _ }
    ->
    Bs_syntaxerr.err loc (Conflict_ffi_attribute "Attribute found that conflicts with [@@bs.val]")

  | {splice ;
     scopes ;
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
     mk_obj = _ ;
     return_wrapper= _ ;
    }
    ->
    let name = string_of_bundle_source prim_name_or_pval_prim in
    if arg_type_specs_length  = 0 then
      (*
         {[
           external ff : int = "" [@@bs.module "xx"]
         ]}
      *)
      Js_var { name; external_module_name; scopes}
    else  Js_call {splice; name; external_module_name; scopes}
  | {val_send = (`Nm_val name | `Nm_external name | `Nm_payload name);
     splice;
     scopes;
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
     mk_obj = _ ;
     return_wrapper = _ ;
    } ->
    (* PR #2162 - since when we assemble arguments the first argument in
       [@@bs.send] is ignored
    *)
    begin match arg_type_specs with
      | [] ->
        Location.raise_errorf
          ~loc "Ill defined attribute [@@bs.send] (the external needs to be a regular function call with at least one argument)"
      |  {arg_type = Arg_cst _ ; arg_label = _} :: _
        ->
        Location.raise_errorf
          ~loc "Ill defined attribute [@@bs.send] (first argument can't be const)"
      | _ :: _  ->
        Js_send {splice ; name; js_send_scopes = scopes ;  pipe = false}
    end
  | {val_send = #bundle_source; _ }
    -> Location.raise_errorf ~loc "You used a FFI attribute that can't be used with [@@bs.send]"
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
     mk_obj = _;
     return_wrapper = _;
     scopes;
     splice ;
    } ->
    (** can be one argument *)
    Js_send {splice  ;
             name = string_of_bundle_source prim_name_or_pval_prim;
             js_send_scopes = scopes;
             pipe = true}

  | {val_send_pipe = Some _ ; _}
    -> Location.raise_errorf ~loc "conflict attributes found with [@@bs.send.pipe]"

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
     splice = false;
     scopes;
     mk_obj = _ ;
     return_wrapper = _ ;
    }
    -> Js_new {name; external_module_name;  scopes}
  | {new_name = #bundle_source ; _ } ->
    Bs_syntaxerr.err loc (Conflict_ffi_attribute "Attribute found that conflicts with [@@bs.new]")
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
     external_module_name = None;
     splice = false;
     mk_obj = _ ;
     return_wrapper = _;
     scopes ;
    }
    ->
    if arg_type_specs_length = 2 then
      Js_set { js_set_scopes = scopes ; js_set_name = name}
    else  Location.raise_errorf ~loc "Ill defined attribute [@@bs.set] (two args required)"
  | {set_name = #bundle_source; _}
    -> Location.raise_errorf ~loc "conflict attributes found with [@@bs.set]"
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
     external_module_name = None;
     splice = false ;
     mk_obj = _;
     return_wrapper = _;
     scopes
    }
    ->
    if arg_type_specs_length = 1 then
      Js_get { js_get_name = name; js_get_scopes = scopes }
    else
      Location.raise_errorf ~loc "Ill defined attribute [@@bs.get] (only one argument)"
  | {get_name = #bundle_source; _}
    -> Location.raise_errorf ~loc "Attribute found that conflicts with [@@bs.get]"

  | {get_name = `Nm_na;
     val_name = `Nm_na  ;
     call_name = `Nm_na ;
     module_as_val = None;
     set_index = false;
     get_index = false;
     val_send = `Nm_na ;
     val_send_pipe = None;
     new_name = `Nm_na ;
     set_name = `Nm_na ;
     external_module_name = None;
     splice = _ ;
     scopes = _;
     mk_obj = _;
     return_wrapper = _;

    }
    ->  Location.raise_errorf ~loc "Could not infer which FFI category it belongs to, maybe you forgot [%@%@bs.val]? "  

(** Note that the passed [type_annotation] is already processed by visitor pattern before*)
let handle_attributes
    (loc : Bs_loc.t)
    (type_annotation : Parsetree.core_type)
    (prim_attributes : Ast_attributes.t) 
    (pval_name : string )
    (prim_name : string)
  : Parsetree.core_type *  External_ffi_types.t * Parsetree.attributes * bool
  =
  (** sanity check here
      {[ int -> int -> (int -> int -> int [@bs.uncurry])]}
      It does not make sense
  *)
  if has_bs_uncurry type_annotation.ptyp_attributes then
    Location.raise_errorf
      ~loc "[@@bs.uncurry] can not be applied to the whole definition";
  let prim_name_or_pval_name =
    if String.length prim_name = 0 then  `Nm_val pval_name
    else  `Nm_external prim_name  (* need check name *) in
  let result_type, arg_types_ty =
    (* Note this assumes external type is syntatic (no abstraction)*)
    Ast_core_type.list_of_arrow type_annotation in
  if has_bs_uncurry result_type.ptyp_attributes then
    Location.raise_errorf
      ~loc:result_type.ptyp_loc
      "[@@bs.uncurry] can not be applied to tailed position";
  let no_arguments = arg_types_ty = [] in  
  let unused_attrs, external_desc =
    parse_external_attributes no_arguments  
      prim_name prim_name_or_pval_name  prim_attributes in
  if external_desc.mk_obj then
    (* warn unused attributes here ? *)
    let new_type, spec = process_obj loc external_desc prim_name arg_types_ty result_type in 
    new_type, spec, unused_attrs, false
  else
    let splice = external_desc.splice in
    let arg_type_specs, new_arg_types_ty, arg_type_specs_length   =
      let init : External_arg_spec.t list * Ast_compatible.param_type list * int  = 
        match external_desc.val_send_pipe with
        | Some obj ->
          let new_ty, arg_type = refine_arg_type ~nolabel:true obj in
          begin match arg_type with
            | Arg_cst _ ->
              Location.raise_errorf ~loc:obj.ptyp_loc "[@bs.as] is not supported in bs.send type "
            | _ ->
              (* more error checking *)
              [External_arg_spec.empty_kind arg_type],
              [{label = Ast_compatible.no_label;
                ty = new_ty;
                attr =  [];
                loc = obj.ptyp_loc} ],
              0           
          end
        | None -> [],[], 0 in 
      Ext_list.fold_right arg_types_ty init
        (fun  param_type (arg_type_specs, arg_types, i) ->
           let arg_label = Ast_compatible.convert param_type.label in
           let ty = param_type.ty in 
           if i = 0 && splice  then
             begin match arg_label with 
               | Optional _ -> 
                 Location.raise_errorf ~loc "[@@@@bs.splice] expect the last type to be a non optional"
               | Labelled _ | Nolabel 
                -> 
                if Ast_core_type.is_any ty then 
                  Location.raise_errorf ~loc "[@@@@bs.splice] expect the last type to be an array";                  
                if spec_of_ptyp true ty <> Nothing then 
                  Location.raise_errorf ~loc "[@@@@bs.splice] expect the last type to be an array";
                match ty.ptyp_desc with 
                | Ptyp_constr({txt = Lident "array"; _}, [_])
                  -> ()
                | _ -> Location.raise_errorf ~loc "[@@@@bs.splice] expect the last type to be an array";
             end ; 
           let arg_label, arg_type, new_arg_types =
             match arg_label with
             | Optional s  ->
               let arg_type = get_opt_arg_type ~nolabel:false ty in
               begin match arg_type with
                 | NonNullString _ ->
                   (* ?x:([`x of int ] [@bs.string]) does not make sense *)
                   Location.raise_errorf
                     ~loc
                     "[@@bs.string] does not work with optional when it has arities in label %s" s
                 | _ ->
                   External_arg_spec.optional s, arg_type,
                   param_type :: arg_types end
             | Labelled s  ->
               begin match refine_arg_type ~nolabel:false ty with
                 | new_ty, (Arg_cst i as arg_type)  ->
                   External_arg_spec.label s (Some i), arg_type, arg_types
                 | new_ty, arg_type ->
                   External_arg_spec.label s None, arg_type, 
                   {param_type with ty = new_ty} :: arg_types
               end
             | Nolabel ->
               begin match refine_arg_type ~nolabel:true ty with
                 | new_ty , (Arg_cst i as arg_type) ->
                   External_arg_spec.empty_lit i , arg_type,  arg_types
                 | new_ty , arg_type ->
                   External_arg_spec.empty_label, arg_type, {param_type with ty = new_ty} :: arg_types
               end
           in
           ({ arg_label  ;
              arg_type
            } :: arg_type_specs,
            new_arg_types,
            if arg_type = Ignore then i
            else i + 1
           )
        )  in
    let ffi : External_ffi_types.external_spec  = 
      external_desc_of_non_obj 
        loc external_desc prim_name_or_pval_name arg_type_specs_length 
        arg_types_ty arg_type_specs in 
    let relative = External_ffi_types.check_ffi ~loc ffi in 
    (* result type can not be labeled *)
    (* currently we don't process attributes of
       return type, in the future we may  *)
    let return_wrapper = check_return_wrapper loc external_desc.return_wrapper result_type in
    Ast_compatible.mk_fn_type new_arg_types_ty result_type,  
    Ffi_bs (arg_type_specs, return_wrapper, ffi),
    unused_attrs,
    relative 



let handle_attributes_as_string
    (pval_loc : Location.t)
    (typ : Ast_core_type.t) 
    (attrs : Ast_attributes.t) 
    (pval_name : string)
    (prim_name : string) 
  : response =
  let pval_type, ffi, pval_attributes, no_inline_cross_module  =
    handle_attributes pval_loc typ attrs pval_name prim_name  in
  { pval_type;
    pval_prim = [prim_name; External_ffi_types.to_string ffi];
    pval_attributes;
    no_inline_cross_module 
  }



let pval_prim_of_labels (labels : string Asttypes.loc list) =
  let arg_kinds =
    Ext_list.fold_right labels ([] : External_arg_spec.t list ) 
      (fun {loc ; txt } arg_kinds
        ->
          let arg_label =
            External_arg_spec.label
              (Lam_methname.translate ~loc txt) None in
          {arg_type = Nothing ;
           arg_label  } :: arg_kinds
      ) in
  let encoding =
    External_ffi_types.to_string (Ffi_obj_create arg_kinds) in
  [""; encoding]

let pval_prim_of_option_labels
    (labels : (bool * string Asttypes.loc) list)
    (ends_with_unit : bool)
  =
  let arg_kinds =
    Ext_list.fold_right labels
      (if ends_with_unit then
         [External_arg_spec.empty_kind Extern_unit]
       else [])
      (fun (is_option,{loc ; txt }) arg_kinds
        ->
          let label_name = Lam_methname.translate ~loc txt in
          let arg_label =
            if is_option then
              External_arg_spec.optional label_name
            else External_arg_spec.label label_name None
          in
          {arg_type = Nothing ;
           arg_label  } :: arg_kinds) in
  let encoding =
    External_ffi_types.to_string (Ffi_obj_create arg_kinds) in
  [""; encoding]

