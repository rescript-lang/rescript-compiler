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



let variant_can_bs_unwrap_fields row_fields =
  let validity = 
    List.fold_left
      begin fun st row ->
        match st, row with
        | (* we've seen no fields or only valid fields so far *)
          (`No_fields | `Valid_fields),
          (* and this field has one constructor arg that we can unwrap to *)
          Parsetree.Rtag (label, attrs, false, ([ _ ]))
          ->
          `Valid_fields
        | (* otherwise, this field or a previous field was invalid *)
          _ ->
          `Invalid_field
      end
      `No_fields
      row_fields
  in
  match validity with
  | `Valid_fields -> true
  | `No_fields
  | `Invalid_field -> false

(** Given the type of argument, process its [bs.] attribute and new type,
    The new type is currently used to reconstruct the external type 
    and result type in [@@bs.obj]
    They are not the same though, for example
    {[
      external f : hi:([ `hi | `lo ] [@bs.string]) -> unit -> _ = "" [@@bs.obj]
    ]}
    The result type would be [ hi:string ]
*)
let get_arg_type ~nolabel optional 
    (ptyp : Ast_core_type.t) : 
  External_arg_spec.attr * Ast_core_type.t  = 
  let ptyp = if optional then Ast_core_type.extract_option_type_exn ptyp else ptyp in 
  if Ast_core_type.is_any ptyp then (* (_[@bs.as ])*)
    if optional then 
      Bs_syntaxerr.err ptyp.ptyp_loc Invalid_underscore_type_in_external
    else begin
      match Ast_attributes.process_bs_string_or_int_as ptyp.Parsetree.ptyp_attributes with 
      |  None, _ -> 
        Bs_syntaxerr.err ptyp.ptyp_loc Invalid_underscore_type_in_external

      | Some (`Int i), others -> 
        Ast_attributes.warn_unused_attributes others;
        Arg_cst(External_arg_spec.cst_int i), Ast_literal.type_int ~loc:ptyp.ptyp_loc ()  
      | Some (`Str i), others -> 
        Ast_attributes.warn_unused_attributes others;
        Arg_cst (External_arg_spec.cst_string i), Ast_literal.type_string ~loc:ptyp.ptyp_loc () 
      | Some (`Json_str s), others ->
        Ast_attributes.warn_unused_attributes others;
        Arg_cst (External_arg_spec.cst_json ptyp.ptyp_loc s),
        Ast_literal.type_string ~loc:ptyp.ptyp_loc () 

    end 
  else (* ([`a|`b] [@bs.string]) *)
    match Ast_attributes.process_bs_string_int_unwrap_uncurry ptyp.ptyp_attributes, ptyp.ptyp_desc with
    | (`String, ptyp_attributes),  Ptyp_variant ( row_fields, Closed, None)
      -> 
      let case, result, row_fields  = 
        (Ext_list.fold_right (fun tag (nullary, acc, row_fields) -> 
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
             | _ -> Bs_syntaxerr.err ptyp.ptyp_loc Invalid_bs_string_type

           ) row_fields (`Nothing, [], [])) in 
      (match case with 
       | `Nothing -> Bs_syntaxerr.err ptyp.ptyp_loc Invalid_bs_string_type
       | `Null -> NullString result 
       | `NonNull -> NonNullString result) , 
      {ptyp with ptyp_desc = Ptyp_variant(row_fields, Closed, None);
                 ptyp_attributes ;
      }
    | (`String, _),  _ ->
      Bs_syntaxerr.err ptyp.ptyp_loc Invalid_bs_string_type
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

              | _ -> 
                Bs_syntaxerr.err ptyp.ptyp_loc Invalid_bs_int_type

           ) (0, [],[]) row_fields) in 
      Int (List.rev acc),
      {ptyp with 
       ptyp_desc = Ptyp_variant(List.rev rev_row_fields, Closed, None );
       ptyp_attributes
      }
    | (`Int, _), _ -> Bs_syntaxerr.err ptyp.ptyp_loc Invalid_bs_int_type
    | (`Unwrap, ptyp_attributes), (Ptyp_variant (row_fields, Closed, _) as ptyp_desc)
      when variant_can_bs_unwrap_fields row_fields
      ->
      Unwrap, {ptyp with ptyp_desc; ptyp_attributes}
    | (`Unwrap, _), _ ->
      Bs_syntaxerr.err ptyp.ptyp_loc Invalid_bs_unwrap_type
    | (`Uncurry opt_arity, ptyp_attributes), ptyp_desc -> 
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

       end, {ptyp with ptyp_attributes})
    | (`Nothing, ptyp_attributes),  ptyp_desc ->
      begin match ptyp_desc with
        | Ptyp_constr ({txt = Lident "bool"; _}, [])
          -> 
          Bs_warnings.prerr_warning ptyp.ptyp_loc Unsafe_ffi_bool_type;
          Nothing
        | Ptyp_constr ({txt = Lident "unit"; _}, [])
          -> if nolabel then Extern_unit else  Nothing
        | Ptyp_constr ({txt = Lident "array"; _}, [_])
          -> Array
        | Ptyp_variant _ ->
          Bs_warnings.prerr_warning ptyp.ptyp_loc Unsafe_poly_variant_type;
          Nothing           
        | _ ->
          Nothing           
      end, ptyp



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




type st = 
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





let process_external_attributes 
    no_arguments 
    (prim_name_or_pval_prim: [< bundle_source ] as 'a)
    pval_prim
    (prim_attributes : Ast_attributes.t) : _ * Ast_attributes.t =

  (* shared by `[@@bs.val]`, `[@@bs.send]`, 
     `[@@bs.set]`, `[@@bs.get]` , `[@@bs.new]` 
     `[@@bs.send.pipe]` does not use it 
  *)
  let name_from_payload_or_prim ~loc (payload : Parsetree.payload) : name_source =
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
  List.fold_left 
    (fun (st, attrs)
      (({txt ; loc}, payload) as attr : Ast_attributes.attr) 
      ->
        if Ext_string.starts_with txt "bs." then
          begin match txt with 
            | "bs.val" ->  
              if no_arguments then
                {st with val_name = name_from_payload_or_prim ~loc payload}
              else 
                {st with call_name = name_from_payload_or_prim ~loc  payload}

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
            | "bs.splice" -> {st with splice = true}
            | "bs.send" -> 
              { st with val_send = name_from_payload_or_prim ~loc payload}
            | "bs.send.pipe"
              ->
              { st with val_send_pipe = Some (Ast_payload.as_core_type loc payload)}                
            | "bs.set" -> 
              {st with set_name = name_from_payload_or_prim ~loc  payload}
            | "bs.get" -> {st with get_name = name_from_payload_or_prim ~loc payload}

            | "bs.new" -> {st with new_name = name_from_payload_or_prim ~loc payload}
            | "bs.set_index" -> {st with set_index = true}
            | "bs.get_index"-> {st with get_index = true}
            | "bs.obj" -> {st with mk_obj = true}
            | "bs.return" ->
              let aux loc txt : External_ffi_types.return_wrapper = 
                begin match txt with 
                  | "undefined_to_opt" -> Return_undefined_to_opt
                  | "null_to_opt" -> Return_null_to_opt
                  | "nullable"
                  | "null_undefined_to_opt" -> Return_null_undefined_to_opt
                  | "identity" -> Return_identity 
                  | _ ->
                    Bs_syntaxerr.err loc Not_supported_directive_in_bs_return
                  end in
                  let actions = 
                    Ast_payload.ident_or_record_as_config loc payload 
                  in
                  begin match actions with 
                    | [ ({txt; _ },None) ] -> 
                      { st with return_wrapper = aux loc txt}
                    | _ ->
                      Bs_syntaxerr.err loc Not_supported_directive_in_bs_return
                  end
            | _ -> (Bs_warnings.warn_unused_attribute loc txt; st)
          end, attrs
        else (st , attr :: attrs)
    )
    (init_st, []) prim_attributes 


let rec has_bs_uncurry (attrs : Ast_attributes.t) = 
  match attrs with 
  | ({txt = "bs.uncurry"; _ }, _) :: attrs -> 
    true 
  | _ :: attrs -> has_bs_uncurry attrs 
  | [] -> false 


let check_return_wrapper 
    loc (wrapper : External_ffi_types.return_wrapper) 
    result_type = 
  match wrapper with 
  | Return_identity -> wrapper
  | Return_unset  ->         
    if Ast_core_type.is_unit result_type then 
      Return_replaced_with_unit 
    else if Ast_core_type.is_user_bool result_type then 
      Return_to_ocaml_bool
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
  | Return_replaced_with_unit 
  | Return_to_ocaml_bool  -> 
    assert false (* Not going to happen from user input*)




(** Note that the passed [type_annotation] is already processed by visitor pattern before 
*)
let handle_attributes 
    (loc : Bs_loc.t)
    (pval_prim : string ) 
    (type_annotation : Parsetree.core_type)
    (prim_attributes : Ast_attributes.t) (prim_name : string)
  : Ast_core_type.t * string * External_ffi_types.t * Ast_attributes.t =
  (** sanity check here 
      {[ int -> int -> (int -> int -> int [@bs.uncurry])]}
      It does not make sense 
  *)
  if has_bs_uncurry type_annotation.Parsetree.ptyp_attributes then 
    begin 
      Location.raise_errorf 
        ~loc "[@@bs.uncurry] can not be applied to the whole definition"
    end; 

  let prim_name_or_pval_prim =
    if String.length prim_name = 0 then  `Nm_val pval_prim
    else  `Nm_external prim_name  (* need check name *)
  in    
  let result_type, arg_types_ty =
    Ast_core_type.list_of_arrow type_annotation in
  if has_bs_uncurry result_type.ptyp_attributes then 
    begin 
      Location.raise_errorf 
        ~loc:result_type.ptyp_loc
        "[@@bs.uncurry] can not be applied to tailed position"
    end ;
  let (st, left_attrs) = 
    process_external_attributes 
      (arg_types_ty = [])
      prim_name_or_pval_prim pval_prim prim_attributes in 


  if st.mk_obj then 
    begin match st with 
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
          Ext_list.fold_right 
            (fun (label,ty,attr,loc) ( arg_labels, arg_types, result_types) -> 
               let arg_label = Ast_core_type.label_name label in 
               let new_arg_label, new_arg_types,  output_tys = 
                 match arg_label with 
                 | Empty -> 
                   let arg_type, new_ty = get_arg_type ~nolabel:true false ty in 
                   begin match arg_type with 
                     | Extern_unit ->  
                       External_arg_spec.empty_kind arg_type, (label,new_ty,attr,loc)::arg_types, result_types
                     | _ ->  
                       Location.raise_errorf ~loc "expect label, optional, or unit here"
                   end 
                 | Label name -> 
                   let arg_type, new_ty = get_arg_type ~nolabel:false false ty in 
                   begin match arg_type with 
                     | Ignore -> 
                       External_arg_spec.empty_kind arg_type, 
                       (label,new_ty,attr,loc)::arg_types, result_types
                     | Arg_cst  i  -> 
                       let s = (Lam_methname.translate ~loc name) in
                       {arg_label = External_arg_spec.label s (Some i);
                        arg_type }, 
                       arg_types, (* ignored in [arg_types], reserved in [result_types] *)
                       ((name , [], new_ty) :: result_types)
                     | Nothing | Array -> 
                       let s = (Lam_methname.translate ~loc name) in
                       {arg_label = External_arg_spec.label s None ; arg_type },
                       (label,new_ty,attr,loc)::arg_types, 
                       ((name , [], new_ty) :: result_types)
                     | Int _  -> 
                       let s = Lam_methname.translate ~loc name in
                       {arg_label = External_arg_spec.label s None; arg_type},
                       (label,new_ty,attr,loc)::arg_types, 
                       ((name, [], Ast_literal.type_int ~loc ()) :: result_types)  
                     | NullString _ -> 
                       let s = Lam_methname.translate ~loc name in
                       {arg_label = External_arg_spec.label s None; arg_type}, 
                       (label,new_ty,attr,loc)::arg_types, 
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
                   let arg_type, new_ty_extract = get_arg_type ~nolabel:false true ty in 
                   let new_ty = Ast_core_type.lift_option_type new_ty_extract in 
                   begin match arg_type with 
                     | Ignore -> 
                       External_arg_spec.empty_kind arg_type, 
                       (label,new_ty,attr,loc)::arg_types, result_types

                     | Nothing | Array -> 
                       let s = (Lam_methname.translate ~loc name) in 
                       {arg_label = External_arg_spec.optional s; arg_type}, 
                       (label,new_ty,attr,loc)::arg_types, 
                       ( (name, [], Ast_comb.to_undefined_type loc new_ty_extract) ::  result_types)
                     | Int _  -> 
                       let s = Lam_methname.translate ~loc name in 
                       {arg_label = External_arg_spec.optional s ; arg_type },
                       (label,new_ty,attr,loc)::arg_types,
                       ((name, [], Ast_comb.to_undefined_type loc @@ Ast_literal.type_int ~loc ()) :: result_types)                      
                     | NullString _  -> 
                       let s = Lam_methname.translate ~loc name in 
                       {arg_label = External_arg_spec.optional s ; arg_type }, 
                       (label,new_ty,attr,loc)::arg_types,
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
               (
                 new_arg_label::arg_labels,
                 new_arg_types,
                 output_tys)) arg_types_ty 
            ( [], [], []) in 

        let result = 
          if Ast_core_type.is_any  result_type then            
            Ast_core_type.make_obj ~loc result_types 
          else           
            snd @@ get_arg_type ~nolabel:true false result_type (* result type can not be labeled *)            

        in
        begin 
          (             
            Ext_list.fold_right (fun (label,ty,attrs,loc) acc -> 
                Ast_helper.Typ.arrow ~loc  ~attrs label ty acc 
              ) new_arg_types_ty result
          ) ,
          prim_name,
          Ffi_obj_create arg_kinds,
          left_attrs
        end

      | _ -> Location.raise_errorf ~loc "Attribute found that conflicts with [@@bs.obj]"  

    end  

  else   
    let splice = st.splice in 
    let arg_type_specs, new_arg_types_ty, arg_type_specs_length   = 
      Ext_list.fold_right 
        (fun (label,ty,attr,loc) (arg_type_specs, arg_types, i) -> 
           let arg_label = Ast_core_type.label_name label in 
           let arg_label, arg_type, new_arg_types = 
             match arg_label with 
             | Optional s  -> 

               let arg_type , new_ty = get_arg_type ~nolabel:false true ty in 
               begin match arg_type with 
                 | NonNullString _ -> 
                   (* ?x:([`x of int ] [@bs.string]) does not make sense *)
                   Location.raise_errorf 
                     ~loc
                     "[@@bs.string] does not work with optional when it has arities in label %s" label
                 | _ -> 
                   External_arg_spec.optional s, arg_type, 
                   ((label, Ast_core_type.lift_option_type new_ty , attr,loc) :: arg_types) end
             | Label s  -> 
               begin match get_arg_type ~nolabel:false false  ty with
                 | (Arg_cst ( i) as arg_type), new_ty -> 
                   External_arg_spec.label s (Some i), arg_type, arg_types
                 | arg_type, new_ty -> 
                   External_arg_spec.label s None, arg_type, (label, new_ty,attr,loc) :: arg_types
               end
             | Empty -> 
               begin match get_arg_type ~nolabel:true false  ty with 
                 | (Arg_cst ( i) as arg_type), new_ty -> 
                   External_arg_spec.empty_lit i , arg_type,  arg_types
                 | arg_type, new_ty -> 
                   External_arg_spec.empty_label, arg_type, (label, new_ty,attr,loc) :: arg_types
               end
           in
           (if i = 0 && splice  then
              match arg_type with 
              | Array  -> ()
              | _ ->  Location.raise_errorf ~loc "[@@@@bs.splice] expect the last type to be an array");
           ({ External_arg_spec.arg_label  ; 
              arg_type 
            } :: arg_type_specs,
            new_arg_types,
            if arg_type = Ignore then i 
            else i + 1
           )
        ) arg_types_ty 
        (match st with
         | {val_send_pipe = Some obj; _ } ->      
           let arg_type, new_ty = get_arg_type ~nolabel:true false obj in 
           begin match arg_type with 
             | Arg_cst _ -> 
               Location.raise_errorf ~loc:obj.ptyp_loc "[@bs.as] is not supported in bs.send type "
             | _ -> 
               (* more error checking *)
               [External_arg_spec.empty_kind arg_type]
               ,
               ["", new_ty, [], obj.ptyp_loc]
               ,0
           end

         | {val_send_pipe = None ; _ } -> [],[], 0) in 

    let ffi : External_ffi_types.attr  = match st with           
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
        if String.length prim_name <> 0 then 
          Location.raise_errorf ~loc "[@@bs.set_index] expect external names to be empty string";
        if arg_type_specs_length = 3 then 
          Js_set_index {js_set_index_scopes = scopes}
        else 
          Location.raise_errorf ~loc "Ill defined attribute [@@bs.set_index](arity of 3)"

      | {set_index = true; _}
        ->
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
        if String.length prim_name <> 0 then 
          Location.raise_errorf ~loc "[@@bs.get_index] expect external names to be empty string";
        if arg_type_specs_length = 2 then 
          Js_get_index {js_get_index_scopes = scopes}
        else Location.raise_errorf ~loc 
            "Ill defined attribute [@@bs.get_index] (arity expected 2 : while %d)" arg_type_specs_length

      | {get_index = true; _}

        -> 
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
      | {module_as_val = Some x; _}
        -> 
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
        -> 
        Js_global { name; external_module_name; scopes}
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
          Js_global { name; external_module_name; scopes}
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
              ~loc "Ill defined attribute [@@bs.send] (at least one argument)"
          |  {arg_type = Arg_cst _ ; arg_label = _} :: _
            -> 
            Location.raise_errorf 
              ~loc "Ill defined attribute [@@bs.send] (first argument can not be const)"
          | _ :: _  -> 
            Js_send {splice ; name; js_send_scopes = scopes ;  pipe = false}
        end

      | {val_send = #bundle_source; _ } 
        -> Location.raise_errorf ~loc "You used an FFI attribute that can't be used with [@@bs.send]"

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
         splice ;
         scopes; 
         mk_obj = _ ; 
         return_wrapper = _ ; 

        } 
        -> Js_new {name; external_module_name; splice; scopes}
      | {new_name = #bundle_source ; _ }
        -> 
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
        ->  Location.raise_errorf ~loc "Could not infer which FFI category it belongs to, maybe you forgot [%@%@bs.val]? "  in 
    begin 
      External_ffi_types.check_ffi ~loc ffi;
      (* result type can not be labeled *)
      (* currently we don't process attributes of 
         return type, in the future we may  *)
      let  new_result_type  =  result_type in
      (* get_arg_type ~nolabel:true false result_type in *)
      let return_wrapper : External_ffi_types.return_wrapper = 
        check_return_wrapper loc st.return_wrapper new_result_type
      in 
      (
        Ext_list.fold_right (fun (label,ty,attrs,loc) acc -> 
            Ast_helper.Typ.arrow ~loc  ~attrs label ty acc 
          ) new_arg_types_ty new_result_type
      ) ,

      prim_name,
      (Ffi_bs (arg_type_specs,return_wrapper ,  ffi)), left_attrs
    end

let handle_attributes_as_string 
    pval_loc
    pval_prim 
    (typ : Ast_core_type.t) attrs v = 
  let pval_type, prim_name, ffi, processed_attrs  = 
    handle_attributes pval_loc pval_prim typ attrs v  in
  pval_type, [prim_name; External_ffi_types.to_string ffi], processed_attrs



let pval_prim_of_labels labels = 
  let encoding = 
    let arg_kinds = 
      Ext_list.fold_right 
        (fun {Asttypes.loc ; txt } arg_kinds
          ->
            let arg_label =  External_arg_spec.label (Lam_methname.translate ~loc txt) None in
            {External_arg_spec.arg_type = Nothing ; 
             arg_label  } :: arg_kinds
        )
        labels [] in 
    External_ffi_types.to_string 
      (Ffi_obj_create arg_kinds)   in 
  [""; encoding]

