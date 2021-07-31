(* Copyright (C) 2018 - Hongbo Zhang, Authors of ReScript
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


let caml_id_field_info : Lambda.field_dbg_info = Fld_record {name = Literals.exception_id; mutable_flag = Immutable;}
let lam_caml_id : Lam_primitive.t = Pfield(0, caml_id_field_info)

let prim = Lam.prim 


let lam_extension_id loc (head : Lam.t) =
  prim ~primitive:lam_caml_id ~args:[head] loc    

let lazy_block_info : Lam_tag_info.t = 
  Blk_record 
    [|Literals.lazy_done; 
      Literals.lazy_val|]  

let unbox_extension info (args : Lam.t list) mutable_flag loc =
  prim ~primitive:(Pmakeblock (0,info,mutable_flag)) ~args loc 


(** A conservative approach to avoid packing exceptions
    for lambda expression like {[
      try { ... }catch(id){body}
    ]}
    we approximate that if [id] is destructed or not.
    If it is destructed, we need pack it in case it is JS exception.
    The packing is called Js.Exn.internalTOOCamlException, which is a nop for OCaml exception, 
    but will wrap as (Error e) when it is an JS exception. 

    {[
      try .. with 
      | A (x,y) -> 
      | Js.Error ..
    ]}

    Without such wrapping, the code above would raise

    Note it is not guaranteed that exception raised(or re-raised) is a structured
    ocaml exception but it is guaranteed that if such exception is processed it would
    still be an ocaml exception.
    for example {[
      match x with
      | exception e -> raise e
    ]}
    it will re-raise an exception as it is (we are not packing it anywhere)

    It is hard to judge an exception is destructed or escaped, any potential
    alias(or if it is passed as an argument) would cause it to be leaked
*)
let exception_id_destructed (l : Lam.t) (fv : Ident.t): bool  =
  let rec 
    hit_opt (x : _ option) = 
    match x with 
    | None -> false
    | Some a -> hit a   
  and hit_list_snd : 'a. ('a * _ ) list -> bool = fun x ->    
    Ext_list.exists_snd  x  hit
  and hit_list xs = Ext_list.exists xs hit 
  and hit (l : Lam.t) =
    match l  with
    (* | Lprim {primitive = Pintcomp _ ;
             args = ([x;y ])  } ->
       begin match x,y with
        | Lvar _, Lvar _ -> false
        | Lvar _, _ -> hit y
        | _, Lvar _ -> hit x
        | _, _  -> hit x || hit y
       end *) (* FIXME: this can be uncovered after we do the unboxing *)
    | Lprim {primitive = Praise ; args = [Lvar _]} -> false 
    | Lprim {primitive = _; args; _} ->
      hit_list args
    | Lvar id ->    
      Ident.same id fv
    | Lassign(id, e) ->
      Ident.same id fv || hit e
    | Lstaticcatch(e1, (_,_vars), e2) ->
      hit e1 || hit e2
    | Ltrywith(e1, _exn, e2) ->
      hit e1 || hit e2
    | Lfunction{body;params=_} ->
      hit body;
    | Llet(_str, _id, arg, body) ->
      hit arg || hit body
    | Lletrec(decl, body) ->
      hit body ||
      hit_list_snd decl
    | Lfor(_v, e1, e2, _dir, e3) ->
      hit e1 || hit e2 || hit e3
    | Lconst _ -> false
    | Lapply{ap_func; ap_args; _} ->
      hit ap_func || hit_list ap_args
    | Lglobal_module _  (* global persistent module, play safe *)
      -> false
    | Lswitch(arg, sw) ->
      hit arg ||
      hit_list_snd sw.sw_consts ||
      hit_list_snd sw.sw_blocks ||
      hit_opt sw.sw_failaction 
    | Lstringswitch (arg,cases,default) ->
      hit arg ||
      hit_list_snd cases ||
      hit_opt default 
    | Lstaticraise (_,args) ->
      hit_list args
    | Lifthenelse(e1, e2, e3) ->
      hit e1 || hit e2 || hit e3
    | Lsequence(e1, e2) ->
      hit e1 || hit e2
    | Lwhile(e1, e2) ->
      hit e1 || hit e2
  in hit l


let abs_int x = if x < 0 then - x else x
let no_over_flow x  = abs_int x < 0x1fff_ffff 

let lam_is_var (x : Lam.t) (y : Ident.t) = 
  match x with 
  | Lvar y2 -> Ident.same y2 y 
  | _ -> false

(** Make sure no int range overflow happens
    also we only check [int]
*)
let happens_to_be_diff
    (sw_consts :
       (int * Lambda.lambda) list) : int option =
  match sw_consts with
  | (a, Lconst (Const_pointer (a0,Pt_constructor _)| Const_base (Const_int a0)))::
    (b, Lconst (Const_pointer (b0,Pt_constructor _)| Const_base (Const_int b0)))::
    rest when
      no_over_flow a  &&
      no_over_flow a0 &&
      no_over_flow b &&
      no_over_flow b0 ->
    let diff = a0 - a in
    if b0 - b = diff then
      if Ext_list.for_all rest (fun (x, lam) ->
          match lam with
          | Lconst (Const_pointer(x0, Pt_constructor _) | Const_base(Const_int x0))
            when no_over_flow x0 && no_over_flow x ->
            x0 - x = diff
          | _ -> false
        ) then
        Some diff
      else
        None
    else None
  | _ -> None 




(* type required_modules = Lam_module_ident.Hash_set.t *)


(** drop Lseq (List! ) etc 
    see #3852, we drop all these required global modules
    but added it back based on our own module analysis
*)
let rec drop_global_marker (lam : Lam.t) =
  match lam with
  | Lsequence (Lglobal_module _, rest) ->
    drop_global_marker rest
  | _ -> lam

let seq = Lam.seq 
let unit = Lam.unit 

let convert_record_repr ( x : Types.record_representation) 
  : Lam_primitive.record_representation = 
  match x with 
  | Record_regular 
  | Record_float ->  Record_regular
  | Record_extension -> Record_extension 
  | Record_unboxed _ -> assert false 
  (* see patches in {!Typedecl.get_unboxed_from_attributes}*)
  | Record_inlined {tag; name; num_nonconsts} -> 
    Record_inlined {tag; name; num_nonconsts}


let lam_prim ~primitive:( p : Lambda.primitive) ~args loc : Lam.t =
  match p with
  | Pidentity -> Ext_list.singleton_exn args 
  | Puncurried_apply 
  | Pccall _ -> assert false
  | Prevapply -> assert false
  | Pdirapply -> assert false
  | Ploc _ -> assert false (* already compiled away here*)

  | Pbytes_to_string (* handled very early *)
    -> prim ~primitive:Pbytes_to_string ~args loc
  | Pbytes_of_string -> prim ~primitive:Pbytes_of_string ~args loc
  | Pignore -> (* Pignore means return unit, it is not an nop *)
    seq (Ext_list.singleton_exn args) unit 
  | Pgetglobal _ ->
    assert false
  | Psetglobal _ ->
    (* we discard [Psetglobal] in the beginning*)
    drop_global_marker (Ext_list.singleton_exn args)
  (* prim ~primitive:(Psetglobal id) ~args loc *)
  | Pmakeblock (tag,info, mutable_flag
               , _block_shape 
               )
    -> 
    begin match info with 
      | Blk_some_not_nested 
        -> 
        prim ~primitive:Psome_not_nest ~args loc 
      | Blk_some 
        ->    
        prim ~primitive:Psome ~args loc 
      | Blk_constructor{name ; num_nonconst } ->  
        let info : Lam_tag_info.t = Blk_constructor {name; num_nonconst} in
        prim ~primitive:(Pmakeblock (tag,info,mutable_flag)) ~args loc
      | Blk_tuple  -> 
        let info : Lam_tag_info.t = Blk_tuple in
        prim ~primitive:(Pmakeblock (tag,info,mutable_flag)) ~args loc
      | Blk_extension  -> 
        let info : Lam_tag_info.t = Blk_extension in
        unbox_extension info args mutable_flag loc 
      | Blk_record_ext s ->
        let info : Lam_tag_info.t = Blk_record_ext s in
        unbox_extension info args mutable_flag loc 
      | Blk_extension_slot -> 
        ( 
          match args with 
          | [ Lconst (Const_string name)] -> 
            prim ~primitive:(Pcreate_extension name) ~args:[] loc
          | _ ->
            assert false
        )

      | Blk_class  -> 
        let info : Lam_tag_info.t = Blk_class in
        prim ~primitive:(Pmakeblock (tag,info,mutable_flag)) ~args loc  
      | Blk_array -> 
        let info : Lam_tag_info.t = Blk_array in
        prim ~primitive:(Pmakeblock (tag,info,mutable_flag)) ~args loc    
      | Blk_record s -> 
        let info : Lam_tag_info.t = Blk_record s in
        prim ~primitive:(Pmakeblock (tag,info,mutable_flag)) ~args loc    
      | Blk_record_inlined {name; fields; num_nonconst} ->
        let info : Lam_tag_info.t = Blk_record_inlined {name; fields; num_nonconst} in
        prim ~primitive:(Pmakeblock (tag,info,mutable_flag)) ~args loc
      | Blk_module s -> 
        let info : Lam_tag_info.t = Blk_module s in
        prim ~primitive:(Pmakeblock (tag,info,mutable_flag)) ~args loc
      | Blk_module_export _ -> 
        let info : Lam_tag_info.t = Blk_module_export in
        prim ~primitive:(Pmakeblock (tag,info,mutable_flag)) ~args loc  
      | Blk_poly_var s -> 
        begin match args with 
          | [_; value] -> 
            let info : Lam_tag_info.t = Blk_poly_var  in
            let tag_val : Lam_constant.t = 
              if Ext_string.is_valid_hash_number s then Const_int {i = Ext_string.hash_number_as_i32_exn s; comment = None}
              else Const_string s in 
            prim ~primitive:(Pmakeblock (tag,info,mutable_flag)) ~args:[Lam.const tag_val; value] loc      
          | _ -> assert false  
        end
      | Blk_lazy_general  
        ->
        begin match args with 
          | [Lvar _ | Lconst _ | Lfunction _ as result ] -> 
            let args = 
              [ Lam.const Const_js_true ; 
                result
              ] in 
            prim ~primitive:(Pmakeblock (tag,lazy_block_info,Mutable)) ~args loc  
          | [computation] -> 
            let args = 
              [ Lam.const Const_js_false ; 
                (* FIXME: arity 0 does not get proper supported*)
                Lam.function_ 
                  ~arity:0 
                  ~params:[] ~body:computation 
                  ~attr:Lam.default_fn_attr                
              ] in 
            prim ~primitive:(Pmakeblock (tag,lazy_block_info,Mutable)) ~args loc  

          | _ -> assert false
        end
      | Blk_na s -> 
        let info : Lam_tag_info.t = Blk_na s in
        prim ~primitive:(Pmakeblock (tag,info,mutable_flag)) ~args loc
    end  
  | Pfield (id,info)
    -> prim ~primitive:(Pfield (id,info)) ~args loc

  | Psetfield (id, _,
               _initialization_or_assignment,
               info)
    -> prim ~primitive:(Psetfield (id,info)) ~args loc
  | Psetfloatfield _
  | Pfloatfield _
    -> assert false  
  | Pduprecord (repr,_)
    -> prim ~primitive:(Pduprecord (convert_record_repr repr)) ~args loc
  | Plazyforce -> prim ~primitive:Plazyforce ~args loc


  | Praise _ ->
    prim ~primitive:Praise ~args loc
  | Psequand -> prim ~primitive:Psequand ~args loc
  | Psequor -> prim ~primitive:Psequor ~args loc
  | Pnot -> prim ~primitive:Pnot ~args loc
  | Pnegint -> prim ~primitive:Pnegint ~args  loc
  | Paddint -> prim ~primitive:Paddint ~args loc
  | Psubint -> prim ~primitive:Psubint ~args loc
  | Pmulint -> prim ~primitive:Pmulint ~args loc
  | Pdivint 
      _is_safe (*FIXME*)
    -> prim ~primitive:Pdivint ~args loc
  | Pmodint 
      _is_safe (*FIXME*)
    -> prim ~primitive:Pmodint ~args loc
  | Pandint -> prim ~primitive:Pandint ~args loc
  | Porint -> prim ~primitive:Porint ~args loc
  | Pxorint -> prim ~primitive:Pxorint ~args loc
  | Plslint -> prim ~primitive:Plslint ~args loc
  | Plsrint -> prim ~primitive:Plsrint ~args loc
  | Pasrint -> prim ~primitive:Pasrint ~args loc
  | Pstringlength -> prim ~primitive:Pstringlength ~args loc
  | Pstringrefu -> prim ~primitive:Pstringrefu ~args loc
  | Pabsfloat -> assert false
  | Pstringrefs -> prim ~primitive:Pstringrefs ~args loc
  | Pbyteslength -> prim ~primitive:Pbyteslength ~args loc
  | Pbytesrefu -> prim ~primitive:Pbytesrefu ~args loc
  | Pbytessetu -> prim ~primitive:Pbytessetu ~args  loc
  | Pbytesrefs -> prim ~primitive:Pbytesrefs ~args loc
  | Pbytessets -> prim ~primitive:Pbytessets ~args loc
  | Pisint -> prim ~primitive:Pisint ~args loc
  | Pisout -> 
    begin match args with 
      | [ range; Lprim {primitive = Poffsetint i; args = [x]}] ->

        prim ~primitive:(Pisout i) ~args:[range;x] loc 
      | _ ->     
        prim ~primitive:(Pisout 0) ~args loc
    end
  | Pintoffloat -> prim ~primitive:Pintoffloat ~args loc
  | Pfloatofint -> prim ~primitive:Pfloatofint ~args loc
  | Pnegfloat -> prim ~primitive:Pnegfloat ~args loc

  | Paddfloat -> prim ~primitive:Paddfloat ~args loc
  | Psubfloat -> prim ~primitive:Psubfloat ~args loc
  | Pmulfloat -> prim ~primitive:Pmulfloat ~args loc
  | Pdivfloat -> prim ~primitive:Pdivfloat ~args loc
  | Pintcomp x -> prim ~primitive:(Pintcomp x)  ~args loc
  | Poffsetint x -> prim ~primitive:(Poffsetint x) ~args loc
  | Poffsetref x -> prim ~primitive:(Poffsetref x) ~args  loc
  | Pfloatcomp x -> prim ~primitive:(Pfloatcomp x) ~args loc
  | Pmakearray 
      (_, _mutable_flag) (*FIXME*)
    -> prim ~primitive:Pmakearray ~args  loc
  | Parraylength _ -> prim ~primitive:Parraylength ~args loc
  | Parrayrefu _ -> prim ~primitive:(Parrayrefu ) ~args loc
  | Parraysetu _ -> prim ~primitive:(Parraysetu ) ~args loc
  | Parrayrefs _ -> prim ~primitive:(Parrayrefs ) ~args loc
  | Parraysets _ -> prim ~primitive:(Parraysets ) ~args loc
  | Pbintofint x -> 
    begin match x with 
      | Pint32 | Pnativeint -> Ext_list.singleton_exn args
      | Pint64 -> prim ~primitive:(Pint64ofint ) ~args loc
    end
  | Pintofbint x -> 
    begin match x with 
      | Pint32 | Pnativeint -> Ext_list.singleton_exn args 
      | Pint64 -> prim ~primitive:Pintofint64  ~args loc
    end
  | Pnegbint x -> 
    begin match x with 
      | Pnativeint 
      | Pint32 -> prim ~primitive:Pnegint ~args loc 
      | Pint64 -> prim ~primitive:Pnegint64 ~args loc 
    end  
  | Paddbint x -> 
    begin match x with 
      | Pnativeint 
      | Pint32  -> prim ~primitive:Paddint ~args loc 
      | Pint64 -> prim ~primitive:Paddint64 ~args loc
    end 
  | Psubbint x -> 
    begin match x with 
      | Pnativeint 
      | Pint32 -> prim ~primitive:(Psubint) ~args loc
      | Pint64 -> prim ~primitive:(Psubint64) ~args loc
    end
  | Pmulbint x -> 
    begin match x with 
      | Pnativeint
      | Pint32 -> prim ~primitive:Pmulint ~args loc
      | Pint64 -> prim ~primitive:Pmulint64 ~args loc
    end
  | Pdivbint 
      {size = x; is_safe = _} (*FIXME*)
    ->
    begin match x with 
      | Pnativeint 
      | Pint32 ->  prim ~primitive:(Pdivint) ~args loc
      | Pint64->  prim ~primitive:(Pdivint64) ~args loc
    end
  | Pmodbint 
      {size = x; is_safe = _} (*FIXME*)
    -> 
    begin match x with 
      | Pnativeint 
      | Pint32 -> prim ~primitive:(Pmodint ) ~args loc
      | Pint64 -> prim ~primitive:(Pmodint64 ) ~args loc
    end
  | Pandbint x -> 
    begin match x with 
      | Pnativeint
      | Pint32 -> 
        prim ~primitive:(Pandint ) ~args loc
      | Pint64 -> prim ~primitive:(Pandint64 ) ~args loc  
    end
  | Porbint x -> 
    begin match x with 
      | Pnativeint 
      | Pint32 -> 
        prim ~primitive:(Porint ) ~args loc
      | Pint64 -> 
        prim ~primitive:(Porint64 ) ~args loc
    end
  | Pxorbint x -> 
    begin match x with 
      | Pnativeint
      | Pint32 ->
        prim ~primitive:(Pxorint ) ~args loc
      | Pint64 -> 
        prim ~primitive:(Pxorint64 ) ~args loc
    end
  | Plslbint x -> 
    begin match x with 
      | Pnativeint 
      | Pint32 -> prim ~primitive:(Plslint) ~args loc
      | Pint64 -> prim ~primitive:(Plslint64) ~args loc
    end
  | Plsrbint x -> 
    begin match x with 
      | Pnativeint 
      | Pint32 -> prim ~primitive:(Plsrint) ~args loc
      | Pint64 -> prim ~primitive:(Plsrint64) ~args loc
    end
  | Pasrbint x -> 
    begin match x with 
      | Pnativeint 
      | Pint32 -> prim ~primitive:(Pasrint) ~args loc
      | Pint64 -> prim ~primitive:(Pasrint64) ~args loc
    end
  | Pctconst x ->
    begin match x with
      | Word_size 
      | Int_size -> Lam.const(Const_int {i = 32l; comment = None})  
      | Max_wosize -> Lam.const (Const_int {i = 2147483647l; comment = Some "Max_wosize"})
      | Big_endian
        -> prim ~primitive:(Pctconst Big_endian) ~args loc
      | Ostype_unix
        -> prim ~primitive:(Pctconst Ostype_unix) ~args loc
      | Ostype_win32
        -> prim ~primitive:(Pctconst Ostype_win32) ~args loc
      | Ostype_cygwin
        -> Lam.false_
      | Backend_type
        -> prim ~primitive:(Pctconst Backend_type) ~args loc
    end


  | Pcvtbint (a,b) -> 
    begin match a, b with 
      | (Pnativeint | Pint32), (Pnativeint | Pint32) 
      | Pint64, Pint64 ->   Ext_list.singleton_exn args 
      | Pint64, (Pnativeint | Pint32) 
        -> 
        prim ~primitive:(Pintofint64) ~args loc
      | (Pnativeint | Pint32) , Pint64 
        ->   
        prim ~primitive:(Pint64ofint) ~args loc
    end
  | Pbintcomp (a,b) -> 
    begin match a with 
      | Pnativeint 
      | Pint32 -> prim ~primitive:(Pintcomp b) ~args loc
      | Pint64 -> prim ~primitive:(Pint64comp b) ~args loc
    end
  | Popaque -> Ext_list.singleton_exn args      

(* Does not exist since we compile array in js backend unlike native backend *)

let convert_inline_attr (inline : Lambda.inline_attribute) : Lam.inline_attribute = 
  match inline with    
  | Always_inline -> Always_inline
  | Never_inline -> Never_inline
  | Unroll _
  | Default_inline -> Default_inline 

let convert_fn_attribute (attr : Lambda.function_attribute) : Lam.function_attribute = 
  let inline : Lam.inline_attribute = 
    convert_inline_attr attr.inline in
  let is_a_functor =
    if attr.is_a_functor then Lam.Functor_yes else Functor_no in     
  Lam.{inline; is_a_functor}



let may_depend = Lam_module_ident.Hash_set.add

let rec rename_optional_parameters map params  (body : Lambda.lambda) = 
  match body with 
  | Llet(k,value_kind,id, (Lifthenelse(
      Lprim(p,[Lvar ({name = "*opt*"} as opt)],p_loc), 
      Lprim(p1,[Lvar ({name = "*opt*"} as opt2)],x_loc), f)),rest)
    when Ident.same opt opt2 && List.mem opt params 
    ->
    let map, rest = rename_optional_parameters map params rest in 
    let new_id = Ident.create (id.name ^ "Opt") in 
    Map_ident.add map opt new_id,
    Lambda.Llet(k,value_kind,id, 
                (Lifthenelse(
                    Lprim(p,[Lvar new_id],p_loc), 
                    Lprim(p1,[Lvar new_id],x_loc), f)),rest)
  | _ -> 
    map, body


let convert (exports : Set_ident.t) (lam : Lambda.lambda) : Lam.t * Lam_module_ident.Hash_set.t  =
  let alias_tbl = Hash_ident.create 64 in
  let exit_map = Hash_int.create 0 in
  let may_depends = Lam_module_ident.Hash_set.create 0 in

  let rec
    convert_ccall (a_prim : Primitive.description)  (args : Lambda.lambda list) loc : Lam.t =
    let prim_name = a_prim.prim_name in
    let prim_name_len  = String.length prim_name in
    match External_ffi_types.from_string a_prim.prim_native_name with
    | Ffi_normal ->
      if prim_name_len > 0 && String.unsafe_get prim_name 0 = '#' then
        convert_js_primitive a_prim args loc
      else
        let args = Ext_list.map args convert_aux in
        prim ~primitive:(Pccall {prim_name }) ~args loc
    | Ffi_obj_create labels ->
      let args = Ext_list.map args  convert_aux in
      prim ~primitive:(Pjs_object_create labels) ~args loc
    | Ffi_bs(arg_types, result_type, ffi) ->
      let arg_types = 
        match arg_types with 
        | Params ls -> ls 
        | Param_number i -> Ext_list.init i (fun _ -> External_arg_spec.dummy )   in 
      let args = Ext_list.map args convert_aux in
      Lam.handle_bs_non_obj_ffi arg_types result_type ffi args loc prim_name
    | Ffi_inline_const i -> Lam.const i

  and convert_js_primitive (p: Primitive.description) (args : Lambda.lambda list) loc : Lam.t =
    let s = p.prim_name in
    match () with
    | _ when s = "#is_not_none" -> 
      prim ~primitive:Pis_not_none ~args:(Ext_list.map args convert_aux ) loc 
    | _ when s = "#val_from_unnest_option" 
      -> 
      let v = convert_aux (Ext_list.singleton_exn args) in 
      prim ~primitive:Pval_from_option_not_nest
        ~args:[v] loc 
    | _ when s = "#val_from_option" 
      -> 
      prim ~primitive:Pval_from_option
        ~args:(Ext_list.map args convert_aux ) loc
    | _ when s = "#is_poly_var_block"
      ->     
      prim ~primitive:Pis_poly_var_block
        ~args:(Ext_list.map args convert_aux ) loc
    | _ when s = "#raw_expr" ->
      (match args with
       | [Lconst( Const_base (Const_string(code,_)))] ->
         (* js parsing here *)
         let kind = 
           Classify_function.classify code in 
         prim ~primitive:(Praw_js_code {code; code_info = Exp kind})
           ~args:[] loc
       | _ -> assert false)

    | _ when s = "#raw_stmt" ->
      begin match args with
        | [Lconst( Const_base (Const_string(code,_)))] ->
          let kind = Classify_function.classify_stmt code in 
          prim ~primitive:(Praw_js_code {code; code_info = Stmt kind})
            ~args:[] loc
        | _ -> assert false
      end
    | _ when s =  "#debugger"  ->
      (* ATT: Currently, the arity is one due to PPX *)
      prim ~primitive:Pdebugger ~args:[] loc
    | _ when s = "#null" ->
      Lam.const (Const_js_null)
    | _ when s = "#os_type" ->   
      prim ~primitive:(Pctconst Ostype) ~args:[unit] loc 
    | _ when s = "#undefined" ->
      Lam.const (Const_js_undefined)
    | _ when s = "#init_mod" ->
      let args = Ext_list.map args  convert_aux in
      begin match args with
        | [_loc; Lconst(Const_block(0,_,[Const_block(0,_,[])]))]
          ->
          Lam.unit
        | _ -> prim ~primitive:Pinit_mod ~args loc
      end
    | _ when s = "#update_mod" ->
      let args = Ext_list.map args convert_aux in
      begin match args with
        | [Lconst(Const_block(0,_,[Const_block(0,_,[])]));_;_]
          -> Lam.unit
        | _ -> prim ~primitive:Pupdate_mod ~args loc
      end
    | _ when s = "#extension_slot_eq" -> 
      begin match Ext_list.map args convert_aux with 
        | [lhs; rhs] -> 
          prim 
            ~primitive:(Pccall {prim_name = "caml_string_equal"})
            ~args:[lam_extension_id loc lhs ;
                   rhs;
                  ] loc
        | _ -> assert false end  
    | _ ->
      let primitive : Lam_primitive.t =
        match s with
        | "#apply" -> Pjs_runtime_apply
        | "#apply1"
        | "#apply2"
        | "#apply3"
        | "#apply4"
        | "#apply5"
        | "#apply6"
        | "#apply7"
        | "#apply8" -> Pjs_apply
        | "#makemutablelist" ->
          Pmakeblock(0, Blk_constructor{name = "::"; num_nonconst = 1},Mutable)
        | "#undefined_to_opt" -> Pundefined_to_opt
        | "#nullable_to_opt" -> Pnull_undefined_to_opt
        | "#null_to_opt" -> Pnull_to_opt
        | "#is_nullable" -> Pis_null_undefined
        | "#string_append" -> Pstringadd
        | "#obj_length" -> Pcaml_obj_length
        | "#function_length" -> Pjs_function_length

        | "#unsafe_lt" -> Pjscomp Clt
        | "#unsafe_gt" -> Pjscomp Cgt
        | "#unsafe_le" -> Pjscomp Cle
        | "#unsafe_ge" -> Pjscomp Cge
        | "#unsafe_eq" -> Pjscomp Ceq
        | "#unsafe_neq" -> Pjscomp Cneq

        | "#typeof" -> Pjs_typeof
        | "#run"  -> 
          Pvoid_run

        | "#fn_mk" -> Pjs_fn_make (Ext_pervasives.nat_of_string_exn p.prim_native_name)
        | "#fn_method" -> Pjs_fn_method 
        | "#unsafe_downgrade" -> Pjs_unsafe_downgrade {name = Ext_string.empty;  setter = false}
        | _ -> Location.raise_errorf ~loc
                 "@{<error>Error:@} internal error, using unrecognized primitive %s" s
      in
      let args = Ext_list.map args convert_aux in
      prim ~primitive ~args loc
  and convert_aux (lam : Lambda.lambda) : Lam.t =
    match lam with
    | Lvar x ->
      Lam.var (Hash_ident.find_default alias_tbl x x)
    | Lconst x ->
      Lam.const (Lam_constant_convert.convert_constant x )
    | Lapply 
        {ap_func = fn; ap_args = args; ap_loc = loc; ap_inlined}
      ->
      (* we need do this eargly in case [aux fn] add some wrapper *)
      Lam.apply (convert_aux fn) (Ext_list.map args convert_aux ) {ap_loc = loc; ap_inlined = (convert_inline_attr ap_inlined); ap_status =  App_na}
    | Lfunction 
        { params; body ; attr }
      ->  
      let new_map,body = rename_optional_parameters Map_ident.empty params body in 
      let attr = convert_fn_attribute attr in 
      if Map_ident.is_empty new_map then
        Lam.function_ ~attr
          ~arity:(List.length params)  ~params
          ~body:(convert_aux body)
      else 
        let params = Ext_list.map params (fun x -> Map_ident.find_default new_map x x) in 
        Lam.function_ ~attr
          ~arity:(List.length params)  ~params
          ~body:(convert_aux body)

    | Llet 
        (kind,_value_kind, id,e,body) (*FIXME*)
      -> convert_let kind id e body

    | Lletrec (bindings,body)
      ->
      let bindings = Ext_list.map_snd  bindings convert_aux in
      let body = convert_aux body in
      let lam = Lam.letrec bindings body in
      Lam_scc.scc bindings lam body
    (* inlining will affect how mututal recursive behave *)
    | Lprim(Prevapply, [x ; f ],  outer_loc)
    | Lprim(Pdirapply, [f ; x],  outer_loc) ->
      convert_pipe f x outer_loc
    | Lprim (Prevapply, _, _ ) -> assert false
    | Lprim(Pdirapply, _, _) -> assert false
    | Lprim(Pccall a, args, loc)  ->
      convert_ccall a  args loc
    | Lprim (Pgetglobal id, args, _) ->
      let args = Ext_list.map args convert_aux in
      if Ident.is_predef_exn id then
        Lam.const (Const_string id.name)
      else
        begin
          may_depend may_depends (Lam_module_ident.of_ml id);
          assert (args = []);
          Lam.global_module id
        end
    | Lprim (Puncurried_apply,
      [Lapply {ap_func = Lprim (Popaque,[ap_func],_); ap_args}],loc)   
      ->
        let ap_func = convert_aux ap_func in 
        let ap_args = Ext_list.map ap_args convert_aux  in 
        prim ~primitive:Pfull_apply ~args:(ap_func::ap_args) loc
      (* There may be some optimization opportunities here
         for cases like `(fun [@bs] a b -> a + b ) 1 2 [@bs]` *)          
    | Lprim (Puncurried_apply, _,_ ) ->
         assert false
    | Lprim (primitive,args, loc)
      ->
      let args = Ext_list.map args convert_aux in
      lam_prim ~primitive ~args loc
    | Lswitch 
        (e,s, _loc)
      -> convert_switch e s
    | Lstringswitch (e, cases, default, _ ) ->
      Lam.stringswitch 
        (convert_aux e) 
        (Ext_list.map_snd cases convert_aux)
        (Ext_option.map default convert_aux)
    | Lstaticraise (id,[]) ->
      Lam.staticraise (Hash_int.find_default exit_map id id) []
    | Lstaticraise (id, args) ->
      Lam.staticraise id (Ext_list.map args convert_aux )
    | Lstaticcatch (b, (i,[]), Lstaticraise (j,[]) )
      -> (* peep-hole [i] aliased to [j] *)      
      Hash_int.add exit_map i (Hash_int.find_default exit_map j j);
      convert_aux b
    | Lstaticcatch (b, (i, ids), handler) ->
      Lam.staticcatch (convert_aux b) (i,ids) (convert_aux handler)
    | Ltrywith (b, id, handler) ->
      let body = convert_aux b in
      let handler = convert_aux handler in
      if exception_id_destructed handler id then
        let newId = Ident.create ("raw_" ^ id.name) in
        Lam.try_ body newId
          (Lam.let_ StrictOpt id
             (prim ~primitive:Pwrap_exn ~args:[Lam.var newId] Location.none)
             handler
          )
      else
        Lam.try_  body id handler
    | Lifthenelse (b,then_,else_) ->
      Lam.if_ (convert_aux b) (convert_aux then_) (convert_aux else_)
    | Lsequence (a,b)
      -> Lam.seq (convert_aux a) (convert_aux b)
    | Lwhile (b,body) ->
      Lam.while_ (convert_aux b) (convert_aux body)
    | Lfor (id, from_, to_, dir, loop) ->
      Lam.for_ id (convert_aux from_) (convert_aux to_) dir (convert_aux loop)
    | Lassign (id, body) ->
      Lam.assign id (convert_aux body)
    | Lsend (name,  obj, loc) ->
      let obj =  convert_aux obj in 
      let args = [obj] in 
      let setter = Ext_string.ends_with name Literals.setter_suffix in 
      let property = 
        if setter then   
          Lam_methname.translate 
            (String.sub name 0
               (String.length name - Literals.setter_suffix_len))
        else Lam_methname.translate  name in 
      prim ~primitive:(Pjs_unsafe_downgrade {name = property; setter})
          ~args loc  




  and convert_let (kind : Lam_compat.let_kind) id (e : Lambda.lambda) body : Lam.t = 
    match kind, e with
    | Alias , Lvar u  ->
      let new_u = Hash_ident.find_default alias_tbl u u in
      Hash_ident.add alias_tbl id new_u ;
      if Set_ident.mem exports id then
        Lam.let_ kind id (Lam.var new_u) (convert_aux body)
      else convert_aux body
    | _, _ -> 
      let new_e = convert_aux e in 
      let new_body = convert_aux body in 
          (*
            reverse engineering cases as {[           
           (let (switcher/1013 =a (-1+ match/1012))
               (if (isout 2 switcher/1013) (exit 1)
                   (switch* switcher/1013
                      case int 0: 'a'
                        case int 1: 'b'
                        case int 2: 'c')))            
         ]}
         To elemininate the id [switcher], we need ensure it appears only 
         in two places.

         To advance this case, when [sw_failaction] is None
      *)
      match kind, new_e, new_body with 
      | Alias, Lprim {primitive = Poffsetint offset; args =  [Lvar _ as matcher ]},
        Lswitch (Lvar switcher3 ,
                 ({
                   sw_consts_full = false ; 
                   sw_consts ;
                   sw_blocks = []; sw_blocks_full = true;
                   sw_failaction = Some ifso
                 } as px)
                ) 
        when Ident.same switcher3 id    &&
             not (Lam_hit.hit_variable id ifso ) && 
             not (Ext_list.exists_snd sw_consts (Lam_hit.hit_variable id))
        -> 
        Lam.switch matcher 
          {px with 
           sw_consts = 
             Ext_list.map sw_consts 
               (fun (i,act) -> i - offset, act)
          }
      | _ -> 
        Lam.let_ kind id new_e new_body
  and convert_pipe (f : Lambda.lambda) (x : Lambda.lambda) outer_loc =        
    let x  = convert_aux x in
    let f =  convert_aux f in
    match  f with
    | Lfunction {params = [param]; body = Lprim{primitive; args = [Lvar inner_arg];  }}
      when Ident.same param inner_arg -> 
      Lam.prim ~primitive ~args:[x] outer_loc
    | Lapply {ap_func = Lfunction{params; body = Lprim{primitive; args = inner_args}}; ap_args=args}
      when Ext_list.for_all2_no_exn inner_args params lam_is_var &&
           Ext_list.length_larger_than_n inner_args args 1 
      ->
      Lam.prim ~primitive ~args:(Ext_list.append_one args x) outer_loc
    | Lapply{ap_func;ap_args; ap_info} ->
      Lam.apply ap_func (Ext_list.append_one ap_args x) {ap_loc = outer_loc; ap_inlined = ap_info.ap_inlined; ap_status =  App_na }
    | _ ->
      Lam.apply f [x] {ap_loc = outer_loc; ap_inlined = Default_inline; ap_status = App_na}
  and convert_switch (e : Lambda.lambda) (s : Lambda.lambda_switch) = 
    let  e = convert_aux e in
    match s with
    | {
      sw_failaction = None ;
      sw_blocks = [];
      sw_numblocks = 0;
      sw_consts ;
      sw_numconsts ;
    } ->
      begin match happens_to_be_diff sw_consts with
        | Some 0 -> e
        | Some i ->
          prim
            ~primitive:Paddint
            ~args:[e; Lam.const(Const_int {i = Int32.of_int i; comment = None})]
            Location.none
        | None ->
          Lam.switch e
            {sw_failaction = None;
             sw_blocks = [];
             sw_blocks_full = true;
             sw_consts =
               Ext_list.map_snd  sw_consts convert_aux;
             sw_consts_full = 
               Ext_list.length_ge sw_consts sw_numconsts;
             sw_names = s.sw_names;
            }
      end
    | _ -> 
      Lam.switch  e   
        { sw_consts_full =  Ext_list.length_ge s.sw_consts s.sw_numconsts ;
          sw_consts = Ext_list.map_snd  s.sw_consts convert_aux;
          sw_blocks_full = Ext_list.length_ge s.sw_blocks s.sw_numblocks;
          sw_blocks = Ext_list.map_snd s.sw_blocks convert_aux;
          sw_failaction =Ext_option.map s.sw_failaction convert_aux;
          sw_names = s.sw_names } in
  convert_aux lam , may_depends


(** FIXME: more precise analysis of [id], if it is not 
    used, we can remove it
        only two places emit [Lifused],
    {[
      lsequence (Lifused(id, set_inst_var obj id expr)) rem
        Lifused (env2, Lprim(Parrayset Paddrarray, [Lvar self; Lvar env2; Lvar env1']))
    ]}

        Note the variable, [id], or [env2] is already defined, it can be removed if it is not
        used. This optimization seems useful, but doesnt really matter since it only hit translclass

        more details, see [translclass] and [if_used_test]
        seems to be an optimization trick for [translclass]

        | Lifused(v, l) ->
          if count_var v > 0 then simplif l else lambda_unit
*)  


      (*
        | Lfunction(kind,params,Lprim(prim,inner_args,inner_loc))
          when List.for_all2_no_exn (fun x y ->
          match y with
          | Lambda.Lvar y when Ident.same x y -> true
          | _ -> false
           ) params inner_args
          ->
          let rec aux outer_args params =
            match outer_args, params with
            | x::xs , _::ys ->
              x :: aux xs ys
            | [], [] -> []
            | x::xs, [] ->
            | [], y::ys
          if Ext_list.same_length inner_args args then
            aux (Lprim(prim,args,inner_loc))
          else

   {[
     (fun x y -> f x y) (computation;e) -->
     (fun y -> f (computation;e) y)
   ]}
   is wrong

   or
   {[
     (fun x y -> f x y ) ([|1;2;3|]) -->
     (fun y -> f [|1;2;3|] y)
   ]}
   is also wrong.

   It seems, we need handle [@variadic] earlier

   or
   {[
     (fun x y -> f x y) ([|1;2;3|]) -->
     let x0, x1, x2 =1,2,3 in
     (fun y -> f [|x0;x1;x2|] y)
   ]}
   But this still need us to know [@variadic] in advance


   we should not remove it immediately, since we have to be careful
   where it is used, it can be [exported], [Lvar] or [Lassign] etc
   The other common mistake is that
   {[
     let x = y (* elimiated x/y*)
     let u = x  (* eliminated u/x *)
   ]}

   however, [x] is already eliminated
   To improve the algorithm
   {[
     let x = y (* x/y *)
     let u = x (* u/y *)
   ]}
   This looks more correct, but lets be conservative here

   global module inclusion {[ include List ]}
   will cause code like {[ let include =a Lglobal_module (list)]}

   when [u] is global, it can not be bound again,
   it should always be the leaf
*)      