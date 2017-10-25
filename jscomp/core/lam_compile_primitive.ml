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








module E = Js_exp_make 

(* If it is the return value, since it is a side-effect call,
   we return unit, otherwise just return it
*)
let decorate_side_effect ({st; should_return;_} : Lam_compile_context.t) e : E.t = 
  match st, should_return with 
  | _, ReturnTrue _ 
  | (Assign _ | Declare _ | NeedValue), _  -> E.seq e E.unit
  | EffectCall, ReturnFalse -> e 
(* NeedValue should return a meaningful expression*)

let translate  loc
    ({ meta = { env; _}; _} as cxt : Lam_compile_context.t) 
    (prim : Lam.primitive)
    (args : J.expression list) : J.expression = 
  match prim with
  (* | Pcreate_exception s  *)
  (*   ->  *)
  (*   Js_of_lam_exception.make_exception (E.str s) *)
  | Pcreate_extension s 
    -> 
    Js_of_lam_exception.make (E.str s)
  | Pwrap_exn -> 
    E.runtime_call Js_runtime_modules.exn "internalToOCamlException" args 
  | Lam.Praw_js_code_exp s -> 
    E.raw_js_code Exp s  
  | Lam.Praw_js_code_stmt s -> 
    E.raw_js_code Stmt s 
  | Lam.Pjs_runtime_apply -> 
    begin match args with 
      | [f ;  args] -> 
        E.flat_call f args
      | _ -> assert false 
    end
  | Pjs_apply -> 
    begin match args with 
      | fn :: rest -> 
        E.call ~info:{arity=Full; call_info =  Call_na} fn rest 
      | _ -> assert false
    end

  | Lam.Pnull_to_opt -> 
    begin match args with 
      | [e] -> 
        begin match e.expression_desc with 
          | Var _ -> 
            E.econd (E.is_nil e) Js_of_lam_option.none (Js_of_lam_option.some e)
          | _ ->
            E.runtime_call Js_runtime_modules.js_primitive
              "null_to_opt" args 
              (* GPR #974
                 let id = Ext_ident.create "v" in
                 let tmp = E.var id in
                 E.(seq (assign tmp e ) 
                    (econd (is_nil tmp) Js_of_lam_option.none (Js_of_lam_option.some tmp)) )
              *)
        end
      | _ -> assert false 
    end
  | Lam.Pundefined_to_opt ->
    begin match args with 
      | [e] -> 
        begin match e.expression_desc with 
          | Var _ -> 
            E.econd (E.is_undef e) Js_of_lam_option.none (Js_of_lam_option.some e)
          | _ -> 
            E.runtime_call Js_runtime_modules.js_primitive  
              "undefined_to_opt" args 
              (* # GPR 974
                 let id = Ext_ident.create "v" in
                 let tmp = E.var id in
                 E.(seq (assign tmp e ) 
                     (econd (is_undef tmp) Js_of_lam_option.none (Js_of_lam_option.some tmp)) )
              *)
        end
      | _ -> assert false 
    end    
  | Lam.Pnull_undefined_to_opt -> 
    begin match args with 
      | [e] -> 
        begin match e.expression_desc with 
          | Var _ -> 
            E.econd (E.is_null_undefined e) 
              Js_of_lam_option.none 
              (Js_of_lam_option.some e)
          | _ ->
            E.runtime_call 
              Js_runtime_modules.js_primitive        
              "null_undefined_to_opt" args 
        end
      | _ -> assert false  
    end   
  | Pjs_function_length -> 
    begin match args with 
      | [f] -> E.function_length f
      | _ -> assert false 
    end
  | Lam.Pcaml_obj_length -> 
    begin match args with 
      | [e] -> E.obj_length e 
      | _ -> assert false 
    end
  | Lam.Pcaml_obj_set_length -> 
    begin match args with 
      | [a;b] -> E.set_length a b 
      | _ -> assert false 
    end
  | Lam.Pjs_string_of_small_array -> 
    begin match args with 
      | [e] -> E.string_of_small_int_array e 
      | _ -> assert false 
    end 
  | Lam.Pjs_is_instance_array -> 
    begin match args with 
      | [e] -> E.is_instance_array e 
      | _ -> assert false 
    end 


  | Pis_null -> 
    begin match args with 
      | [e] -> E.is_nil e 
      | _ -> assert false 
    end   
  | Pis_undefined -> 
    begin match args with 
      | [e] -> E.is_undef e 
      | _ -> assert false 
    end
  | Pis_null_undefined -> 
    begin match args with 
      | [ arg] -> 
        E.is_null_undefined arg
      | _ -> assert false 
    end
  
  | Pjs_boolean_to_bool -> 
    begin match args with 
      | [e] -> E.bool_of_boolean e 
      | _ -> assert false 
    end
  | Pjs_typeof -> 
    begin match args with 
      | [e] -> E.typeof e 
      | _ -> assert false 
    end
  | Pjs_unsafe_downgrade _
  | Pdebugger 
  | Pjs_fn_run _ 
  | Pjs_fn_make _

  | Pjs_fn_runmethod _ 
    -> assert false (* already handled by {!Lam_compile} *)
  | Pjs_fn_method _ -> assert false
  | Pglobal_exception id ->
    Js_of_lam_exception.get_builtin_by_name id.name    
  | Pstringadd ->
    begin match args with      
      | [a;b] ->
        E.string_append a b
      | _ -> assert false          
    end          
  | Pinit_mod -> 
    E.runtime_call Js_runtime_modules.module_ "init_mod" args
  | Pupdate_mod ->
    E.runtime_call Js_runtime_modules.module_ "update_mod" args
  | Pmakeblock(tag, tag_info, mutable_flag ) ->  (* RUNTIME *)
    Js_of_lam_block.make_block 
      (Js_op_util.of_lam_mutable_flag mutable_flag) 
      tag_info (E.small_int tag) args 
  | Pfield (i, fld_info) -> 
    begin match args with 
      | [ e ]  -> 
        Js_of_lam_block.field fld_info e (Int32.of_int i)
      (* Invariant depends on runtime *)
      | _ -> assert false
    end

  (** Negate boxed int *)
  | Pnegbint Pint32
    ->
    begin match args with
      | [ e ] -> E.int32_minus (E.zero_int_literal)  e 
      | _ -> assert false
    end
  | Pnegint
    -> 
    begin match args with (* #977 *)
      | [ e ] -> E.int32_minus (E.zero_int_literal)  e 
      | _ -> assert false
    end

  | Pnegbint Pnativeint
    -> 
    begin match args with
      | [ e ] -> E.unchecked_int32_minus (E.zero_int_literal)  e 
      | _ -> assert false
    end
  | Pnegbint Pint64
    -> 
    Js_long.neg args 


  | Pnegfloat 
    -> 
    begin match args with 
      | [ e ] -> E.float_minus (E.zero_float_lit) e 
      | _ -> assert false
    end
  (** Negate boxed int end*)
  (* Int addition and subtraction *)
  | Paddint 
  | Paddbint  Pint32
    ->
    begin match args with
      | [e1;e2] ->
        E.int32_add  e1  e2
      | _ -> assert false
    end
  | Paddbint Pnativeint 
    -> 
    begin match args with
      | [e1;e2] ->
        E.unchecked_int32_add  e1  e2
      | _ -> assert false
    end

  | Paddbint Pint64
    ->  
    Js_long.add args 


  | Paddfloat
    -> 
    begin match args with
      | [e1;e2] ->
        E.float_add  e1  e2
      | _ -> assert false
    end
  | Psubint 
    -> 
    begin match args with 
      | [e1; e2] ->     
        E.int32_minus e1 e2 
      | _ -> assert false
    end
  | Psubbint Pint32
    -> 
    begin match args with
      | [e1;e2] ->
        E.int32_minus   e1  e2
      | _ -> assert false 
    end
  | Psubbint Pnativeint
    -> 
    begin match args with
      | [e1;e2] ->
        E.unchecked_int32_minus   e1  e2
      | _ -> assert false 
    end
  | Psubbint Pint64
    -> 
    Js_long.sub args 
  | Psubfloat
    ->
    begin match args with
      | [e1;e2] ->
        E.float_minus   e1  e2
      | _ -> assert false 
    end
  | Pmulbint Lambda.Pnativeint
    -> 
    begin match args with
      | [e1; e2]  ->
        E.unchecked_int32_mul  e1  e2
      | _ -> assert false 
    end

  | Pmulint 
  | Pmulbint Lambda.Pint32
    ->
    begin match args with
      | [e1; e2]  ->
        E.int32_mul  e1  e2
      | _ -> assert false 
    end
  | Pmulbint Pint64 
    -> 
    Js_long.mul args 
  | Pmulfloat 
    -> 
    begin match args with
      | [e1; e2]  ->
        E.float_mul  e1  e2
      | _ -> assert false 
    end
  | Pdivfloat -> 
    begin match args with  
      | [e1;e2] -> E.float_div  e1  e2
      | _ -> assert false 
    end
  | Pdivbint Pnativeint
    -> 
    begin match args with 
      | [e1;e2] ->
        E.int32_div ~checked:false e1 e2
      | _ -> assert false
    end
  | Pdivint 
  | Pdivbint Pint32
    -> 
    begin match args with 
      | [e1;e2] ->
        E.int32_div ~checked:(!Js_config.check_div_by_zero) e1 e2
      | _ -> assert false
    end

  | Pdivbint Pint64 
    -> Js_long.div args 
  | Pmodint 
  | Pmodbint Pnativeint
  | Pmodbint Pint32
    ->
    begin match args with
      | [e1; e2] ->
        E.int32_mod   ~checked:(!Js_config.check_div_by_zero) e1  e2
      | _ -> assert false 
    end
  | Pmodbint Lambda.Pint64 
    -> Js_long.mod_ args  
  | Plslint 
  | Plslbint Lambda.Pnativeint
  | Plslbint Lambda.Pint32
    ->
    begin match args with
      | [e1;e2] ->
        E.int32_lsl e1  e2
      | _ -> assert false 
    end
  | Plslbint Lambda.Pint64 
    -> Js_long.lsl_ args
  | Plsrbint Lambda.Pnativeint
    -> 
    begin match args with
      | [e1; e2] ->
        E.int32_lsr   e1  e2
      | _ -> assert false
    end
  | Plsrint 
  | Plsrbint Lambda.Pint32
    ->
    begin match args with
      | [e1; {J.expression_desc = Number (Int {i=0l; _}|Uint 0l | Nint 0n); _}]
        -> 
        e1
      | [e1; e2] ->
        E.to_int32 @@ E.int32_lsr   e1  e2
      | _ -> assert false
    end
  | Plsrbint Lambda.Pint64
    -> Js_long.lsr_ args
  | Pasrint 
  | Pasrbint Lambda.Pnativeint
  | Pasrbint Lambda.Pint32
    ->
    begin match args with
      | [e1;e2] ->
        E.int32_asr  e1  e2
      | _ -> assert false
    end
  | Pasrbint Lambda.Pint64 
    -> Js_long.asr_ args      
  | Pandint 
  | Pandbint Lambda.Pnativeint
  | Pandbint Lambda.Pint32
    ->
    begin match args with
      | [e1;e2] ->
        E.int32_band  e1  e2
      | _ -> assert false
    end
  | Pandbint Lambda.Pint64
    -> Js_long.and_ args
  | Porint 
  | Porbint Lambda.Pnativeint
  | Porbint Lambda.Pint32
    ->
    begin match args with
      | [e1;e2] ->
        E.int32_bor  e1  e2
      | _ -> assert false
    end
  | Porbint Lambda.Pint64 
    -> Js_long.or_ args
  | Pxorint 
  | Pxorbint Lambda.Pnativeint
  | Pxorbint Lambda.Pint32 
    -> 
    begin match args with
      | [e1;e2] ->
        E.int32_bxor  e1  e2
      | _ -> assert false
    end
  | Pxorbint Lambda.Pint64 
    ->
    Js_long.xor args    
  | Pjscomp cmp ->
    begin match args with
      | [l;r] -> E.js_comp cmp l r 
      | _ -> assert false 
    end
  | Pbintcomp (Pnativeint ,cmp)
  | Pfloatcomp cmp
  | Pintcomp cmp
  | Pbintcomp (Pint32 ,cmp)
    ->
    begin 
      (* Global Builtin Exception is an int, like 
         [Not_found] or [Invalid_argument] ?
      *)
      match args with 
      | [e1;e2] -> E.int_comp cmp e1 e2
      | _ -> assert false 
    end
  (* List --> stamp = 0 
     Assert_false --> stamp = 26 
  *)
  | Pbintcomp (Pint64 ,cmp)
    -> Js_long.comp cmp args

  | Pcvtbint ((Pint32 | Pnativeint ), Pint64) 
    -> Js_long.of_int32 args
  | Pcvtbint (Pint64, Pint64)
  | Pcvtbint ((Pnativeint|Pint32), (Pnativeint|Pint32))
    ->   
    begin match args with 
      | [e0] -> e0 
      | _ -> assert false
    end
  | Pcvtbint (Pint64, (Pnativeint|Pint32)) 
    ->  
    Js_long.to_int32 args 
  | Pintoffloat -> 
    begin
      match args with 
      | [e] -> E.to_int32 e 
      | _ -> assert false 
    end
  | Pbintofint Pint64
    -> Js_long.of_int32 args 
  | Pbintofint (Pnativeint 
               | Pint32 )
  | Pintofbint Pnativeint
  | Pintofbint Pint32
  | Pfloatofint 
    -> 
    begin match args with 
      | [e] -> e 
      | _ -> assert false 
    end
  | Pintofbint Pint64
    -> Js_long.to_int32 args
  | Pabsfloat -> 
    begin match args with 
      | [e] ->
        E.math "abs" [e]
      (* GCC treat built-ins like Math in a dirfferent way*)
      | _ -> assert false
    end
  | Pnot ->
    begin match args with
      | [e] ->  E.not  e 
      | _ -> assert false
    end
  | Poffsetint n ->
    begin match args with
      | [e] ->  E.int32_add  e (E.small_int  n)
      | _ -> assert false
    end
  | Poffsetref n ->
    begin match args with
      | [e] -> 
        let v = (Js_of_lam_block.field Fld_na e 0l) in
        E.seq (E.assign  v (E.int32_add v (E.small_int  n))) E.unit
      | _ -> assert false
    end

  | Psequand -> (* TODO: rhs is possibly a tail call *)
    begin match args with
      | [e1;e2] ->
        E.and_   e1  e2
      | _ -> assert false
    end
  | Psequor -> (* TODO: rhs is possibly a tail call *)
    begin match args with
      | [e1;e2] ->
        E.or_  e1  e2
      | _ -> assert false
    end
  | Pisout -> 
    begin match args with 
      (* predicate: [x > range  or x < 0 ]
         can be simplified if x is positive , x > range
         if x is negative, fine, its uint is for sure larger than range,
         the output is not readable, we might change it back.

         Note that if range is small like [1], then the negative of 
         it can be more precise (given integer)
         a normal case of the compiler is  that it will do a shift 
         in the first step [ (x - 1) > 1 or ( x - 1 ) < 0 ]
      *)
      | [range; e] -> E.is_out e range
      | _ -> assert false
    end
  | Pbytes_of_string -> 
    begin 
      (* TODO: write a js primitive  - or is it necessary ?
         if we have byte_get/string_get
         still necessary, since you can set it now.
      *)
      match args with 
      |[e] -> Js_of_lam_string.bytes_of_string e
      | _ -> assert false
    end
  | Pbytes_to_string  -> 
    begin 
      match args with 
      |[e] -> Js_of_lam_string.bytes_to_string e 
      | _ -> assert false
    end
  | Pstringlength ->
    begin match args with
      | [e] -> E.string_length e 
      | _ -> assert false 
    end
  | Pbyteslength  -> 
    begin match args with
      | [e] -> E.bytes_length e 
      | _ -> assert false 
    end
  (* This should only be Pbyteset(u|s), which in js, is an int array 
     Bytes is an int array in javascript
  *)
  | Pbytessetu
  | Pbytessets -> 
    begin match args with
      | [e;e0;e1] -> decorate_side_effect cxt 
                       (Js_of_lam_string.set_byte e e0 e1)

      | _ -> assert false
    end
  | Pbytesrefu ->
    begin match args with
      | [e;e1] -> Js_of_lam_string.ref_byte e e1
      | _ -> assert false
    end

  | Pbytesrefs ->
    begin match args with
      | [e ; e1] ->
        if !Clflags.fast then
          Js_of_lam_string.ref_byte e e1
        else E.runtime_call Js_runtime_modules.bytes "get" args            
      | _ -> assert false         
    end
  (* For bytes and string, they both return [int] in ocaml 
      we need tell Pbyteref from Pstringref
      1. Pbyteref -> a[i]
      2. Pstringref -> a.charCodeAt (a[i] is wrong)
  *)
  | Pstringrefu  ->
    begin match args with
      | [e;e1] -> Js_of_lam_string.ref_string e e1 
      | _ -> assert false
    end

  | Pstringrefs ->
    begin match args with
      | [e;e1] ->
        if !Clflags.fast then
          Js_of_lam_string.ref_string e e1             
        else       
          E.runtime_call Js_runtime_modules.string "get" args          
      | _ -> assert false
    end
  (** only when Lapply -> expand = true*)
  | Praise  -> assert false (* handled before here *)

  (* Runtime encoding relevant *)
  | Parraylength Pgenarray
  | Parraylength Paddrarray
  | Parraylength Pintarray
  | Parraylength Pfloatarray  -> 
    begin match args with 
      | [e] -> E.array_length e 
      | _ -> assert false
    end
  | Psetfield (i, _, field_info) -> 
    begin match args with 
      | [e0;e1] ->  (** RUNTIME *)
        decorate_side_effect cxt 
          (Js_of_lam_block.set_field field_info e0 (Int32.of_int i) e1)
      (*TODO: get rid of [E.unit ()]*)
      | _ -> assert false
    end
  | Psetfloatfield (i,field_info)
    -> (** RUNTIME --  RETURN VALUE SHOULD BE UNIT *)
    begin 
      match args with 
      | [e;e0] -> 
        decorate_side_effect cxt 
          (Js_of_lam_float_record.set_double_field field_info e (Int32.of_int i) e0 ) 
      | _ -> assert false
    end


  | Pfloatfield (i, field_info) -> (** RUNTIME *)
    begin 
      match args with 
      | [e] ->
        Js_of_lam_float_record.get_double_feild field_info e
          (Int32.of_int i) 
      | _ -> assert false 
    end
  | Parrayrefu _kind ->  
    begin match args with
      | [e;e1] -> Js_of_lam_array.ref_array e e1 (* Todo: Constant Folding *)
      | _ -> assert false
    end
  | Parrayrefs _kind ->
    Lam_dispatch_primitive.translate "caml_array_get" args
  | Pmakearray kind -> 
    Js_of_lam_array.make_array Mutable kind args 
  | Parraysetu _kind -> 
    begin match args with (* wrong*)
      | [e;e0;e1] -> decorate_side_effect cxt @@ Js_of_lam_array.set_array  e e0 e1
      | _ -> assert false
    end

  | Parraysets _kind -> 
    Lam_dispatch_primitive.translate "caml_array_set" args
  | Pccall prim -> 
    Lam_dispatch_primitive.translate prim.prim_name  args
  (* Lam_compile_external_call.translate loc cxt prim args *)
  (* Test if the argument is a block or an immediate integer *)
  | Pjs_object_create labels
    -> 
    assert false 
  (*Lam_compile_external_obj.assemble_args_obj labels args *)
  | Pjs_call (_, arg_types, ffi) -> 
    Lam_compile_external_call.translate_ffi 
      loc cxt arg_types ffi args 
  (** FIXME, this can be removed later *)
  | Pisint -> 
    begin 
      match args with 
      | [e] ->  E.is_type_number e 

      | _ ->   assert false
    end
  | Pctconst ct -> 
    begin
      match ct with 
      | Big_endian -> 
        if Sys.big_endian then  E.caml_true
        else E.caml_false
      | Word_size -> 
        E.small_int  Sys.word_size
      | Ostype_unix -> 
        if Sys.unix then E.caml_true else E.caml_false
      | Ostype_win32 -> 
        if Sys.win32 then E.caml_true else E.caml_false
      | Ostype_cygwin -> 
        if Sys.cygwin then E.caml_true else E.caml_false
    end
  (* | Psetglobal _  ->  *)
  (*   assert false (\* already handled *\) *)
  (* assert false *)
  | Pduprecord ((Record_regular 
                | Record_float ),0)
  | Pduprecord ((Record_regular 
                | Record_float ),_) -> 
    begin match args with 
      | [e] -> Js_of_lam_record.copy e
      | _ -> assert false       
    end
  | Pbigarrayref (unsafe, dimension, kind, layout)
    -> 
    (* can be refined to 
       [caml_bigarray_float32_c_get_1]
       note that kind can be [generic]
       and layout can be [unknown],
       dimension is always available
    *)
    begin match dimension, kind, layout, unsafe with 
      | 1,  ( Pbigarray_float32 | Pbigarray_float64
            | Pbigarray_sint8 | Pbigarray_uint8
            | Pbigarray_sint16 | Pbigarray_uint16
            | Pbigarray_int32 | Pbigarray_int64
            | Pbigarray_caml_int | Pbigarray_native_int
            | Pbigarray_complex32 | Pbigarray_complex64), Pbigarray_c_layout, _
        -> 
        begin match args with
          | [x;indx] -> Js_of_lam_array.ref_array x indx
          | _ -> assert false
        end
      | _, _, _ ,_ -> 
        E.not_implemented ("caml_ba_get_" ^ string_of_int dimension )
        (* E.runtime_call Js_config.bigarray  *)
        (*   ("caml_ba_get_" ^ string_of_int dimension ) args  *)
    end
  | Pbigarrayset (unsafe, dimension, kind, layout)
    -> 
    begin match dimension, kind, layout, unsafe with 
      | 1,  ( Pbigarray_float32 | Pbigarray_float64
            | Pbigarray_sint8 | Pbigarray_uint8
            | Pbigarray_sint16 | Pbigarray_uint16
            | Pbigarray_int32 | Pbigarray_int64
            | Pbigarray_caml_int | Pbigarray_native_int
            | Pbigarray_complex32 | Pbigarray_complex64), Pbigarray_c_layout, _
        -> 
        begin match args with 
          | [x; index; value] -> 
            Js_of_lam_array.set_array x index value          
          | _ -> assert false
        end

      | _ , _, _,_ 
        -> 
        E.not_implemented
          ("caml_ba_set_" ^ string_of_int dimension )
          (* E.runtime_call Js_config.bigarray  *)
          (*   ("caml_ba_set_" ^ string_of_int dimension ) args  *)
    end

  | Pbigarraydim i
    -> 
    E.not_implemented ("caml_ba_dim_" ^ string_of_int i)
  (* E.runtime_call Js_config.bigarray *)
  (*   ("caml_ba_dim_" ^ string_of_int i) args        *)
  | Pbswap16 
    -> 
    E.runtime_call Js_runtime_modules.int32 "caml_bswap16" args
  | Pbbswap Lambda.Pnativeint
  | Pbbswap Lambda.Pint32
    -> 
    E.runtime_call Js_runtime_modules.int32 "caml_int32_bswap" args
  | Pbbswap Lambda.Pint64
    -> Js_long.swap args 
  | Pstring_load_16 unsafe
    -> E.runtime_call Js_runtime_modules.string "caml_string_get16" args
  | Pstring_load_32 unsafe
    -> E.runtime_call Js_runtime_modules.string "caml_string_get32" args
  | Pstring_load_64 unsafe
    -> Js_long.get64 args

  | Plazyforce
  (* | Plazyforce -> *)
  (*     let parm = Ident.create "prim" in *)
  (*     Lfunction(Curried, [parm], *)
  (*               Matching.inline_lazy_force (Lvar parm) Location.none) *)
  (* It is inlined, this should not appear here *)    
  | Pbittest 

  | Pstring_set_16 _
  | Pstring_set_32 _
  | Pstring_set_64 _
  | Pbigstring_load_16 _
  | Pbigstring_load_32 _
  | Pbigstring_load_64 _
  | Pbigstring_set_16 _
  | Pbigstring_set_32 _
  | Pbigstring_set_64 _
    -> 
    let comment = "Missing primitive" in       
    let s = Lam_print.primitive_to_string prim in
    let warn = Printf.sprintf  "%s: %s\n" comment s in
    Ext_log.warn __LOC__ "%s"  warn;
    (*we dont use [throw] here, since [throw] is an statement  *)        
    E.dump  Error [ E.str warn]


