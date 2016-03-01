(* BuckleScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Author: Hongbo Zhang  *)



module E = Js_exp_make 

(* If it is the return value, since it is a side-effect call,
   we return unit, otherwise just return it
 *)
let decorate_side_effect ({st; should_return;_} : Lam_compile_defs.cxt) e : E.t = 
  match st, should_return with 
  | _, True _ 
  | (Assign _ | Declare _ | NeedValue), _  -> E.seq e E.unit
  | EffectCall, False -> e 
  (* NeedValue should return a meaningful expression*)

let translate 
    ({ meta = { env; _}; _} as cxt : Lam_compile_defs.cxt) 
    (prim : Lambda.primitive)
    (args : J.expression list) : J.expression = 
  match prim with
  | Pmakeblock(tag, tag_info, mutable_flag ) ->  (* RUNTIME *)
    Js_of_lam_block.make_block 
      (Js_op_util.of_lam_mutable_flag mutable_flag) 
      tag_info (E.small_int tag) args 
  | Pfield i -> 
    begin match args with 
      | [ e ]  -> Js_of_lam_block.field e (Int32.of_int i) (* Invariant depends on runtime *)
      | _ -> assert false
    end

(** Negate boxed int *)
  | Pnegbint Pint32
    ->
    begin match args with
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

  | Pnegint
    -> 
    begin match args with
    | [ e ] -> E.unchecked_int32_minus (E.zero_int_literal)  e 
    | _ -> assert false
    end

  | Pnegfloat 
    -> 
    begin match args with 
    | [ e ] -> E.float_minus (E.zero_float_lit) e 
    | _ -> assert false
    end
(** Negate boxed int end*)
(* Int addition and subtraction *)
  | Paddint 
    -> 
      begin match args with 
      | [e1; e2] 
        -> E.unchecked_int32_add e1 e2
      | _ -> assert false 
      end
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
      E.unchecked_int32_minus e1 e2 
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
  | Pmulint 
  | Pmulbint Lambda.Pnativeint
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
  | Pdivint 
  | Pdivbint Lambda.Pnativeint
  | Pdivbint Lambda.Pint32
    -> 
    begin match args with 
      | [e1;e2] ->
        E.int32_div e1 e2
      | _ -> assert false
    end

  | Pdivbint Lambda.Pint64 
    -> Js_long.div args 
  | Pmodint 
  | Pmodbint Lambda.Pnativeint
  | Pmodbint Lambda.Pint32
  | Pmodbint Lambda.Pint64 
    ->
    begin match args with
      | [e1; e2] ->
        E.int32_mod   e1  e2
      | _ -> assert false 
    end
  | Plslint 
  | Plslbint Lambda.Pnativeint
  | Plslbint Lambda.Pint32
  | Plslbint Lambda.Pint64 
    ->
    begin match args with
      | [e1;e2] ->
        E.int32_lsl e1  e2
      | _ -> assert false 
    end
  | Plsrint 
  | Plsrbint Lambda.Pnativeint
  | Plsrbint Lambda.Pint32
  | Plsrbint Lambda.Pint64 ->
    begin match args with
      | [e1; e2] ->
        E.int32_lsr   e1  e2
      | _ -> assert false
    end
  | Pasrint 
  | Pasrbint Lambda.Pnativeint
  | Pasrbint Lambda.Pint32
  | Pasrbint Lambda.Pint64 
    ->
    begin match args with
      | [e1;e2] ->
        E.int32_asr  e1  e2
      | _ -> assert false
    end

  | Pandint 
  | Pandbint Lambda.Pnativeint
  | Pandbint Lambda.Pint32
  | Pandbint Lambda.Pint64
    ->
    begin match args with
      | [e1;e2] ->
        E.int32_band  e1  e2
      | _ -> assert false
    end
  | Porint 
  | Porbint Lambda.Pnativeint
  | Porbint Lambda.Pint32
  | Porbint Lambda.Pint64 
    ->
    begin match args with
      | [e1;e2] ->
        E.int32_bor  e1  e2
      | _ -> assert false
    end
  | Pxorint 
  | Pxorbint Lambda.Pnativeint
  | Pxorbint Lambda.Pint32
  | Pxorbint Lambda.Pint64 ->
    begin match args with
      | [e1;e2] ->
        E.int32_bxor  e1  e2
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
      | [e] -> e 
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
      | [e] ->  E.unchecked_int32_add  e (E.small_int  n)
      | _ -> assert false
    end
  | Poffsetref n ->
    begin match args with
      | [e] -> 
        let v = (Js_of_lam_block.field e 0l) in
        E.assign  v (E.unchecked_int32_add v (E.small_int  n))
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
  | Pidentity ->
    begin 
      match args with [e] -> e | _ -> assert false  
    end
  | Pmark_ocaml_object -> 
    begin 
      match args with 
      | [e] ->   e 
      | _ -> assert false
    end
  | Pchar_of_int -> 
    begin match args with 
      | [e] -> Js_of_lam_string.caml_char_of_int e 
      | _ -> assert false
    end
  | Pchar_to_int -> 
    begin match args with 
      | [e] -> Js_of_lam_string.caml_char_to_int e 
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
  | Pstringsetu 
  | Pstringsets ->
    begin
      Ext_log.err __LOC__ "string is immutable, %s is not available" "string.unsafe_get" ;     
      assert false (* string is immutable *)  
    end


  | Pbytesrefu 
  | Pbytesrefs ->
      begin match args with
      | [e;e1] -> Js_of_lam_string.ref_byte e e1
      | _ -> assert false
      end

   (* For bytes and string, they both return [int] in ocaml 
       we need tell Pbyteref from Pstringref
       1. Pbyteref -> a[i]
       2. Pstringref -> a.charCodeAt (a[i] is wrong)
    *)
  | Pstringrefu 
  | Pstringrefs ->
      begin match args with
      | [e;e1] -> Js_of_lam_string.ref_string e e1 
      | _ -> assert false
      end
  | Pignore -> 
      begin 
        match args with 
        | [e] -> e
        | _ -> assert false 
      end
  | Pgetglobal i   -> 
    (* TODO -- check args, case by case -- 
        1. include Array --> let include  = Array 
        2. get exception
    *)
    Lam_compile_global.get_exp (i,env,false)
  
    (** only when Lapply -> expand = true*)
  | Praise _raise_kind -> assert false (* handled before here *)
  | Prevapply _  -> 
    begin 
      match args with 
      | [arg;f] -> E.call f [arg]
      | _ -> assert  false
    end
  | Pdirapply _ -> 
    begin 
      match args with 
      | [f; arg] -> E.call f [arg]
      | _ -> assert false 
    end
  | Ploc kind ->   assert false (* already compiled away here*)

(* Runtime encoding relevant *)
  | Parraylength Pgenarray
  | Parraylength Paddrarray
  | Parraylength Pintarray
  | Parraylength Pfloatarray  -> 
      begin match args with 
      | [e] -> E.array_length e 
      | _ -> assert false
      end
  | Psetfield (i, _) -> 
      begin match args with 
      | [e0;e1] ->  (** RUNTIME *)
          decorate_side_effect cxt (Js_of_lam_block.set_field e0 (Int32.of_int i) e1)
            (*TODO: get rid of [E.unit ()]*)
      | _ -> assert false
      end
  | Psetfloatfield i -> (** RUNTIME --  RETURN VALUE SHOULD BE UNIT *)
      begin 
        match args with 
        | [e;e0] -> decorate_side_effect cxt 
                      (Js_of_lam_float_record.set_double_field e (Int32.of_int i) e0 ) 
        | _ -> assert false
      end


  | Pfloatfield i -> (** RUNTIME *)
      begin 
        match args with 
        | [e] -> Js_of_lam_float_record.get_double_feild e (Int32.of_int i) 
        | _ -> assert false 
      end
  | Parrayrefu _kind
  | Parrayrefs _kind ->  
      begin match args with
      | [e;e1] -> Js_of_lam_array.ref_array e e1 (* Todo: Constant Folding *)
      | _ -> assert false
      end
  | Pmakearray kind -> 
      Js_of_lam_array.make_array Mutable kind args 
  | Parraysetu _kind
  | Parraysets _kind -> 
      begin match args with (* wrong*)
      | [e;e0;e1] -> decorate_side_effect cxt @@ Js_of_lam_array.set_array  e e0 e1
      | _ -> assert false
      end
  | Pccall ({prim_attributes ; prim_ty } as prim) -> 
      Lam_compile_external_call.translate cxt prim args 
     (* Test if the argument is a block or an immediate integer *)
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
  | Psetglobal _  -> 
    assert false (* already handled *)
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
      E.runtime_call Js_config.bigarray 
        ("caml_ba_get_" ^ string_of_int dimension ) args 
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
        E.runtime_call Js_config.bigarray 
          ("caml_ba_set_" ^ string_of_int dimension ) args 
    end

  | Pbigarraydim i
    -> 
    E.runtime_call Js_config.bigarray
      ("caml_ba_dim_" ^ string_of_int i) args       
  | Plazyforce 
  | Pbittest 
  | Pstring_load_16 _
  | Pstring_load_32 _
  | Pstring_load_64 _
  | Pstring_set_16 _
  | Pstring_set_32 _
  | Pstring_set_64 _
  | Pbigstring_load_16 _
  | Pbigstring_load_32 _
  | Pbigstring_load_64 _
  | Pbigstring_set_16 _
  | Pbigstring_set_32 _
  | Pbigstring_set_64 _
  | Pint_as_pointer 
  | Pbswap16 
  | Pbbswap Lambda.Pnativeint
  | Pbbswap Lambda.Pint32
  | Pbbswap Lambda.Pint64 (* TODO *)

    -> 
      let comment = "Missing primitve" in       
      let s = Lam_util.string_of_primitive prim in
      let warn = Printf.sprintf  "%s: %s\n" comment s in
      Ext_log.warn __LOC__ "%s"  warn;
      (*we dont use [throw] here, since [throw] is an statement  *)        
      E.dump  Error [ E.str warn]


