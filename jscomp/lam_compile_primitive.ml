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
  | (Assign _ | Declare _ | NeedValue), _  -> E.seq e (E.unit ())
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
      tag_info (E.int tag) args 
  | Pfield i -> 
    begin match args with 
      | [ e ]  -> Js_of_lam_block.field e i (* Invariant depends on runtime *)
      | _ -> assert false
    end
  | (Pnegint | Pnegbint _ | Pnegfloat) ->
    begin match args with
      | [ e ] -> E.int32_minus (E.int 0)  e 
      | _ -> assert false
    end
  | Pnot ->
    begin match args with
      | [e] ->  E.not  e 
      | _ -> assert false
    end
  | Poffsetint n ->
    begin match args with
      | [e] ->  E.int32_add  e (E.int n) 
      | _ -> assert false
    end
  | Poffsetref n ->
    begin match args with
      | [e] -> 
        let v = (Js_of_lam_block.field e 0) in
        E.assign  v (E.int32_add v (E.int n))
      | _ -> assert false
    end
  | Paddint | Paddbint _
    ->
    begin match args with
      | [e1;e2] ->
        E.int32_add  e1  e2
      | _ -> assert false
    end
 | Paddfloat
     -> 
    begin match args with
      | [e1;e2] ->
        E.float_add  e1  e2
      | _ -> assert false
    end
  | Psubint | Psubbint _ 
    -> 
    begin match args with
      | [e1;e2] ->
          E.int32_minus   e1  e2
      | _ -> assert false end
  | Psubfloat
    ->
      begin match args with
      | [e1;e2] ->
          E.float_minus   e1  e2
      | _ -> assert false 
      end
  | Pmulint | Pmulbint _ 
    ->
    begin match args with
      | [e1; e2]  ->
        E.int32_mul  e1  e2
      | _ -> assert false 
    end
  | Pmulfloat 
    -> 
      begin match args with
      | [e1; e2]  ->
          E.float_mul  e1  e2
      | _ -> assert false 
      end
  | Pdivfloat -> 
    begin match args with  (* TODO: see ocamljs -- assertion*)
      | [e1;e2] -> E.float_div  e1  e2
      | _ -> assert false end
  | (  Pdivint | Pdivbint _)->
    begin match args with  (* TODO: see ocamljs -- assertion*)
      | [e1;e2] ->
        E.int32_div e1 e2  (** 32 bits  *)
      | _ -> assert false end
  | Pmodint | Pmodbint _ ->
    begin match args with
      | [e1; e2] ->
        E.int32_mod   e1  e2
      | _ -> assert false 
    end
  | Plslint | Plslbint _ ->
    begin match args with
      | [e1;e2] ->
        E.int32_lsl e1  e2
      | _ -> assert false 
    end
  | (Plsrint | Plsrbint _) ->
    begin match args with
      | [e1; e2] ->
        E.int32_lsr   e1  e2
      | _ -> assert false
    end
  | (Pasrint | Pasrbint _) ->
    begin match args with
      | [e1;e2] ->
        E.int32_asr  e1  e2
      | _ -> assert false
    end

  | Pandint | Pandbint _->
    begin match args with
      | [e1;e2] ->
        E.int32_band  e1  e2
      | _ -> assert false
    end
  | Porint | Porbint _ ->
    begin match args with
      | [e1;e2] ->
        E.int32_bor  e1  e2
      | _ -> assert false
    end
  | Pxorint | Pxorbint _ ->
    begin match args with
      | [e1;e2] ->
        E.int32_bxor  e1  e2
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
      | [e] ->  E.tag_ml_obj e 
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
  | Pbintcomp (_, cmp)
  | Pfloatcomp cmp 
  | Pintcomp cmp ->
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
  | Pintoffloat -> 
    begin
      match args with 
      | [e] -> e 
      | _ -> assert false 
    end
(* Runtime encoding relevant *)
  | Parraylength _  -> 
      begin match args with 
      | [e] -> E.array_length e 
      | _ -> assert false
      end
  | Psetfield (i, _) -> 
      begin match args with 
      | [e0;e1] ->  (** RUNTIME *)
          decorate_side_effect cxt (Js_of_lam_block.set_field e0 i e1)
            (*TODO: get rid of [E.unit ()]*)
      | _ -> assert false
      end
  | Psetfloatfield i -> (** RUNTIME --  RETURN VALUE SHOULD BE UNIT *)
      begin 
        match args with 
        | [e;e0] -> decorate_side_effect cxt (Js_of_lam_float_record.set_double_field e i e0 ) 
        | _ -> assert false
      end


  | Pfloatfield i -> (** RUNTIME *)
      begin 
        match args with 
        | [e] -> Js_of_lam_float_record.get_double_feild e i 
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
  | Pbintofint _
  | Pintofbint _
  | Pfloatofint -> 
      begin match args with 
      | [e] -> e 
      | _ -> assert false 
      end
  | Pabsfloat -> 
    begin match args with 
      | [e] ->
        E.math "abs" [e]
        (* GCC treat built-ins like Math in a dirfferent way*)
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
        if Sys.big_endian then  E.true_
        else E.false_
      | Word_size -> 
        E.int Sys.word_size
      | Ostype_unix -> 
        if Sys.unix then E.true_ else E.false_
      | Ostype_win32 -> 
        if Sys.win32 then E.true_ else E.false_
      | Ostype_cygwin -> 
        if Sys.cygwin then E.true_ else E.false_
    end
  | Pcvtbint (_boxed_integer_source, _boxed_integer_dest) ->
    begin 
      match args with 
      | [e0] -> e0 (* TODO: int64 is not supported yet *)
      | _ -> assert false
    end
  | Psetglobal _  -> 
    assert false (* already handled *)
    (* assert false *)
  | Pduprecord ( (Record_regular | Record_float),  _) -> 
    (* this is due to we encode record as an array, it is going to change
       if we have another encoding       
    *)    
    begin match args with 
    | [e] -> E.array_copy e
    | _ -> assert false       
    end
  | Plazyforce
  | Pbittest 
  | Pbigarrayref (_, _, _, _)
  | Pbigarrayset (_, _, _, _)
  | Pbigarraydim _
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
  | Pbswap16
  | Pbbswap _
  | Pint_as_pointer  -> 
      let comment = "Missing primitve" in       
      let s = Lam_util.string_of_primitive prim in
      let warn = Printf.sprintf  "%s: %s\n" comment s in
      Ext_log.warn __LOC__ "%s"  warn;
      (*we dont use [throw] here, since [throw] is an statement  *)        
      E.dump  Error [ E.str warn]


