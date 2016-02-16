(* OCamlScript compiler
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
open Lambda
  
type ident = Ident.t

class virtual fold =
  object ((o : 'self_type))
    method string : string -> 'self_type = o#unknown
    method ref :
      'a. ('self_type -> 'a -> 'self_type) -> 'a ref -> 'self_type =
      fun _f_a { contents = _x } -> let o = _f_a o _x in o
    method option :
      'a. ('self_type -> 'a -> 'self_type) -> 'a option -> 'self_type =
      fun _f_a -> function | None -> o | Some _x -> let o = _f_a o _x in o
    method list :
      'a. ('self_type -> 'a -> 'self_type) -> 'a list -> 'self_type =
      fun _f_a ->
        function
        | [] -> o
        | _x :: _x_i1 -> let o = _f_a o _x in let o = o#list _f_a _x_i1 in o
    method int : int -> 'self_type = o#unknown
    method bool : bool -> 'self_type = function | false -> o | true -> o
    method tag_info : tag_info -> 'self_type =
      function
      | Constructor _x -> let o = o#string _x in o
      | Tuple -> o
      | Array -> o
      | Variant _x -> let o = o#string _x in o
      | Record -> o
      | NA -> o
    method structured_constant : structured_constant -> 'self_type =
      function
      | Const_base _x -> let o = o#unknown _x in o
      | Const_pointer (_x, _x_i1) ->
          let o = o#int _x in let o = o#pointer_info _x_i1 in o
      | Const_block (_x, _x_i1, _x_i2) ->
          let o = o#int _x in
          let o = o#tag_info _x_i1 in
          let o = o#list (fun o -> o#structured_constant) _x_i2 in o
      | Const_float_array _x -> let o = o#list (fun o -> o#string) _x in o
      | Const_immstring _x -> let o = o#string _x in o
    method shared_code : shared_code -> 'self_type =
      (* OCamlScript compiler
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
      (* Author: Hongbo Zhang  *) (* exception/ Record labels *)
      (* Globals *) (* Operations on heap blocks *) (* Force lazy values *)
      (* External call *) (* Exceptions *) (* Boolean operations *)
      (* Integer operations *) (* Float operations *) (* String operations *)
      (* Array operations *)
      (* Test if the argument is a block or an immediate integer *)
      (* Test if the (integer) argument is outside an interval *)
      (* Bitvect operations *)
      (* Operations on boxed integers (Nativeint.t, Int32.t, Int64.t) *)
      (*source*) (*destination*)
      (* Operations on big arrays: (unsafe, #dimensions, kind, layout) *)
      (* size of the nth dimension of a big array *)
      (* load/set 16,32,64 bits from a string: (unsafe)*)
      (* load/set 16,32,64 bits from a
     (char, int8_unsigned_elt, c_layout) Bigarray.Array1.t : (unsafe) *)
      (* Compile time constants *) (* byte swap *)
      (* Integer to external pointer *) (* label name *)
      o#list
        (fun o (_x, _x_i1) -> let o = o#int _x in let o = o#int _x_i1 in o)
    method raise_kind : raise_kind -> 'self_type =
      function | Raise_regular -> o | Raise_reraise -> o | Raise_notrace -> o
    method public_info : public_info -> 'self_type =
      o#option (fun o -> o#string)
    method primitive : primitive -> 'self_type =
      function
      | Pidentity -> o
      | Pbytes_to_string -> o
      | Pbytes_of_string -> o
      | Pchar_to_int -> o
      | Pchar_of_int -> o
      | Pmark_ocaml_object -> o
      | Pignore -> o
      | Prevapply _x -> let o = o#unknown _x in o
      | Pdirapply _x -> let o = o#unknown _x in o
      | Ploc _x -> let o = o#loc_kind _x in o
      | Pgetglobal _x -> let o = o#ident _x in o
      | Psetglobal _x -> let o = o#ident _x in o
      | Pmakeblock (_x, _x_i1, _x_i2) ->
          let o = o#int _x in
          let o = o#tag_info _x_i1 in let o = o#unknown _x_i2 in o
      | Pfield _x -> let o = o#int _x in o
      | Psetfield (_x, _x_i1) ->
          let o = o#int _x in let o = o#bool _x_i1 in o
      | Pfloatfield _x -> let o = o#int _x in o
      | Psetfloatfield _x -> let o = o#int _x in o
      | Pduprecord (_x, _x_i1) ->
          let o = o#unknown _x in let o = o#int _x_i1 in o
      | Plazyforce -> o
      | Pccall _x -> let o = o#unknown _x in o
      | Praise _x -> let o = o#raise_kind _x in o
      | Psequand -> o
      | Psequor -> o
      | Pnot -> o
      | Pnegint -> o
      | Paddint -> o
      | Psubint -> o
      | Pmulint -> o
      | Pdivint -> o
      | Pmodint -> o
      | Pandint -> o
      | Porint -> o
      | Pxorint -> o
      | Plslint -> o
      | Plsrint -> o
      | Pasrint -> o
      | Pintcomp _x -> let o = o#comparison _x in o
      | Poffsetint _x -> let o = o#int _x in o
      | Poffsetref _x -> let o = o#int _x in o
      | Pintoffloat -> o
      | Pfloatofint -> o
      | Pnegfloat -> o
      | Pabsfloat -> o
      | Paddfloat -> o
      | Psubfloat -> o
      | Pmulfloat -> o
      | Pdivfloat -> o
      | Pfloatcomp _x -> let o = o#comparison _x in o
      | Pstringlength -> o
      | Pstringrefu -> o
      | Pstringsetu -> o
      | Pstringrefs -> o
      | Pstringsets -> o
      | Pbyteslength -> o
      | Pbytesrefu -> o
      | Pbytessetu -> o
      | Pbytesrefs -> o
      | Pbytessets -> o
      | Pmakearray _x -> let o = o#array_kind _x in o
      | Parraylength _x -> let o = o#array_kind _x in o
      | Parrayrefu _x -> let o = o#array_kind _x in o
      | Parraysetu _x -> let o = o#array_kind _x in o
      | Parrayrefs _x -> let o = o#array_kind _x in o
      | Parraysets _x -> let o = o#array_kind _x in o
      | Pisint -> o
      | Pisout -> o
      | Pbittest -> o
      | Pbintofint _x -> let o = o#boxed_integer _x in o
      | Pintofbint _x -> let o = o#boxed_integer _x in o
      | Pcvtbint (_x, _x_i1) ->
          let o = o#boxed_integer _x in let o = o#boxed_integer _x_i1 in o
      | Pnegbint _x -> let o = o#boxed_integer _x in o
      | Paddbint _x -> let o = o#boxed_integer _x in o
      | Psubbint _x -> let o = o#boxed_integer _x in o
      | Pmulbint _x -> let o = o#boxed_integer _x in o
      | Pdivbint _x -> let o = o#boxed_integer _x in o
      | Pmodbint _x -> let o = o#boxed_integer _x in o
      | Pandbint _x -> let o = o#boxed_integer _x in o
      | Porbint _x -> let o = o#boxed_integer _x in o
      | Pxorbint _x -> let o = o#boxed_integer _x in o
      | Plslbint _x -> let o = o#boxed_integer _x in o
      | Plsrbint _x -> let o = o#boxed_integer _x in o
      | Pasrbint _x -> let o = o#boxed_integer _x in o
      | Pbintcomp (_x, _x_i1) ->
          let o = o#boxed_integer _x in let o = o#comparison _x_i1 in o
      | Pbigarrayref (_x, _x_i1, _x_i2, _x_i3) ->
          let o = o#bool _x in
          let o = o#int _x_i1 in
          let o = o#bigarray_kind _x_i2 in
          let o = o#bigarray_layout _x_i3 in o
      | Pbigarrayset (_x, _x_i1, _x_i2, _x_i3) ->
          let o = o#bool _x in
          let o = o#int _x_i1 in
          let o = o#bigarray_kind _x_i2 in
          let o = o#bigarray_layout _x_i3 in o
      | Pbigarraydim _x -> let o = o#int _x in o
      | Pstring_load_16 _x -> let o = o#bool _x in o
      | Pstring_load_32 _x -> let o = o#bool _x in o
      | Pstring_load_64 _x -> let o = o#bool _x in o
      | Pstring_set_16 _x -> let o = o#bool _x in o
      | Pstring_set_32 _x -> let o = o#bool _x in o
      | Pstring_set_64 _x -> let o = o#bool _x in o
      | Pbigstring_load_16 _x -> let o = o#bool _x in o
      | Pbigstring_load_32 _x -> let o = o#bool _x in o
      | Pbigstring_load_64 _x -> let o = o#bool _x in o
      | Pbigstring_set_16 _x -> let o = o#bool _x in o
      | Pbigstring_set_32 _x -> let o = o#bool _x in o
      | Pbigstring_set_64 _x -> let o = o#bool _x in o
      | Pctconst _x -> let o = o#compile_time_constant _x in o
      | Pbswap16 -> o
      | Pbbswap _x -> let o = o#boxed_integer _x in o
      | Pint_as_pointer -> o
    method pointer_info : pointer_info -> 'self_type =
      function
      | NullConstructor _x -> let o = o#string _x in o
      | NullVariant _x -> let o = o#string _x in o
      | NAPointer -> o
    method meth_kind : meth_kind -> 'self_type =
      function
      | Self -> o
      | Public _x -> let o = o#public_info _x in o
      | Cached -> o
    method loc_kind : loc_kind -> 'self_type =
      function
      | Loc_FILE -> o
      | Loc_LINE -> o
      | Loc_MODULE -> o
      | Loc_LOC -> o
      | Loc_POS -> o
    method let_kind : let_kind -> 'self_type =
      function | Strict -> o | Alias -> o | StrictOpt -> o | Variable -> o
    method lambda_switch : lambda_switch -> 'self_type =
      fun
        {
          sw_numconsts = _x;
          sw_consts = _x_i1;
          sw_numblocks = _x_i2;
          sw_blocks = _x_i3;
          sw_failaction = _x_i4
        } ->
        let o = o#int _x in
        let o =
          o#list
            (fun o (_x, _x_i1) ->
               let o = o#int _x in let o = o#lambda _x_i1 in o)
            _x_i1 in
        let o = o#int _x_i2 in
        let o =
          o#list
            (fun o (_x, _x_i1) ->
               let o = o#int _x in let o = o#lambda _x_i1 in o)
            _x_i3 in
        let o = o#option (fun o -> o#lambda) _x_i4 in o
    method lambda_event_kind : lambda_event_kind -> 'self_type =
      function
      | Lev_before -> o
      | Lev_after _x -> let o = o#unknown _x in o
      | Lev_function -> o
    method lambda_event : lambda_event -> 'self_type =
      fun { lev_loc = _x; lev_kind = _x_i1; lev_repr = _x_i2; lev_env = _x_i3
        } ->
        let o = o#unknown _x in
        let o = o#lambda_event_kind _x_i1 in
        let o = o#option (fun o -> o#ref (fun o -> o#int)) _x_i2 in
        let o = o#unknown _x_i3 in o
    method lambda : lambda -> 'self_type =
      function
      | Lvar _x -> let o = o#ident _x in o
      | Lconst _x -> let o = o#structured_constant _x in o
      | Lapply (_x, _x_i1, _x_i2) ->
          let o = o#lambda _x in
          let o = o#list (fun o -> o#lambda) _x_i1 in
          let o = o#apply_info _x_i2 in o
      | Lfunction (_x, _x_i1, _x_i2) ->
          let o = o#function_kind _x in
          let o = o#list (fun o -> o#ident) _x_i1 in
          let o = o#lambda _x_i2 in o
      | Llet (_x, _x_i1, _x_i2, _x_i3) ->
          let o = o#let_kind _x in
          let o = o#ident _x_i1 in
          let o = o#lambda _x_i2 in let o = o#lambda _x_i3 in o
      | Lletrec (_x, _x_i1) ->
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#ident _x in let o = o#lambda _x_i1 in o)
              _x in
          let o = o#lambda _x_i1 in o
      | Lprim (_x, _x_i1) ->
          let o = o#primitive _x in
          let o = o#list (fun o -> o#lambda) _x_i1 in o
      | Lswitch (_x, _x_i1) ->
          let o = o#lambda _x in let o = o#lambda_switch _x_i1 in o
      | Lstringswitch (_x, _x_i1, _x_i2) ->
          let o = o#lambda _x in
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#string _x in let o = o#lambda _x_i1 in o)
              _x_i1 in
          let o = o#option (fun o -> o#lambda) _x_i2 in o
      | Lstaticraise (_x, _x_i1) ->
          let o = o#int _x in let o = o#list (fun o -> o#lambda) _x_i1 in o
      | Lstaticcatch (_x, _x_i1, _x_i2) ->
          let o = o#lambda _x in
          let o =
            (fun (_x, _x_i1) ->
               let o = o#int _x in
               let o = o#list (fun o -> o#ident) _x_i1 in o)
              _x_i1 in
          let o = o#lambda _x_i2 in o
      | Ltrywith (_x, _x_i1, _x_i2) ->
          let o = o#lambda _x in
          let o = o#ident _x_i1 in let o = o#lambda _x_i2 in o
      | Lifthenelse (_x, _x_i1, _x_i2) ->
          let o = o#lambda _x in
          let o = o#lambda _x_i1 in let o = o#lambda _x_i2 in o
      | Lsequence (_x, _x_i1) ->
          let o = o#lambda _x in let o = o#lambda _x_i1 in o
      | Lwhile (_x, _x_i1) ->
          let o = o#lambda _x in let o = o#lambda _x_i1 in o
      | Lfor (_x, _x_i1, _x_i2, _x_i3, _x_i4) ->
          let o = o#ident _x in
          let o = o#lambda _x_i1 in
          let o = o#lambda _x_i2 in
          let o = o#unknown _x_i3 in let o = o#lambda _x_i4 in o
      | Lassign (_x, _x_i1) ->
          let o = o#ident _x in let o = o#lambda _x_i1 in o
      | Lsend (_x, _x_i1, _x_i2, _x_i3, _x_i4) ->
          let o = o#meth_kind _x in
          let o = o#lambda _x_i1 in
          let o = o#lambda _x_i2 in
          let o = o#list (fun o -> o#lambda) _x_i3 in
          let o = o#unknown _x_i4 in o
      | Levent (_x, _x_i1) ->
          let o = o#lambda _x in let o = o#lambda_event _x_i1 in o
      | Lifused (_x, _x_i1) ->
          let o = o#ident _x in let o = o#lambda _x_i1 in o
    method ident : ident -> 'self_type = o#unknown
    method function_kind : function_kind -> 'self_type =
      function | Curried -> o | Tupled -> o
    method compile_time_constant : compile_time_constant -> 'self_type =
      function
      | Big_endian -> o
      | Word_size -> o
      | Ostype_unix -> o
      | Ostype_win32 -> o
      | Ostype_cygwin -> o
    method comparison : comparison -> 'self_type =
      function
      | Ceq -> o
      | Cneq -> o
      | Clt -> o
      | Cgt -> o
      | Cle -> o
      | Cge -> o
    method boxed_integer : boxed_integer -> 'self_type =
      function | Pnativeint -> o | Pint32 -> o | Pint64 -> o
    method bigarray_layout : bigarray_layout -> 'self_type =
      function
      | Pbigarray_unknown_layout -> o
      | Pbigarray_c_layout -> o
      | Pbigarray_fortran_layout -> o
    method bigarray_kind : bigarray_kind -> 'self_type =
      function
      | Pbigarray_unknown -> o
      | Pbigarray_float32 -> o
      | Pbigarray_float64 -> o
      | Pbigarray_sint8 -> o
      | Pbigarray_uint8 -> o
      | Pbigarray_sint16 -> o
      | Pbigarray_uint16 -> o
      | Pbigarray_int32 -> o
      | Pbigarray_int64 -> o
      | Pbigarray_caml_int -> o
      | Pbigarray_native_int -> o
      | Pbigarray_complex32 -> o
      | Pbigarray_complex64 -> o
    method array_kind : array_kind -> 'self_type =
      function
      | Pgenarray -> o
      | Paddrarray -> o
      | Pintarray -> o
      | Pfloatarray -> o
    method apply_status : apply_status -> 'self_type =
      function | NA -> o | Full -> o
    method apply_info : apply_info -> 'self_type =
      fun { apply_loc = _x; apply_status = _x_i1 } ->
        let o = o#unknown _x in let o = o#apply_status _x_i1 in o
    method unknown : 'a. 'a -> 'self_type = fun _ -> o
  end
  

