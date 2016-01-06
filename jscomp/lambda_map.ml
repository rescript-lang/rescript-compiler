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

class virtual map =
  object ((o : 'self_type))
    method string : string -> string = o#unknown
    method ref :
      'a 'a_out. ('self_type -> 'a -> 'a_out) -> 'a ref -> 'a_out ref =
      fun _f_a { contents = _x } -> let _x = _f_a o _x in { contents = _x; }
    method option :
      'a 'a_out. ('self_type -> 'a -> 'a_out) -> 'a option -> 'a_out option =
      fun _f_a ->
        function | None -> None | Some _x -> let _x = _f_a o _x in Some _x
    method list :
      'a 'a_out. ('self_type -> 'a -> 'a_out) -> 'a list -> 'a_out list =
      fun _f_a ->
        function
        | [] -> []
        | _x :: _x_i1 ->
            let _x = _f_a o _x in
            let _x_i1 = o#list _f_a _x_i1 in _x :: _x_i1
    method int : int -> int = o#unknown
    method bool : bool -> bool = function | false -> false | true -> true
    method tag_info : tag_info -> tag_info =
      function
      | Constructor _x -> let _x = o#string _x in Constructor _x
      | Tuple -> Tuple
      | Array -> Array
      | Variant _x -> let _x = o#string _x in Variant _x
      | Record -> Record
      | NA -> NA
    method structured_constant : structured_constant -> structured_constant =
      function
      | Const_base _x -> let _x = o#unknown _x in Const_base _x
      | Const_pointer (_x, _x_i1) ->
          let _x = o#int _x in
          let _x_i1 = o#pointer_info _x_i1 in Const_pointer (_x, _x_i1)
      | Const_block (_x, _x_i1, _x_i2) ->
          let _x = o#int _x in
          let _x_i1 = o#tag_info _x_i1 in
          let _x_i2 = o#list (fun o -> o#structured_constant) _x_i2
          in Const_block (_x, _x_i1, _x_i2)
      | Const_float_array _x ->
          let _x = o#list (fun o -> o#string) _x in Const_float_array _x
      | Const_immstring _x -> let _x = o#string _x in Const_immstring _x
    method shared_code : shared_code -> shared_code = (* Globals *)
      (* Operations on heap blocks *) (* Force lazy values *)
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
        (fun o (_x, _x_i1) ->
           let _x = o#int _x in let _x_i1 = o#int _x_i1 in (_x, _x_i1))
    method raise_kind : raise_kind -> raise_kind =
      function
      | Raise_regular -> Raise_regular
      | Raise_reraise -> Raise_reraise
      | Raise_notrace -> Raise_notrace
    method public_info : public_info -> public_info =
      o#option (fun o -> o#string)
    method primitive : primitive -> primitive =
      function
      | Pidentity -> Pidentity
      | Pbytes_to_string -> Pbytes_to_string
      | Pbytes_of_string -> Pbytes_of_string
      | Pchar_to_int -> Pchar_to_int
      | Pchar_of_int -> Pchar_of_int
      | Pmark_ocaml_object -> Pmark_ocaml_object
      | Pignore -> Pignore
      | Prevapply _x -> let _x = o#unknown _x in Prevapply _x
      | Pdirapply _x -> let _x = o#unknown _x in Pdirapply _x
      | Ploc _x -> let _x = o#loc_kind _x in Ploc _x
      | Pgetglobal _x -> let _x = o#ident _x in Pgetglobal _x
      | Psetglobal _x -> let _x = o#ident _x in Psetglobal _x
      | Pmakeblock (_x, _x_i1, _x_i2) ->
          let _x = o#int _x in
          let _x_i1 = o#tag_info _x_i1 in
          let _x_i2 = o#unknown _x_i2 in Pmakeblock (_x, _x_i1, _x_i2)
      | Pfield _x -> let _x = o#int _x in Pfield _x
      | Psetfield (_x, _x_i1) ->
          let _x = o#int _x in
          let _x_i1 = o#bool _x_i1 in Psetfield (_x, _x_i1)
      | Pfloatfield _x -> let _x = o#int _x in Pfloatfield _x
      | Psetfloatfield _x -> let _x = o#int _x in Psetfloatfield _x
      | Pduprecord (_x, _x_i1) ->
          let _x = o#unknown _x in
          let _x_i1 = o#int _x_i1 in Pduprecord (_x, _x_i1)
      | Plazyforce -> Plazyforce
      | Pccall _x -> let _x = o#unknown _x in Pccall _x
      | Praise _x -> let _x = o#raise_kind _x in Praise _x
      | Psequand -> Psequand
      | Psequor -> Psequor
      | Pnot -> Pnot
      | Pnegint -> Pnegint
      | Paddint -> Paddint
      | Psubint -> Psubint
      | Pmulint -> Pmulint
      | Pdivint -> Pdivint
      | Pmodint -> Pmodint
      | Pandint -> Pandint
      | Porint -> Porint
      | Pxorint -> Pxorint
      | Plslint -> Plslint
      | Plsrint -> Plsrint
      | Pasrint -> Pasrint
      | Pintcomp _x -> let _x = o#comparison _x in Pintcomp _x
      | Poffsetint _x -> let _x = o#int _x in Poffsetint _x
      | Poffsetref _x -> let _x = o#int _x in Poffsetref _x
      | Pintoffloat -> Pintoffloat
      | Pfloatofint -> Pfloatofint
      | Pnegfloat -> Pnegfloat
      | Pabsfloat -> Pabsfloat
      | Paddfloat -> Paddfloat
      | Psubfloat -> Psubfloat
      | Pmulfloat -> Pmulfloat
      | Pdivfloat -> Pdivfloat
      | Pfloatcomp _x -> let _x = o#comparison _x in Pfloatcomp _x
      | Pstringlength -> Pstringlength
      | Pstringrefu -> Pstringrefu
      | Pstringsetu -> Pstringsetu
      | Pstringrefs -> Pstringrefs
      | Pstringsets -> Pstringsets
      | Pbyteslength -> Pbyteslength
      | Pbytesrefu -> Pbytesrefu
      | Pbytessetu -> Pbytessetu
      | Pbytesrefs -> Pbytesrefs
      | Pbytessets -> Pbytessets
      | Pmakearray _x -> let _x = o#array_kind _x in Pmakearray _x
      | Parraylength _x -> let _x = o#array_kind _x in Parraylength _x
      | Parrayrefu _x -> let _x = o#array_kind _x in Parrayrefu _x
      | Parraysetu _x -> let _x = o#array_kind _x in Parraysetu _x
      | Parrayrefs _x -> let _x = o#array_kind _x in Parrayrefs _x
      | Parraysets _x -> let _x = o#array_kind _x in Parraysets _x
      | Pisint -> Pisint
      | Pisout -> Pisout
      | Pbittest -> Pbittest
      | Pbintofint _x -> let _x = o#boxed_integer _x in Pbintofint _x
      | Pintofbint _x -> let _x = o#boxed_integer _x in Pintofbint _x
      | Pcvtbint (_x, _x_i1) ->
          let _x = o#boxed_integer _x in
          let _x_i1 = o#boxed_integer _x_i1 in Pcvtbint (_x, _x_i1)
      | Pnegbint _x -> let _x = o#boxed_integer _x in Pnegbint _x
      | Paddbint _x -> let _x = o#boxed_integer _x in Paddbint _x
      | Psubbint _x -> let _x = o#boxed_integer _x in Psubbint _x
      | Pmulbint _x -> let _x = o#boxed_integer _x in Pmulbint _x
      | Pdivbint _x -> let _x = o#boxed_integer _x in Pdivbint _x
      | Pmodbint _x -> let _x = o#boxed_integer _x in Pmodbint _x
      | Pandbint _x -> let _x = o#boxed_integer _x in Pandbint _x
      | Porbint _x -> let _x = o#boxed_integer _x in Porbint _x
      | Pxorbint _x -> let _x = o#boxed_integer _x in Pxorbint _x
      | Plslbint _x -> let _x = o#boxed_integer _x in Plslbint _x
      | Plsrbint _x -> let _x = o#boxed_integer _x in Plsrbint _x
      | Pasrbint _x -> let _x = o#boxed_integer _x in Pasrbint _x
      | Pbintcomp (_x, _x_i1) ->
          let _x = o#boxed_integer _x in
          let _x_i1 = o#comparison _x_i1 in Pbintcomp (_x, _x_i1)
      | Pbigarrayref (_x, _x_i1, _x_i2, _x_i3) ->
          let _x = o#bool _x in
          let _x_i1 = o#int _x_i1 in
          let _x_i2 = o#bigarray_kind _x_i2 in
          let _x_i3 = o#bigarray_layout _x_i3
          in Pbigarrayref (_x, _x_i1, _x_i2, _x_i3)
      | Pbigarrayset (_x, _x_i1, _x_i2, _x_i3) ->
          let _x = o#bool _x in
          let _x_i1 = o#int _x_i1 in
          let _x_i2 = o#bigarray_kind _x_i2 in
          let _x_i3 = o#bigarray_layout _x_i3
          in Pbigarrayset (_x, _x_i1, _x_i2, _x_i3)
      | Pbigarraydim _x -> let _x = o#int _x in Pbigarraydim _x
      | Pstring_load_16 _x -> let _x = o#bool _x in Pstring_load_16 _x
      | Pstring_load_32 _x -> let _x = o#bool _x in Pstring_load_32 _x
      | Pstring_load_64 _x -> let _x = o#bool _x in Pstring_load_64 _x
      | Pstring_set_16 _x -> let _x = o#bool _x in Pstring_set_16 _x
      | Pstring_set_32 _x -> let _x = o#bool _x in Pstring_set_32 _x
      | Pstring_set_64 _x -> let _x = o#bool _x in Pstring_set_64 _x
      | Pbigstring_load_16 _x -> let _x = o#bool _x in Pbigstring_load_16 _x
      | Pbigstring_load_32 _x -> let _x = o#bool _x in Pbigstring_load_32 _x
      | Pbigstring_load_64 _x -> let _x = o#bool _x in Pbigstring_load_64 _x
      | Pbigstring_set_16 _x -> let _x = o#bool _x in Pbigstring_set_16 _x
      | Pbigstring_set_32 _x -> let _x = o#bool _x in Pbigstring_set_32 _x
      | Pbigstring_set_64 _x -> let _x = o#bool _x in Pbigstring_set_64 _x
      | Pctconst _x -> let _x = o#compile_time_constant _x in Pctconst _x
      | Pbswap16 -> Pbswap16
      | Pbbswap _x -> let _x = o#boxed_integer _x in Pbbswap _x
      | Pint_as_pointer -> Pint_as_pointer
    method pointer_info : pointer_info -> pointer_info =
      function
      | NullConstructor _x -> let _x = o#string _x in NullConstructor _x
      | NullVariant _x -> let _x = o#string _x in NullVariant _x
      | NAPointer -> NAPointer
    method meth_kind : meth_kind -> meth_kind =
      function
      | Self -> Self
      | Public _x -> let _x = o#public_info _x in Public _x
      | Cached -> Cached
    method loc_kind : loc_kind -> loc_kind =
      function
      | Loc_FILE -> Loc_FILE
      | Loc_LINE -> Loc_LINE
      | Loc_MODULE -> Loc_MODULE
      | Loc_LOC -> Loc_LOC
      | Loc_POS -> Loc_POS
    method let_kind : let_kind -> let_kind =
      function
      | Strict -> Strict
      | Alias -> Alias
      | StrictOpt -> StrictOpt
      | Variable -> Variable
    method lambda_switch : lambda_switch -> lambda_switch =
      fun
        {
          sw_numconsts = _x;
          sw_consts = _x_i1;
          sw_numblocks = _x_i2;
          sw_blocks = _x_i3;
          sw_failaction = _x_i4
        } ->
        let _x = o#int _x in
        let _x_i1 =
          o#list
            (fun o (_x, _x_i1) ->
               let _x = o#int _x in let _x_i1 = o#lambda _x_i1 in (_x, _x_i1))
            _x_i1 in
        let _x_i2 = o#int _x_i2 in
        let _x_i3 =
          o#list
            (fun o (_x, _x_i1) ->
               let _x = o#int _x in let _x_i1 = o#lambda _x_i1 in (_x, _x_i1))
            _x_i3 in
        let _x_i4 = o#option (fun o -> o#lambda) _x_i4
        in
          {
            sw_numconsts = _x;
            sw_consts = _x_i1;
            sw_numblocks = _x_i2;
            sw_blocks = _x_i3;
            sw_failaction = _x_i4;
          }
    method lambda_event_kind : lambda_event_kind -> lambda_event_kind =
      function
      | Lev_before -> Lev_before
      | Lev_after _x -> let _x = o#unknown _x in Lev_after _x
      | Lev_function -> Lev_function
    method lambda_event : lambda_event -> lambda_event =
      fun { lev_loc = _x; lev_kind = _x_i1; lev_repr = _x_i2; lev_env = _x_i3
        } ->
        let _x = o#unknown _x in
        let _x_i1 = o#lambda_event_kind _x_i1 in
        let _x_i2 = o#option (fun o -> o#ref (fun o -> o#int)) _x_i2 in
        let _x_i3 = o#unknown _x_i3
        in
          {
            lev_loc = _x;
            lev_kind = _x_i1;
            lev_repr = _x_i2;
            lev_env = _x_i3;
          }
    method lambda : lambda -> lambda =
      function
      | Lvar _x -> let _x = o#ident _x in Lvar _x
      | Lconst _x -> let _x = o#structured_constant _x in Lconst _x
      | Lapply (_x, _x_i1, _x_i2) ->
          let _x = o#lambda _x in
          let _x_i1 = o#list (fun o -> o#lambda) _x_i1 in
          let _x_i2 = o#apply_info _x_i2 in Lapply (_x, _x_i1, _x_i2)
      | Lfunction (_x, _x_i1, _x_i2) ->
          let _x = o#function_kind _x in
          let _x_i1 = o#list (fun o -> o#ident) _x_i1 in
          let _x_i2 = o#lambda _x_i2 in Lfunction (_x, _x_i1, _x_i2)
      | Llet (_x, _x_i1, _x_i2, _x_i3) ->
          let _x = o#let_kind _x in
          let _x_i1 = o#ident _x_i1 in
          let _x_i2 = o#lambda _x_i2 in
          let _x_i3 = o#lambda _x_i3 in Llet (_x, _x_i1, _x_i2, _x_i3)
      | Lletrec (_x, _x_i1) ->
          let _x =
            o#list
              (fun o (_x, _x_i1) ->
                 let _x = o#ident _x in
                 let _x_i1 = o#lambda _x_i1 in (_x, _x_i1))
              _x in
          let _x_i1 = o#lambda _x_i1 in Lletrec (_x, _x_i1)
      | Lprim (_x, _x_i1) ->
          let _x = o#primitive _x in
          let _x_i1 = o#list (fun o -> o#lambda) _x_i1 in Lprim (_x, _x_i1)
      | Lswitch (_x, _x_i1) ->
          let _x = o#lambda _x in
          let _x_i1 = o#lambda_switch _x_i1 in Lswitch (_x, _x_i1)
      | Lstringswitch (_x, _x_i1, _x_i2) ->
          let _x = o#lambda _x in
          let _x_i1 =
            o#list
              (fun o (_x, _x_i1) ->
                 let _x = o#string _x in
                 let _x_i1 = o#lambda _x_i1 in (_x, _x_i1))
              _x_i1 in
          let _x_i2 = o#option (fun o -> o#lambda) _x_i2
          in Lstringswitch (_x, _x_i1, _x_i2)
      | Lstaticraise (_x, _x_i1) ->
          let _x = o#int _x in
          let _x_i1 = o#list (fun o -> o#lambda) _x_i1
          in Lstaticraise (_x, _x_i1)
      | Lstaticcatch (_x, _x_i1, _x_i2) ->
          let _x = o#lambda _x in
          let _x_i1 =
            (fun (_x, _x_i1) ->
               let _x = o#int _x in
               let _x_i1 = o#list (fun o -> o#ident) _x_i1 in (_x, _x_i1))
              _x_i1 in
          let _x_i2 = o#lambda _x_i2 in Lstaticcatch (_x, _x_i1, _x_i2)
      | Ltrywith (_x, _x_i1, _x_i2) ->
          let _x = o#lambda _x in
          let _x_i1 = o#ident _x_i1 in
          let _x_i2 = o#lambda _x_i2 in Ltrywith (_x, _x_i1, _x_i2)
      | Lifthenelse (_x, _x_i1, _x_i2) ->
          let _x = o#lambda _x in
          let _x_i1 = o#lambda _x_i1 in
          let _x_i2 = o#lambda _x_i2 in Lifthenelse (_x, _x_i1, _x_i2)
      | Lsequence (_x, _x_i1) ->
          let _x = o#lambda _x in
          let _x_i1 = o#lambda _x_i1 in Lsequence (_x, _x_i1)
      | Lwhile (_x, _x_i1) ->
          let _x = o#lambda _x in
          let _x_i1 = o#lambda _x_i1 in Lwhile (_x, _x_i1)
      | Lfor (_x, _x_i1, _x_i2, _x_i3, _x_i4) ->
          let _x = o#ident _x in
          let _x_i1 = o#lambda _x_i1 in
          let _x_i2 = o#lambda _x_i2 in
          let _x_i3 = o#unknown _x_i3 in
          let _x_i4 = o#lambda _x_i4 in Lfor (_x, _x_i1, _x_i2, _x_i3, _x_i4)
      | Lassign (_x, _x_i1) ->
          let _x = o#ident _x in
          let _x_i1 = o#lambda _x_i1 in Lassign (_x, _x_i1)
      | Lsend (_x, _x_i1, _x_i2, _x_i3, _x_i4) ->
          let _x = o#meth_kind _x in
          let _x_i1 = o#lambda _x_i1 in
          let _x_i2 = o#lambda _x_i2 in
          let _x_i3 = o#list (fun o -> o#lambda) _x_i3 in
          let _x_i4 = o#unknown _x_i4
          in Lsend (_x, _x_i1, _x_i2, _x_i3, _x_i4)
      | Levent (_x, _x_i1) ->
          let _x = o#lambda _x in
          let _x_i1 = o#lambda_event _x_i1 in Levent (_x, _x_i1)
      | Lifused (_x, _x_i1) ->
          let _x = o#ident _x in
          let _x_i1 = o#lambda _x_i1 in Lifused (_x, _x_i1)
    method ident : ident -> ident = o#unknown
    method function_kind : function_kind -> function_kind =
      function | Curried -> Curried | Tupled -> Tupled
    method compile_time_constant :
      compile_time_constant -> compile_time_constant =
      function
      | Big_endian -> Big_endian
      | Word_size -> Word_size
      | Ostype_unix -> Ostype_unix
      | Ostype_win32 -> Ostype_win32
      | Ostype_cygwin -> Ostype_cygwin
    method comparison : comparison -> comparison =
      function
      | Ceq -> Ceq
      | Cneq -> Cneq
      | Clt -> Clt
      | Cgt -> Cgt
      | Cle -> Cle
      | Cge -> Cge
    method boxed_integer : boxed_integer -> boxed_integer =
      function
      | Pnativeint -> Pnativeint
      | Pint32 -> Pint32
      | Pint64 -> Pint64
    method bigarray_layout : bigarray_layout -> bigarray_layout =
      function
      | Pbigarray_unknown_layout -> Pbigarray_unknown_layout
      | Pbigarray_c_layout -> Pbigarray_c_layout
      | Pbigarray_fortran_layout -> Pbigarray_fortran_layout
    method bigarray_kind : bigarray_kind -> bigarray_kind =
      function
      | Pbigarray_unknown -> Pbigarray_unknown
      | Pbigarray_float32 -> Pbigarray_float32
      | Pbigarray_float64 -> Pbigarray_float64
      | Pbigarray_sint8 -> Pbigarray_sint8
      | Pbigarray_uint8 -> Pbigarray_uint8
      | Pbigarray_sint16 -> Pbigarray_sint16
      | Pbigarray_uint16 -> Pbigarray_uint16
      | Pbigarray_int32 -> Pbigarray_int32
      | Pbigarray_int64 -> Pbigarray_int64
      | Pbigarray_caml_int -> Pbigarray_caml_int
      | Pbigarray_native_int -> Pbigarray_native_int
      | Pbigarray_complex32 -> Pbigarray_complex32
      | Pbigarray_complex64 -> Pbigarray_complex64
    method array_kind : array_kind -> array_kind =
      function
      | Pgenarray -> Pgenarray
      | Paddrarray -> Paddrarray
      | Pintarray -> Pintarray
      | Pfloatarray -> Pfloatarray
    method apply_status : apply_status -> apply_status =
      function | NA -> NA | Full -> Full
    method apply_info : apply_info -> apply_info =
      fun { apply_loc = _x; apply_status = _x_i1 } ->
        let _x = o#unknown _x in
        let _x_i1 = o#apply_status _x_i1
        in { apply_loc = _x; apply_status = _x_i1; }
    method unknown : 'a. 'a -> 'a = fun x -> x
  end
  

