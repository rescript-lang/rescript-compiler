(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Translation from typed abstract syntax to lambda terms,
   for the core language *)

open Misc
open Asttypes
open Primitive
open Types
open Typedtree
open Typeopt
open Lambda

type error =
    Illegal_letrec_pat
  | Illegal_letrec_expr
  | Free_super_var
  | Unknown_builtin_primitive of string

exception Error of Location.t * error

(* Forward declaration -- to be filled in by Translmod.transl_module *)
let transl_module =
  ref((fun cc rootpath modl -> assert false) :
      module_coercion -> Path.t option -> module_expr -> lambda)

let transl_object =
  ref (fun id s cl -> assert false :
       Ident.t -> string list -> class_expr -> lambda)


type specialized = {
  gencomp : Lambda.primitive;
  intcomp : Lambda.primitive;
  boolcomp : Lambda.primitive; 
  floatcomp : Lambda.primitive;
  stringcomp : Lambda.primitive;
  nativeintcomp : Lambda.primitive;
  int32comp : Lambda.primitive;
  int64comp : Lambda.primitive;
  simplify_constant_constructor : bool;
}

let more_bs_primitives ls =        
  if !Clflags.bs_only then 
      ("%bs_max",
    { gencomp = Pccall{prim_name = "caml_max"; prim_arity = 2; prim_alloc = true;
            prim_native_name = ""; prim_native_float = false};
     intcomp = Pccall{prim_name = "caml_int_max"; prim_arity = 2;
            prim_alloc = false; prim_native_name = "";
            prim_native_float = false};
     boolcomp =Pccall{prim_name = "caml_bool_max"; prim_arity = 2;
                       prim_alloc = false; prim_native_name = "";
                       prim_native_float = false}; 
     floatcomp = Pccall{prim_name = "caml_float_max"; prim_arity = 2;
            prim_alloc = false; prim_native_name = "";
            prim_native_float = false};
     stringcomp = Pccall{prim_name = "caml_string_max"; prim_arity = 2;
            prim_alloc = false; prim_native_name = "";
            prim_native_float = false};
     nativeintcomp = Pccall{prim_name = "caml_nativeint_max"; prim_arity = 2;
            prim_alloc = false; prim_native_name = "";
            prim_native_float = false};
     int32comp = Pccall{prim_name = "caml_int32_max"; prim_arity = 2;
            prim_alloc = false; prim_native_name = "";
            prim_native_float = false};
     int64comp = Pccall{prim_name = "caml_int64_max"; prim_arity = 2;
            prim_alloc = false; prim_native_name = "";
            prim_native_float = false};
     simplify_constant_constructor = false}) ::
    ("%bs_min",
    { gencomp = Pccall{prim_name = "caml_min"; prim_arity = 2; prim_alloc = true;
            prim_native_name = ""; prim_native_float = false};
     intcomp = Pccall{prim_name = "caml_int_min"; prim_arity = 2;
            prim_alloc = false; prim_native_name = "";
            prim_native_float = false};
     boolcomp = Pccall{prim_name = "caml_bool_min"; prim_arity = 2;
                       prim_alloc = false; prim_native_name = "";
                       prim_native_float = false};
     floatcomp = Pccall{prim_name = "caml_float_min"; prim_arity = 2;
            prim_alloc = false; prim_native_name = "";
            prim_native_float = false};
     stringcomp = Pccall{prim_name = "caml_string_min"; prim_arity = 2;
            prim_alloc = false; prim_native_name = "";
            prim_native_float = false};
     nativeintcomp = Pccall{prim_name = "caml_nativeint_min"; prim_arity = 2;
            prim_alloc = false; prim_native_name = "";
            prim_native_float = false};
     int32comp = Pccall{prim_name = "caml_int32_min"; prim_arity = 2;
            prim_alloc = false; prim_native_name = "";
            prim_native_float = false};
     int64comp = Pccall{prim_name = "caml_int64_min"; prim_arity = 2;
            prim_alloc = false; prim_native_name = "";
            prim_native_float = false};
     simplify_constant_constructor = false}) ::
     (
       "%bs_equal_null",
       { gencomp = Pccall{prim_name = "caml_equal_null"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false};
         intcomp = Pccall{prim_name = "caml_int_equal_null"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false};
         boolcomp = Pccall{prim_name = "caml_bool_equal_null"; prim_arity = 2; prim_alloc = true;
                          prim_native_name = ""; prim_native_float = false};       
         floatcomp = Pccall{prim_name = "caml_float_equal_null"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false};       
         stringcomp = Pccall{prim_name = "caml_string_equal_null"; prim_arity = 2;
              prim_alloc = false;
              prim_native_name = ""; prim_native_float = false};
         nativeintcomp = Pccall{prim_name = "caml_nativeint_equal_null"; prim_arity = 2;
              prim_alloc = false;
              prim_native_name = ""; prim_native_float = false};
         int32comp = Pccall{prim_name = "caml_int32_equal_null"; prim_arity = 2;
              prim_alloc = false;
              prim_native_name = ""; prim_native_float = false};       
         int64comp = Pccall{prim_name = "caml_int64_equal_null"; prim_arity = 2;
              prim_alloc = false;
              prim_native_name = ""; prim_native_float = false};       
        simplify_constant_constructor = true}
     ) :: 
     (
       "%bs_equal_undefined",
       { gencomp = Pccall{prim_name = "caml_equal_undefined"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false};
         intcomp = Pccall{prim_name = "caml_int_equal_undefined"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false};
         boolcomp = Pccall{prim_name = "caml_bool_equal_undefined"; prim_arity = 2; prim_alloc = true;
                          prim_native_name = ""; prim_native_float = false};
         floatcomp = Pccall{prim_name = "caml_float_equal_undefined"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false};       
         stringcomp = Pccall{prim_name = "caml_string_equal_undefined"; prim_arity = 2;
              prim_alloc = false;
              prim_native_name = ""; prim_native_float = false};
         nativeintcomp = Pccall{prim_name = "caml_nativeint_equal_undefined"; prim_arity = 2;
              prim_alloc = false;
              prim_native_name = ""; prim_native_float = false};
         int32comp = Pccall{prim_name = "caml_int32_equal_undefined"; prim_arity = 2;
              prim_alloc = false;
              prim_native_name = ""; prim_native_float = false};       
         int64comp = Pccall{prim_name = "caml_int64_equal_undefined"; prim_arity = 2;
              prim_alloc = false;
              prim_native_name = ""; prim_native_float = false};       
         simplify_constant_constructor = true}
     ) :: 
     (
       "%bs_equal_nullable",
       { gencomp = Pccall{prim_name = "caml_equal_nullable"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false};
         intcomp = Pccall{prim_name = "caml_int_equal_nullable"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false};
         boolcomp = Pccall{prim_name = "caml_bool_equal_nullable"; prim_arity = 2; prim_alloc = true;
                          prim_native_name = ""; prim_native_float = false};       
         floatcomp = Pccall{prim_name = "caml_float_equal_nullable"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false};       
         stringcomp = Pccall{prim_name = "caml_string_equal_nullable"; prim_arity = 2;
              prim_alloc = false;
              prim_native_name = ""; prim_native_float = false};
         nativeintcomp = Pccall{prim_name = "caml_nativeint_equal_nullable"; prim_arity = 2;
              prim_alloc = false;
              prim_native_name = ""; prim_native_float = false};
         int32comp = Pccall{prim_name = "caml_int32_equal_nullable"; prim_arity = 2;
              prim_alloc = false;
              prim_native_name = ""; prim_native_float = false};       
         int64comp = Pccall{prim_name = "caml_int64_equal_nullable"; prim_arity = 2;
              prim_alloc = false;
              prim_native_name = ""; prim_native_float = false};       
         simplify_constant_constructor = true}
     ) ::     
     ls
     else ls 

(* Translation of primitives *)
 
let comparisons_table = Lazy.from_fun @@ fun _ ->
  create_hashtable 11 @@ more_bs_primitives [
  "%equal",
      { gencomp = Pccall{prim_name = "caml_equal"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false};
        intcomp = Pintcomp Ceq;
        boolcomp = if not !Clflags.bs_only then Pintcomp Ceq
          else Pccall{prim_name = "caml_bool_equal"; prim_arity = 2;
                      prim_alloc = false;
                      prim_native_name = ""; prim_native_float = false}; 
        floatcomp = Pfloatcomp Ceq;
        stringcomp = Pccall{prim_name = "caml_string_equal"; prim_arity = 2;
              prim_alloc = false;
              prim_native_name = ""; prim_native_float = false};
        nativeintcomp = Pbintcomp(Pnativeint, Ceq);
        int32comp = Pbintcomp(Pint32, Ceq);
        int64comp = Pbintcomp(Pint64, Ceq);
        simplify_constant_constructor = true};
  "%notequal",
      { gencomp = Pccall{prim_name = "caml_notequal"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false};
        intcomp = Pintcomp Cneq;
        boolcomp = if not !Clflags.bs_only then Pintcomp Cneq
            else Pccall{prim_name = "caml_bool_notequal"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false} ; 
        floatcomp = Pfloatcomp Cneq;
        stringcomp = Pccall{prim_name = "caml_string_notequal"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false};
        nativeintcomp = Pbintcomp(Pnativeint, Cneq);
        int32comp = Pbintcomp(Pint32, Cneq);
        int64comp = Pbintcomp(Pint64, Cneq);
        simplify_constant_constructor = true};
  "%lessthan",
      { gencomp = Pccall{prim_name = "caml_lessthan"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false};
        intcomp = Pintcomp Clt;
        boolcomp = if not !Clflags.bs_only then Pintcomp Clt
          else Pccall{prim_name = "caml_bool_lessthan"; prim_arity = 2;
                      prim_alloc = false; prim_native_name = "";
                      prim_native_float = false};
        floatcomp = Pfloatcomp Clt;
        stringcomp = Pccall{prim_name = "caml_string_lessthan"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false};
        nativeintcomp = Pbintcomp(Pnativeint, Clt);
        int32comp = Pbintcomp(Pint32, Clt);
        int64comp = Pbintcomp(Pint64, Clt);
        simplify_constant_constructor = false};
  "%greaterthan",
      { gencomp = Pccall{prim_name = "caml_greaterthan"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false};
        intcomp = Pintcomp Cgt;
        boolcomp = if not !Clflags.bs_only then Pintcomp Cgt
          else Pccall{prim_name = "caml_bool_greaterthan"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false};
        floatcomp = Pfloatcomp Cgt;
        stringcomp = Pccall{prim_name = "caml_string_greaterthan"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false};
        nativeintcomp = Pbintcomp(Pnativeint, Cgt);
        int32comp = Pbintcomp(Pint32, Cgt);
        int64comp = Pbintcomp(Pint64, Cgt);
        simplify_constant_constructor = false};
  "%lessequal",
      { gencomp = Pccall{prim_name = "caml_lessequal"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false};
        intcomp = Pintcomp Cle;
        boolcomp = if not !Clflags.bs_only then Pintcomp Cle
          else Pccall{prim_name = "caml_bool_lessequal"; prim_arity = 2;
                      prim_alloc = false; prim_native_name = "";
                      prim_native_float = false};
        floatcomp = Pfloatcomp Cle;
        stringcomp = Pccall{prim_name = "caml_string_lessequal"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false};
        nativeintcomp = Pbintcomp(Pnativeint, Cle);
        int32comp = Pbintcomp(Pint32, Cle);
        int64comp = Pbintcomp(Pint64, Cle);
        simplify_constant_constructor = false};
  "%greaterequal",
      {gencomp = Pccall{prim_name = "caml_greaterequal"; prim_arity = 2;
              prim_alloc = true;
              prim_native_name = ""; prim_native_float = false};
       intcomp = Pintcomp Cge;
       boolcomp = if not !Clflags.bs_only then Pintcomp Cge
         else Pccall{prim_name = "caml_bool_greaterequal"; prim_arity = 2;
                     prim_alloc = false; prim_native_name = "";
                     prim_native_float = false};
       floatcomp = Pfloatcomp Cge;
       stringcomp = Pccall{prim_name = "caml_string_greaterequal"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false};
       nativeintcomp = Pbintcomp(Pnativeint, Cge);
       int32comp = Pbintcomp(Pint32, Cge);
       int64comp = Pbintcomp(Pint64, Cge);
       simplify_constant_constructor = false};
  "%compare",
      { gencomp = Pccall{prim_name = "caml_compare"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false};
        intcomp = Pccall{prim_name = "caml_int_compare"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false};
        boolcomp = if not !Clflags.bs_only then
            Pccall {prim_name = "caml_int_compare";
             prim_arity = 2;
             prim_alloc = false; prim_native_name = "";
             prim_native_float = false}                                               
          else
            Pccall {prim_name = "caml_bool_compare";
             prim_arity = 2;
             prim_alloc = false; prim_native_name = "";
             prim_native_float = false};
        floatcomp = Pccall{prim_name = "caml_float_compare"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false};
        stringcomp = Pccall{prim_name = "caml_string_compare"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false};
        nativeintcomp = Pccall{prim_name = "caml_nativeint_compare"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false};
        int32comp = Pccall{prim_name = "caml_int32_compare"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false};
        int64comp = Pccall{prim_name = "caml_int64_compare"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false};
       simplify_constant_constructor = false}
]

let primitives_table = create_hashtable 57 [
  "%bytes_to_string", Pbytes_to_string;
  "%bytes_of_string", Pbytes_of_string;
  "%identity", Pidentity;
  "%ignore", Pignore;
  "%field0", Pfield (0, Fld_na);
  "%field1", Pfield (1, Fld_na);
  "%setfield0", Psetfield(0, true, Fld_set_na);
  "%makeblock", Pmakeblock(0, Lambda.default_tag_info, Immutable);
  "%makemutable", Pmakeblock(0,Lambda.default_tag_info,  Mutable);
  "%raise", Praise Raise_regular;
  "%reraise", Praise Raise_reraise;
  "%raise_notrace", Praise Raise_notrace;
  "%sequand", Psequand;
  "%sequor", Psequor;
  "%boolnot", Pnot;
  "%big_endian", Pctconst Big_endian;
  "%word_size", Pctconst Word_size;
  "%ostype_unix", Pctconst Ostype_unix;
  "%ostype_win32", Pctconst Ostype_win32;
  "%ostype_cygwin", Pctconst Ostype_cygwin;
  "%negint", Pnegint;
  "%succint", Poffsetint 1;
  "%predint", Poffsetint(-1);
  "%addint", Paddint;
  "%subint", Psubint;
  "%mulint", Pmulint;
  "%divint", Pdivint;
  "%modint", Pmodint;
  "%andint", Pandint;
  "%orint", Porint;
  "%xorint", Pxorint;
  "%lslint", Plslint;
  "%lsrint", Plsrint;
  "%asrint", Pasrint;
  "%eq", Pintcomp Ceq;
  "%noteq", Pintcomp Cneq;
  "%ltint", Pintcomp Clt;
  "%leint", Pintcomp Cle;
  "%gtint", Pintcomp Cgt;
  "%geint", Pintcomp Cge;
  "%incr", Poffsetref(1);
  "%decr", Poffsetref(-1);
  "%intoffloat", Pintoffloat;
  "%floatofint", Pfloatofint;
  "%negfloat", Pnegfloat;
  "%absfloat", Pabsfloat;
  "%addfloat", Paddfloat;
  "%subfloat", Psubfloat;
  "%mulfloat", Pmulfloat;
  "%divfloat", Pdivfloat;
  "%eqfloat", Pfloatcomp Ceq;
  "%noteqfloat", Pfloatcomp Cneq;
  "%ltfloat", Pfloatcomp Clt;
  "%lefloat", Pfloatcomp Cle;
  "%gtfloat", Pfloatcomp Cgt;
  "%gefloat", Pfloatcomp Cge;
  "%string_length", Pstringlength;
  "%string_safe_get", Pstringrefs;
  "%string_safe_set", Pstringsets;
  "%string_unsafe_get", Pstringrefu;
  "%string_unsafe_set", Pstringsetu;

  "%bytes_length", Pbyteslength;
  "%bytes_safe_get", Pbytesrefs;
  "%bytes_safe_set", Pbytessets;
  "%bytes_unsafe_get", Pbytesrefu;
  "%bytes_unsafe_set", Pbytessetu;

  "%array_length", Parraylength Pgenarray;
  "%array_safe_get", Parrayrefs Pgenarray;
  "%array_safe_set", Parraysets Pgenarray;
  "%array_unsafe_get", Parrayrefu Pgenarray;
  "%array_unsafe_set", Parraysetu Pgenarray;
  "%obj_size", Parraylength Pgenarray;
  "%obj_field", Parrayrefu Pgenarray;
  "%obj_set_field", Parraysetu Pgenarray;
  "%obj_is_int", Pisint;
  "%lazy_force", Plazyforce;
  "%nativeint_of_int", Pbintofint Pnativeint;
  "%nativeint_to_int", Pintofbint Pnativeint;
  "%nativeint_neg", Pnegbint Pnativeint;
  "%nativeint_add", Paddbint Pnativeint;
  "%nativeint_sub", Psubbint Pnativeint;
  "%nativeint_mul", Pmulbint Pnativeint;
  "%nativeint_div", Pdivbint Pnativeint;
  "%nativeint_mod", Pmodbint Pnativeint;
  "%nativeint_and", Pandbint Pnativeint;
  "%nativeint_or",  Porbint Pnativeint;
  "%nativeint_xor", Pxorbint Pnativeint;
  "%nativeint_lsl", Plslbint Pnativeint;
  "%nativeint_lsr", Plsrbint Pnativeint;
  "%nativeint_asr", Pasrbint Pnativeint;
  "%int32_of_int", Pbintofint Pint32;
  "%int32_to_int", Pintofbint Pint32;
  "%int32_neg", Pnegbint Pint32;
  "%int32_add", Paddbint Pint32;
  "%int32_sub", Psubbint Pint32;
  "%int32_mul", Pmulbint Pint32;
  "%int32_div", Pdivbint Pint32;
  "%int32_mod", Pmodbint Pint32;
  "%int32_and", Pandbint Pint32;
  "%int32_or",  Porbint Pint32;
  "%int32_xor", Pxorbint Pint32;
  "%int32_lsl", Plslbint Pint32;
  "%int32_lsr", Plsrbint Pint32;
  "%int32_asr", Pasrbint Pint32;
  "%int64_of_int", Pbintofint Pint64;
  "%int64_to_int", Pintofbint Pint64;
  "%int64_neg", Pnegbint Pint64;
  "%int64_add", Paddbint Pint64;
  "%int64_sub", Psubbint Pint64;
  "%int64_mul", Pmulbint Pint64;
  "%int64_div", Pdivbint Pint64;
  "%int64_mod", Pmodbint Pint64;
  "%int64_and", Pandbint Pint64;
  "%int64_or",  Porbint Pint64;
  "%int64_xor", Pxorbint Pint64;
  "%int64_lsl", Plslbint Pint64;
  "%int64_lsr", Plsrbint Pint64;
  "%int64_asr", Pasrbint Pint64;
  "%nativeint_of_int32", Pcvtbint(Pint32, Pnativeint);
  "%nativeint_to_int32", Pcvtbint(Pnativeint, Pint32);
  "%int64_of_int32", Pcvtbint(Pint32, Pint64);
  "%int64_to_int32", Pcvtbint(Pint64, Pint32);
  "%int64_of_nativeint", Pcvtbint(Pnativeint, Pint64);
  "%int64_to_nativeint", Pcvtbint(Pint64, Pnativeint);
  "%caml_ba_ref_1",
    Pbigarrayref(false, 1, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_ref_2",
    Pbigarrayref(false, 2, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_ref_3",
    Pbigarrayref(false, 3, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_set_1",
    Pbigarrayset(false, 1, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_set_2",
    Pbigarrayset(false, 2, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_set_3",
    Pbigarrayset(false, 3, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_unsafe_ref_1",
    Pbigarrayref(true, 1, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_unsafe_ref_2",
    Pbigarrayref(true, 2, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_unsafe_ref_3",
    Pbigarrayref(true, 3, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_unsafe_set_1",
    Pbigarrayset(true, 1, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_unsafe_set_2",
    Pbigarrayset(true, 2, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_unsafe_set_3",
    Pbigarrayset(true, 3, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_dim_1", Pbigarraydim(1);
  "%caml_ba_dim_2", Pbigarraydim(2);
  "%caml_ba_dim_3", Pbigarraydim(3);
  "%caml_string_get16", Pstring_load_16(false);
  "%caml_string_get16u", Pstring_load_16(true);
  "%caml_string_get32", Pstring_load_32(false);
  "%caml_string_get32u", Pstring_load_32(true);
  "%caml_string_get64", Pstring_load_64(false);
  "%caml_string_get64u", Pstring_load_64(true);
  "%caml_string_set16", Pstring_set_16(false);
  "%caml_string_set16u", Pstring_set_16(true);
  "%caml_string_set32", Pstring_set_32(false);
  "%caml_string_set32u", Pstring_set_32(true);
  "%caml_string_set64", Pstring_set_64(false);
  "%caml_string_set64u", Pstring_set_64(true);
  "%caml_bigstring_get16", Pbigstring_load_16(false);
  "%caml_bigstring_get16u", Pbigstring_load_16(true);
  "%caml_bigstring_get32", Pbigstring_load_32(false);
  "%caml_bigstring_get32u", Pbigstring_load_32(true);
  "%caml_bigstring_get64", Pbigstring_load_64(false);
  "%caml_bigstring_get64u", Pbigstring_load_64(true);
  "%caml_bigstring_set16", Pbigstring_set_16(false);
  "%caml_bigstring_set16u", Pbigstring_set_16(true);
  "%caml_bigstring_set32", Pbigstring_set_32(false);
  "%caml_bigstring_set32u", Pbigstring_set_32(true);
  "%caml_bigstring_set64", Pbigstring_set_64(false);
  "%caml_bigstring_set64u", Pbigstring_set_64(true);
  "%bswap16", Pbswap16;
  "%bswap_int32", Pbbswap(Pint32);
  "%bswap_int64", Pbbswap(Pint64);
  "%bswap_native", Pbbswap(Pnativeint);
  "%int_as_pointer", Pint_as_pointer;
]

let prim_makearray =
  { prim_name = "caml_make_vect"; prim_arity = 2; prim_alloc = true;
    prim_native_name = ""; prim_native_float = false }

let prim_obj_dup =
  lazy { prim_name = 
    if !Clflags.bs_only then "caml_array_dup"
    else "caml_obj_dup"; 
    prim_arity = 1; prim_alloc = true;
    prim_native_name = ""; prim_native_float = false }

let find_primitive loc prim_name =
  match prim_name with
      "%revapply" -> Prevapply 
    | "%apply" -> Pdirapply 
    | "%loc_LOC" -> Ploc Loc_LOC
    | "%loc_FILE" -> Ploc Loc_FILE
    | "%loc_LINE" -> Ploc Loc_LINE
    | "%loc_POS" -> Ploc Loc_POS
    | "%loc_MODULE" -> Ploc Loc_MODULE
    | name -> Hashtbl.find primitives_table name

let transl_prim loc prim args =
  let prim_name = prim.prim_name in
  try
    let {gencomp; intcomp; boolcomp; floatcomp; stringcomp;
         nativeintcomp; int32comp; int64comp;
         simplify_constant_constructor} =
      Hashtbl.find (Lazy.force comparisons_table) prim_name in
    begin match args with
    | [arg1 ; _] when has_base_type arg1 Predef.path_bool
      -> boolcomp
    | [arg1; {exp_desc = Texp_construct(_, {cstr_tag = Cstr_constant _}, _)}]
      when simplify_constant_constructor ->
        intcomp
    | [{exp_desc = Texp_construct(_, {cstr_tag = Cstr_constant _}, _)}; arg2]
      when simplify_constant_constructor ->
        intcomp
    | [arg1; {exp_desc = Texp_variant(_, None)}]
      when simplify_constant_constructor ->
        intcomp
    | [{exp_desc = Texp_variant(_, None)}; exp2]
      when simplify_constant_constructor ->
        intcomp
    | [arg1; arg2] when has_base_type arg1 Predef.path_int
                     || has_base_type arg1 Predef.path_char
                     || not (Typeopt.maybe_pointer arg1)->
        intcomp
    | [arg1; arg2] when has_base_type arg1 Predef.path_float ->
        floatcomp
    | [arg1; arg2] when has_base_type arg1 Predef.path_string ->
        stringcomp
    | [arg1; arg2] when has_base_type arg1 Predef.path_nativeint ->
        nativeintcomp
    | [arg1; arg2] when has_base_type arg1 Predef.path_int32 ->
        int32comp
    | [arg1; arg2] when has_base_type arg1 Predef.path_int64 ->
        int64comp
    | _ ->
        gencomp
    end
  with Not_found ->
  try
    let p = find_primitive loc prim_name in
    (* Try strength reduction based on the type of the argument *)
    begin match (p, args) with
        (Psetfield(n, _, dbg_info), [arg1; arg2]) -> Psetfield(n, maybe_pointer arg2, dbg_info)
      | (Parraylength Pgenarray, [arg])   -> Parraylength(array_kind arg)
      | (Parrayrefu Pgenarray, arg1 :: _) -> Parrayrefu(array_kind arg1)
      | (Parraysetu Pgenarray, arg1 :: _) -> Parraysetu(array_kind arg1)
      | (Parrayrefs Pgenarray, arg1 :: _) -> Parrayrefs(array_kind arg1)
      | (Parraysets Pgenarray, arg1 :: _) -> Parraysets(array_kind arg1)
      | (Pbigarrayref(unsafe, n, Pbigarray_unknown, Pbigarray_unknown_layout),
                      arg1 :: _) ->
            let (k, l) = bigarray_kind_and_layout arg1 in
            Pbigarrayref(unsafe, n, k, l)
      | (Pbigarrayset(unsafe, n, Pbigarray_unknown, Pbigarray_unknown_layout),
                      arg1 :: _) ->
            let (k, l) = bigarray_kind_and_layout arg1 in
            Pbigarrayset(unsafe, n, k, l)
      | _ -> p
    end
  with Not_found ->
    if String.length prim_name > 0 && prim_name.[0] = '%' then
      raise(Error(loc, Unknown_builtin_primitive prim_name));
    Pccall prim


(* Eta-expand a primitive without knowing the types of its arguments *)

let transl_primitive loc p =
  let prim =
    try
      let {gencomp; _ } =
        Hashtbl.find (Lazy.force comparisons_table) p.prim_name in
      gencomp
    with Not_found ->
    try
      find_primitive loc p.prim_name
    with Not_found ->
      Pccall p in
  match prim with
  | Plazyforce ->
      let parm = Ident.create "prim" in
      Lfunction(Curried, [parm],
                Matching.inline_lazy_force (Lvar parm) Location.none)
  | Ploc kind ->
    let lam = lam_of_loc kind loc in
    begin match p.prim_arity with
      | 0 -> lam
      | 1 -> (* TODO: we should issue a warning ? *)
        let param = Ident.create "prim" in
        Lfunction(Curried, [param],
          Lprim(Pmakeblock(0, Lambda.default_tag_info, Immutable), [lam; Lvar param], loc))
      | _ -> assert false
    end
  | _ ->
      let rec make_params n =
        if n <= 0 then [] else Ident.create "prim" :: make_params (n-1) in
      let params = make_params p.prim_arity in
      if params = [] then  Lprim(prim,[], loc) (* arity = 0 in Buckle? TODO: unneeded *)
      else Lfunction(Curried, params,
                Lprim(prim, List.map (fun id -> Lvar id) params, loc))

(* To check the well-formedness of r.h.s. of "let rec" definitions *)

let check_recursive_lambda idlist lam =
  let rec check_top idlist = function
    | Lvar v -> not (List.mem v idlist)
    | Llet (_, _, _, _) as lam when check_recursive_recordwith idlist lam ->
        true
    | Llet(str, id, arg, body) ->
        check idlist arg && check_top (add_let id arg idlist) body
    | Lletrec(bindings, body) ->
        let idlist' = add_letrec bindings idlist in
        List.for_all (fun (id, arg) -> check idlist' arg) bindings &&
        check_top idlist' body
    | Lprim (Pmakearray (Pgenarray), args, _) -> false
    | Lsequence (lam1, lam2) -> check idlist lam1 && check_top idlist lam2
    | Levent (lam, _) -> check_top idlist lam
    | lam -> check idlist lam

  and check idlist = function
    | Lvar _ -> true
    | Lfunction(kind, params, body) -> true
    | Llet (_, _, _, _) as lam when check_recursive_recordwith idlist lam ->
        true
    | Llet(str, id, arg, body) ->
        check idlist arg && check (add_let id arg idlist) body
    | Lletrec(bindings, body) ->
        let idlist' = add_letrec bindings idlist in
        List.for_all (fun (id, arg) -> check idlist' arg) bindings &&
        check idlist' body
    | Lprim(Pmakeblock(tag, _,  mut), args, _) ->
        List.for_all (check idlist) args
    | Lprim(Pmakearray(_), args, _) ->
        List.for_all (check idlist) args
    | Lsequence (lam1, lam2) -> check idlist lam1 && check idlist lam2
    | Levent (lam, _) -> check idlist lam
    | lam ->
        let fv = free_variables lam in
        not (List.exists (fun id -> IdentSet.mem id fv) idlist)

  and add_let id arg idlist =
    let fv = free_variables arg in
    if List.exists (fun id -> IdentSet.mem id fv) idlist
    then id :: idlist
    else idlist

  and add_letrec bindings idlist =
    List.fold_right (fun (id, arg) idl -> add_let id arg idl)
                    bindings idlist

  (* reverse-engineering the code generated by transl_record case 2 *)
  (* If you change this, you probably need to change Bytegen.size_of_lambda. *)
  and check_recursive_recordwith idlist = function
    | Llet (Strict, id1, Lprim (Pduprecord _, [e1], _), body) ->
       check_top idlist e1
       && check_recordwith_updates idlist id1 body
    | _ -> false

  and check_recordwith_updates idlist id1 = function
    | Lsequence (Lprim ((Psetfield _ | Psetfloatfield _), [Lvar id2; e1], _), cont)
        -> id2 = id1 && check idlist e1
           && check_recordwith_updates idlist id1 cont
    | Lvar id2 -> id2 = id1
    | _ -> false

  in check_top idlist lam

(* To propagate structured constants *)

exception Not_constant

let extract_constant = function
    Lconst sc -> sc
  | _ -> raise_notrace Not_constant

let extract_float = function
    Const_base(Const_float f) -> f
  | _ -> fatal_error "Translcore.extract_float"

(* To find reasonable names for let-bound and lambda-bound idents *)

let rec name_pattern default = function
    [] -> Ident.create default
  | {c_lhs=p; _} :: rem ->
      match p.pat_desc with
        Tpat_var (id, _) -> id
      | Tpat_alias(p, id, _) -> id
      | _ -> name_pattern default rem

(* Push the default values under the functional abstractions *)

let rec push_defaults loc bindings cases partial =
  match cases with
    [{c_lhs=pat; c_guard=None;
      c_rhs={exp_desc = Texp_function(l, pl,partial)} as exp}] ->
      let pl = push_defaults exp.exp_loc bindings pl partial in
      [{c_lhs=pat; c_guard=None;
        c_rhs={exp with exp_desc = Texp_function(l, pl, partial)}}]
  | [{c_lhs=pat; c_guard=None;
      c_rhs={exp_attributes=[{txt="#default"},_];
             exp_desc = Texp_let
               (Nonrecursive, binds, ({exp_desc = Texp_function _} as e2))}}] ->
      push_defaults loc (binds :: bindings) [{c_lhs=pat;c_guard=None;c_rhs=e2}]
                    partial
  | [case] ->
      let exp =
        List.fold_left
          (fun exp binds ->
            {exp with exp_desc = Texp_let(Nonrecursive, binds, exp)})
          case.c_rhs bindings
      in
      [{case with c_rhs=exp}]
  | {c_lhs=pat; c_rhs=exp; c_guard=_} :: _ when bindings <> [] ->
      let param = name_pattern "param" cases in
      let name = Ident.name param in
      let exp =
        { exp with exp_loc = loc; exp_desc =
          Texp_match
            ({exp with exp_type = pat.pat_type; exp_desc =
              Texp_ident (Path.Pident param, mknoloc (Longident.Lident name),
                          {val_type = pat.pat_type; val_kind = Val_reg;
                           val_attributes = [];
                           Types.val_loc = Location.none;
                          })},
             cases, [], partial) }
      in
      push_defaults loc bindings
        [{c_lhs={pat with pat_desc = Tpat_var (param, mknoloc name)};
          c_guard=None; c_rhs=exp}]
        Total
  | _ ->
      cases

(* Insertion of debugging events *)

let event_before exp lam = match lam with
| Lstaticraise (_,_) -> lam
| _ ->
  if !Clflags.record_event_when_debug && !Clflags.debug
  then Levent(lam, {lev_loc = exp.exp_loc;
                    lev_kind = Lev_before;
                    lev_repr = None;
                    lev_env = Env.summary exp.exp_env})
  else lam

let event_after exp lam =
  if !Clflags.record_event_when_debug && !Clflags.debug
  then Levent(lam, {lev_loc = exp.exp_loc;
                    lev_kind = Lev_after exp.exp_type;
                    lev_repr = None;
                    lev_env = Env.summary exp.exp_env})
  else lam

let event_function exp lam =
  if !Clflags.record_event_when_debug && !Clflags.debug then
    let repr = Some (ref 0) in
    let (info, body) = lam repr in
    (info,
     Levent(body, {lev_loc = exp.exp_loc;
                   lev_kind = Lev_function;
                   lev_repr = repr;
                   lev_env = Env.summary exp.exp_env}))
  else
    lam None

let primitive_is_ccall = function
  (* Determine if a primitive is a Pccall or will be turned later into
     a C function call that may raise an exception *)
  | Pccall _ | Pstringrefs | Pstringsets | Parrayrefs _ | Parraysets _ |
    Pbigarrayref _ | Pbigarrayset _ | Pduprecord _ -> true
  | _ -> false

(* Assertions *)

let assert_failed exp =
  let (fname, line, char) =
    Location.get_pos_info exp.exp_loc.Location.loc_start in
#if undefined BS_NO_COMPILER_PATCH then     
  let fname = 
    if  not !Location.absname then Filename.basename fname else fname 
  in   
#end  
  Lprim(Praise Raise_regular, [event_after exp
    (Lprim(Pmakeblock(0, Lambda.default_tag_info, Immutable),
          [transl_normal_path Predef.path_assert_failure;
           Lconst(Const_block(0, Lambda.default_tag_info,
              [Const_base(Const_string (fname, None));
               Const_base(Const_int line);
               Const_base(Const_int char)]))], exp.exp_loc))], exp.exp_loc)
;;

let rec cut n l =
  if n = 0 then ([],l) else
  match l with [] -> failwith "Translcore.cut"
  | a::l -> let (l1,l2) = cut (n-1) l in (a::l1,l2)

(* Translation of expressions *)

let try_ids = Hashtbl.create 8

let rec transl_exp e =
  let eval_once =
    (* Whether classes for immediate objects must be cached *)
    match e.exp_desc with
      Texp_function _ | Texp_for _ | Texp_while _ -> false
    | _ -> true
  in
  if eval_once then transl_exp0 e else
  Translobj.oo_wrap e.exp_env true transl_exp0 e

and transl_exp0 e =
  match e.exp_desc with
    Texp_ident(path, _, {val_kind = Val_prim p}) ->
      let public_send = p.prim_name = "%send" in
      if public_send || p.prim_name = "%sendself" then
        let kind = if public_send then Public None else Self in
        let obj = Ident.create "obj" and meth = Ident.create "meth" in
        Lfunction(Curried, [obj; meth], Lsend(kind, Lvar meth, Lvar obj, [],
                                              e.exp_loc))
      else if p.prim_name = "%sendcache" then
        let obj = Ident.create "obj" and meth = Ident.create "meth" in
        let cache = Ident.create "cache" and pos = Ident.create "pos" in
        Lfunction(Curried, [obj; meth; cache; pos],
                  Lsend(Cached, Lvar meth, Lvar obj, [Lvar cache; Lvar pos],
                        e.exp_loc))
      else
        transl_primitive e.exp_loc p
  | Texp_ident(path, _, {val_kind = Val_anc _}) ->
      raise(Error(e.exp_loc, Free_super_var))
  | Texp_ident(path, _, {val_kind = Val_reg | Val_self _}) ->
      transl_path ~loc:e.exp_loc e.exp_env path
  | Texp_ident _ -> fatal_error "Translcore.transl_exp: bad Texp_ident"
  | Texp_constant cst ->
      Lconst(Const_base cst)
  | Texp_let(rec_flag, pat_expr_list, body) ->
      transl_let rec_flag pat_expr_list (event_before body (transl_exp body))
  | Texp_function (_, pat_expr_list, partial) ->
      let ((kind, params), body) =
        event_function e
          (function repr ->
            let pl = push_defaults e.exp_loc [] pat_expr_list partial in
            transl_function e.exp_loc !Clflags.native_code repr partial pl)
      in
      Lfunction(kind, params, body)
  | Texp_apply({exp_desc = Texp_ident(path, _, {val_kind = Val_prim p})},
               oargs)
    when List.length oargs >= p.prim_arity
    && List.for_all (fun (_, arg,_) -> arg <> None) oargs ->
      let args, args' = cut p.prim_arity oargs in
      let wrap f =
        if args' = []
        then event_after e f
        else event_after e (transl_apply f args' e.exp_loc)
      in
      let wrap0 f =
        if args' = [] then f else wrap f in
      let args =
         List.map (function _, Some x, _ -> x | _ -> assert false) args in
      let argl = transl_list args in
      let public_send = p.prim_name = "%send"
        || not !Clflags.native_code && p.prim_name = "%sendcache"in
      if public_send || p.prim_name = "%sendself" then
        let kind = if public_send then Public None else Self in
        let obj = List.hd argl in
        wrap (Lsend (kind, List.nth argl 1, obj, [], e.exp_loc))
      else if p.prim_name = "%sendcache" then
        match argl with [obj; meth; cache; pos] ->
          wrap (Lsend(Cached, meth, obj, [cache; pos], e.exp_loc))
        | _ -> assert false
      else begin
        let prim = transl_prim e.exp_loc p args in
        match (prim, args) with
          (Praise k, [arg1]) ->
            let targ = List.hd argl in
            let k =
              match k, targ with
              | Raise_regular, Lvar id
                when Hashtbl.mem try_ids id ->
                  Raise_reraise
              | _ ->
                  k
            in
            wrap0 (Lprim(Praise k, [event_after arg1 targ], e.exp_loc))
        | (Ploc kind, []) ->
          lam_of_loc kind e.exp_loc
        | (Ploc kind, [arg1]) ->
          let lam = lam_of_loc kind arg1.exp_loc in
          Lprim(Pmakeblock(0, Lambda.default_tag_info, Immutable), lam :: argl, e.exp_loc)
        | (Ploc _, _) -> assert false
        | (_, _) ->
            begin match (prim, argl) with
            | (Plazyforce, [a]) ->
                wrap (Matching.inline_lazy_force a e.exp_loc)
            | (Plazyforce, _) -> assert false
            |_ -> let p = Lprim(prim, argl, e.exp_loc) in
               if primitive_is_ccall prim then wrap p else wrap0 p
            end
      end
  | Texp_apply(funct, oargs) ->
      event_after e (transl_apply (transl_exp funct) oargs e.exp_loc)
  | Texp_match(arg, pat_expr_list, exn_pat_expr_list, partial) ->
    transl_match e arg pat_expr_list exn_pat_expr_list partial
  | Texp_try(body, pat_expr_list) ->
      let id = name_pattern "exn" pat_expr_list in
      Ltrywith(transl_exp body, id,
               Matching.for_trywith (Lvar id) (transl_cases_try pat_expr_list))
  | Texp_tuple el ->
      let ll = transl_list el in
      let tag_info = Lambda.Blk_tuple in 
      begin try
        Lconst(Const_block(0, tag_info, List.map extract_constant ll))
      with Not_constant ->
        Lprim(Pmakeblock(0,  tag_info, Immutable), ll, e.exp_loc)
      end
  | Texp_construct(lid, cstr, args) ->
      let ll = transl_list args in
      begin match cstr.cstr_tag with
        Cstr_constant n ->
          Lconst(Const_pointer (n,
            match lid.txt with
            | Lident ("false"|"true") -> Pt_builtin_boolean
            | _ -> (Lambda.Pt_constructor cstr.cstr_name)
            ))
      | Cstr_block n ->
          let tag_info = (Lambda.Blk_constructor (cstr.cstr_name, cstr.cstr_nonconsts)) in
          begin try
            Lconst(Const_block(n,tag_info, List.map extract_constant ll))
          with Not_constant ->
            Lprim(Pmakeblock(n, tag_info, Immutable), ll, e.exp_loc)
          end
      | Cstr_extension(path, is_const) ->
          if is_const then
            transl_path e.exp_env path
          else
            Lprim(Pmakeblock(0, Lambda.default_tag_info, Immutable),
                  transl_path e.exp_env path :: ll, e.exp_loc)
      end
  | Texp_variant(l, arg) ->
      let tag = Btype.hash_variant l in
      begin match arg with
        None -> Lconst(Const_pointer (tag, Lambda.Pt_variant l))
      | Some arg ->
          let lam = transl_exp arg in
          let tag_info = Lambda.Blk_variant l in
          try
            Lconst(Const_block(0, tag_info, [Const_base(Const_int tag);
                                   extract_constant lam]))
          with Not_constant ->
            Lprim(Pmakeblock(0, tag_info, Immutable),
                  [Lconst(Const_base(Const_int tag)); lam], e.exp_loc)
      end
  | Texp_record ((_, lbl1, _) :: _ as lbl_expr_list, opt_init_expr) ->
      if !Clflags.bs_only then
        transl_record_bs e.exp_loc lbl1.lbl_all lbl1.lbl_repres lbl_expr_list opt_init_expr
      else
        transl_record e.exp_loc lbl1.lbl_all lbl1.lbl_repres lbl_expr_list opt_init_expr
  | Texp_record ([], _) ->
      fatal_error "Translcore.transl_exp: bad Texp_record"
  | Texp_field(arg, _, lbl) ->
      let access =
        match lbl.lbl_repres with
          Record_regular -> Pfield (lbl.lbl_pos, Fld_record lbl.lbl_name)
        | Record_float -> Pfloatfield (lbl.lbl_pos, Fld_record lbl.lbl_name) in
      Lprim(access, [transl_exp arg], e.exp_loc)
  | Texp_setfield(arg, _, lbl, newval) ->
      let access =
        match lbl.lbl_repres with
          Record_regular -> Psetfield(lbl.lbl_pos, maybe_pointer newval, Fld_record_set lbl.lbl_name)
        | Record_float -> Psetfloatfield (lbl.lbl_pos, Fld_record_set lbl.lbl_name) in
      Lprim(access, [transl_exp arg; transl_exp newval], e.exp_loc)
  | Texp_array expr_list ->
      let kind = array_kind e in
      let ll = transl_list expr_list in
      begin try
        (* Deactivate constant optimization if array is small enough *)
        if List.length ll <= 4 then raise Not_constant;
        let cl = List.map extract_constant ll in
        let master =
          match kind with
          | Paddrarray | Pintarray ->
              Lconst(Const_block(0, Lambda.Blk_array, cl)) (* ATTENTION: ? [|1;2;3;4|]*)
          | Pfloatarray ->
              Lconst(Const_float_array(List.map extract_float cl))
          | Pgenarray ->
              raise Not_constant in             (* can this really happen? *)
        Lprim(Pccall (Lazy.force prim_obj_dup), [master], e.exp_loc)
      with Not_constant ->
        Lprim(Pmakearray kind, ll, e.exp_loc)
      end
  | Texp_ifthenelse(cond, ifso, Some ifnot) ->
      Lifthenelse(transl_exp cond,
                  event_before ifso (transl_exp ifso),
                  event_before ifnot (transl_exp ifnot))
  | Texp_ifthenelse(cond, ifso, None) ->
      Lifthenelse(transl_exp cond,
                  event_before ifso (transl_exp ifso),
                  lambda_unit)
  | Texp_sequence(expr1, expr2) ->
      Lsequence(transl_exp expr1, event_before expr2 (transl_exp expr2))
  | Texp_while(cond, body) ->
      Lwhile(transl_exp cond, event_before body (transl_exp body))
  | Texp_for(param, _, low, high, dir, body) ->
      Lfor(param, transl_exp low, transl_exp high, dir,
           event_before body (transl_exp body))
  | Texp_send(_, _, Some exp) -> transl_exp exp
  | Texp_send(expr, met, None) ->
      let obj = transl_exp expr in
      let lam =
        match met with
          Tmeth_val id -> Lsend (Self, Lvar id, obj, [], e.exp_loc)
        | Tmeth_name nm ->
            let (tag, cache) = Translobj.meth obj nm in
            let kind = if cache = [] then Public (Some nm) else Cached in
            Lsend (kind, tag, obj, cache, e.exp_loc)
      in
      event_after e lam
  | Texp_new (cl, {Location.loc=loc}, _) ->
      Lapply(Lprim(Pfield (0, Fld_na), [transl_path ~loc e.exp_env cl], loc),
             [lambda_unit], Location.none)
  | Texp_instvar(path_self, path, _) ->
      Lprim(Parrayrefu Paddrarray,
            [transl_normal_path path_self; transl_normal_path path], e.exp_loc)
  | Texp_setinstvar(path_self, path, _, expr) ->
      transl_setinstvar e.exp_loc (transl_normal_path path_self) path expr
  | Texp_override(path_self, modifs) ->
      let cpy = Ident.create "copy" in
      Llet(Strict, cpy,
           Lapply(Translobj.oo_prim "copy", [transl_normal_path path_self],
                  Location.none),
           List.fold_right
             (fun (path, _, expr) rem ->
                Lsequence(transl_setinstvar Location.none (Lvar cpy) path expr, rem))
             modifs
             (Lvar cpy))
  | Texp_letmodule(id, _, modl, body) ->
      Llet(Strict, id, !transl_module Tcoerce_none None modl, transl_exp body)
  | Texp_pack modl ->
      !transl_module Tcoerce_none None modl
  | Texp_assert {exp_desc=Texp_construct(_, {cstr_name="false"}, _)} ->
      if !Clflags.no_assert_false then
        Lambda.lambda_assert_false
      else 
        assert_failed e
  | Texp_assert (cond) ->
      if !Clflags.noassert
      then lambda_unit
      else Lifthenelse (transl_exp cond, lambda_unit, assert_failed e)
  | Texp_lazy e ->
      (* when e needs no computation (constants, identifiers, ...), we
         optimize the translation just as Lazy.lazy_from_val would
         do *)
      begin match e.exp_desc with
        (* a constant expr of type <> float gets compiled as itself *)
      | Texp_constant
          ( Const_int _ | Const_char _ | Const_string _
          | Const_int32 _ | Const_int64 _ | Const_nativeint _ )
      | Texp_function(_, _, _)
      | Texp_construct (_, {cstr_arity = 0}, _)
        -> transl_exp e
      | Texp_constant(Const_float _) ->
          Lprim(Pmakeblock(Obj.forward_tag, Lambda.default_tag_info, Immutable), [transl_exp e], e.exp_loc)
      | Texp_ident(_, _, _) -> (* according to the type *)
          begin match e.exp_type.desc with
          (* the following may represent a float/forward/lazy: need a
             forward_tag *)
          | Tvar _ | Tlink _ | Tsubst _ | Tunivar _
          | Tpoly(_,_) | Tfield(_,_,_,_) ->
              Lprim(Pmakeblock(Obj.forward_tag, Lambda.default_tag_info, Immutable), [transl_exp e], e.exp_loc)
          (* the following cannot be represented as float/forward/lazy:
             optimize *)
          | Tarrow(_,_,_,_) | Ttuple _ | Tpackage _ | Tobject(_,_) | Tnil
          | Tvariant _
              -> transl_exp e
          (* optimize predefined types (excepted float) *)
          | Tconstr(_,_,_) ->
              if has_base_type e Predef.path_int
                || has_base_type e Predef.path_char
                || has_base_type e Predef.path_string
                || has_base_type e Predef.path_bool
                || has_base_type e Predef.path_unit
                || has_base_type e Predef.path_exn
                || has_base_type e Predef.path_array
                || has_base_type e Predef.path_list
                || has_base_type e Predef.path_option
                || has_base_type e Predef.path_nativeint
                || has_base_type e Predef.path_int32
                || has_base_type e Predef.path_int64
              then transl_exp e
              else
                Lprim(Pmakeblock(Obj.forward_tag, Lambda.default_tag_info, Immutable), [transl_exp e], e.exp_loc)
          end
      (* other cases compile to a lazy block holding a function *)
      | _ ->
          let fn = Lfunction (Curried, [Ident.create "param"], transl_exp e) in
          Lprim(Pmakeblock(Config.lazy_tag, Lambda.default_tag_info, Mutable), [fn], e.exp_loc)
      end
  | Texp_object (cs, meths) ->
      let cty = cs.cstr_type in
      let cl = Ident.create "class" in
      !transl_object cl meths
        { cl_desc = Tcl_structure cs;
          cl_loc = e.exp_loc;
          cl_type = Cty_signature cty;
          cl_env = e.exp_env;
          cl_attributes = [];
         }

and transl_list expr_list =
  List.map transl_exp expr_list

and transl_guard guard rhs =
  let expr = event_before rhs (transl_exp rhs) in
  match guard with
  | None -> expr
  | Some cond ->
      event_before cond (Lifthenelse(transl_exp cond, expr, staticfail))

and transl_case {c_lhs; c_guard; c_rhs} =
  c_lhs, transl_guard c_guard c_rhs

and transl_cases cases =
  List.map transl_case cases

and transl_case_try {c_lhs; c_guard; c_rhs} =
  match c_lhs.pat_desc with
  | Tpat_var (id, _)
  | Tpat_alias (_, id, _) ->
      Hashtbl.replace try_ids id ();
      Misc.try_finally
        (fun () -> c_lhs, transl_guard c_guard c_rhs)
        (fun () -> Hashtbl.remove try_ids id)
  | _ ->
      c_lhs, transl_guard c_guard c_rhs

and transl_cases_try cases =
  List.map transl_case_try cases

and transl_tupled_cases patl_expr_list =
  List.map (fun (patl, guard, expr) -> (patl, transl_guard guard expr))
    patl_expr_list

and transl_apply lam sargs loc =
  let lapply funct args =
    match funct with
      Lsend(k, lmet, lobj, largs, loc) ->
        Lsend(k, lmet, lobj, largs @ args, loc)
    | Levent(Lsend(k, lmet, lobj, largs, loc), _) ->
        Lsend(k, lmet, lobj, largs @ args, loc)
    | Lapply(lexp, largs, _) ->
        Lapply(lexp, largs @ args, loc)
    | lexp ->
        Lapply(lexp, args, loc)
  in
  let rec build_apply lam args = function
      (None, optional) :: l ->
        let defs = ref [] in
        let protect name lam =
          match lam with
            Lvar _ | Lconst _ -> lam
          | _ ->
              let id = Ident.create name in
              defs := (id, lam) :: !defs;
              Lvar id
        in
        let args, args' =
          if List.for_all (fun (_,opt) -> opt = Optional) args then [], args
          else args, [] in
        let lam =
          if args = [] then lam else lapply lam (List.rev_map fst args) in
        let handle = protect "func" lam
        and l = List.map (fun (arg, opt) -> may_map (protect "arg") arg, opt) l
        and id_arg = Ident.create "param" in
        let body =
          match build_apply handle ((Lvar id_arg, optional)::args') l with
            Lfunction(Curried, ids, lam) ->
              Lfunction(Curried, id_arg::ids, lam)
          | Levent(Lfunction(Curried, ids, lam), _) ->
              Lfunction(Curried, id_arg::ids, lam)
          | lam ->
              Lfunction(Curried, [id_arg], lam)
        in
        List.fold_left
          (fun body (id, lam) -> Llet(Strict, id, lam, body))
          body !defs
    | (Some arg, optional) :: l ->
        build_apply lam ((arg, optional) :: args) l
    | [] ->
        lapply lam (List.rev_map fst args)
  in
  build_apply lam [] (List.map (fun (l, x,o) -> may_map transl_exp x, o) sargs)

and transl_function loc untuplify_fn repr partial cases =
  match cases with
    [{c_lhs=pat; c_guard=None;
      c_rhs={exp_desc = Texp_function(_, pl,partial')} as exp}]
    when Parmatch.fluid pat ->
      let param = name_pattern "param" cases in
      let ((_, params), body) =
        transl_function exp.exp_loc false repr partial' pl in
      ((Curried, param :: params),
       Matching.for_function loc None (Lvar param) [pat, body] partial)
  | {c_lhs={pat_desc = Tpat_tuple pl}} :: _ when untuplify_fn ->
      begin try
        let size = List.length pl in
        let pats_expr_list =
          List.map
            (fun {c_lhs; c_guard; c_rhs} ->
              (Matching.flatten_pattern size c_lhs, c_guard, c_rhs))
            cases in
        let params = List.map (fun p -> Ident.create "param") pl in
        ((Tupled, params),
         Matching.for_tupled_function loc params
           (transl_tupled_cases pats_expr_list) partial)
      with Matching.Cannot_flatten ->
        let param = name_pattern "param" cases in
        ((Curried, [param]),
         Matching.for_function loc repr (Lvar param)
           (transl_cases cases) partial)
      end
  | _ ->
      let param = name_pattern "param" cases in
      ((Curried, [param]),
       Matching.for_function loc repr (Lvar param)
         (transl_cases cases) partial)

and transl_let rec_flag pat_expr_list body =
  match rec_flag with
    Nonrecursive ->
      let rec transl = function
        [] ->
          body
      | {vb_pat=pat; vb_expr=expr} :: rem ->
          Matching.for_let pat.pat_loc (transl_exp expr) pat (transl rem)
      in transl pat_expr_list
  | Recursive ->
      let idlist =
        List.map
          (fun {vb_pat=pat} -> match pat.pat_desc with
              Tpat_var (id,_) -> id
            | Tpat_alias ({pat_desc=Tpat_any}, id,_) -> id
            | _ -> raise(Error(pat.pat_loc, Illegal_letrec_pat)))
        pat_expr_list in
      let transl_case {vb_pat=pat; vb_expr=expr} id =
        let lam = transl_exp expr in
        if not (check_recursive_lambda idlist lam) then
          raise(Error(expr.exp_loc, Illegal_letrec_expr));
        (id, lam) in
      Lletrec(List.map2 transl_case pat_expr_list idlist, body)

and transl_setinstvar loc self var expr =
  Lprim(Parraysetu (if maybe_pointer expr then Paddrarray else Pintarray),
                    [self; transl_normal_path var; transl_exp expr], loc)

and transl_record loc all_labels repres lbl_expr_list opt_init_expr =
  let size = Array.length all_labels in
  (* Determine if there are "enough" new fields *)
  if 3 + 2 * List.length lbl_expr_list >= size
  then begin
    (* Allocate new record with given fields (and remaining fields
       taken from init_expr if any *)
    let lv = Array.make (Array.length all_labels) staticfail in
    let init_id = Ident.create "init" in
    begin match opt_init_expr with
      None -> ()
    | Some init_expr ->
        for i = 0 to Array.length all_labels - 1 do
          let access =
            let lbl = all_labels.(i) in
            match lbl.lbl_repres with
              Record_regular -> Pfield (i, Fld_record lbl.lbl_name)
            | Record_float -> Pfloatfield (i, Fld_record lbl.lbl_name)  in
          lv.(i) <- Lprim(access, [Lvar init_id], loc)
        done
    end;
    List.iter
      (fun (_, lbl, expr) -> lv.(lbl.lbl_pos) <- transl_exp expr)
      lbl_expr_list;
    let ll = Array.to_list lv in
    let mut =
      if List.exists (fun (_, lbl, expr) -> lbl.lbl_mut = Mutable) lbl_expr_list
      then Mutable
      else Immutable in
    let all_labels_info = all_labels |> Array.map (fun x -> x.Types.lbl_name) in
    let lam =
      try
        if mut = Mutable then raise Not_constant;
        let cl = List.map extract_constant ll in
        match repres with
          Record_regular -> Lconst(Const_block(0, Lambda.Blk_record all_labels_info, cl))
        | Record_float ->
            Lconst(Const_float_array(List.map extract_float cl))
      with Not_constant ->
        match repres with
          Record_regular -> Lprim(Pmakeblock(0, Lambda.Blk_record all_labels_info, mut), ll,loc)
        | Record_float -> Lprim(Pmakearray Pfloatarray, ll, loc) in
    begin match opt_init_expr with
      None -> lam
    | Some init_expr -> Llet(Strict, init_id, transl_exp init_expr, lam)
    end
  end else begin
    (* Take a shallow copy of the init record, then mutate the fields
       of the copy *)
    (* If you change anything here, you will likely have to change
       [check_recursive_recordwith] in this file. *)
    let copy_id = Ident.create "newrecord" in
    let update_field (_, lbl, expr) cont =
      let upd =
        match lbl.lbl_repres with
          Record_regular -> Psetfield(lbl.lbl_pos, maybe_pointer expr, Fld_record_set lbl.lbl_name)
        | Record_float -> Psetfloatfield (lbl.lbl_pos, Fld_record_set lbl.lbl_name) in
      Lsequence(Lprim(upd, [Lvar copy_id; transl_exp expr], loc), cont) in
    begin match opt_init_expr with
      None -> assert false
    | Some init_expr ->
        Llet(Strict, copy_id,
             Lprim(Pduprecord (repres, size), [transl_exp init_expr], loc),
             List.fold_right update_field lbl_expr_list (Lvar copy_id))
    end
  end

and transl_record_bs loc all_labels repres lbl_expr_list opt_init_expr =
  let size = Array.length all_labels in
  (* Determine if there are "enough" new fields *)
  if opt_init_expr = None || size <= 20 || 3 + 2 * List.length lbl_expr_list >= size
  then begin
    (* Allocate new record with given fields (and remaining fields
       taken from init_expr if any *)
    let lv = Array.make size staticfail in
    let init_id = Ident.create "init" in
    for i = 0 to Array.length all_labels - 1 do
      let access =
        let lbl = all_labels.(i) in
         Pfield (i, Fld_record lbl.lbl_name)
      in
      lv.(i) <- Lprim(access, [Lvar init_id], loc)
    done;
    List.iter
      (fun (_, lbl, expr) -> lv.(lbl.lbl_pos) <- transl_exp expr)
      lbl_expr_list;
    let ll = Array.to_list lv in
    let mut = ref Immutable in
    let all_labels_info =
      Lambda.Blk_record (all_labels |> Array.map (fun x -> x.Types.lbl_name)) in
    let lam =
      try
        for i = 0 to Array.length all_labels - 1 do
          if (Array.unsafe_get all_labels i).lbl_mut = Mutable then
            begin
              mut := Mutable;
              raise_notrace Not_constant
            end
        done ;
        let cl = List.map extract_constant ll in
        Lconst(Const_block(0, all_labels_info, cl))        
      with Not_constant ->
        Lprim(Pmakeblock(0, all_labels_info, !mut), ll,loc)
    in
    begin match opt_init_expr with
      None -> lam
    | Some init_expr -> Llet(Strict, init_id, transl_exp init_expr, lam)
    end
  end else begin
    (* Take a shallow copy of the init record, then mutate the fields
       of the copy *)
    (* If you change anything here, you will likely have to change
       [check_recursive_recordwith] in this file. *)
    let copy_id = Ident.create "newrecord" in
    let update_field (_, ({lbl_pos; lbl_name} : Types.label_description), expr) cont =
      let upd =
        Psetfield(lbl_pos, true, Fld_record_set lbl_name)
        (* don't care pointer or not *)
      in
      Lsequence(Lprim(upd, [Lvar copy_id; transl_exp expr], loc), cont) in
    begin match opt_init_expr with
      None -> assert false
    | Some init_expr ->
        Llet(Strict, copy_id,
             Lprim(Pduprecord (repres, size), [transl_exp init_expr], loc),
             List.fold_right update_field lbl_expr_list (Lvar copy_id))
    end
  end

and transl_match e arg pat_expr_list exn_pat_expr_list partial =
  let id = name_pattern "exn" exn_pat_expr_list
  and cases = transl_cases pat_expr_list
  and exn_cases = transl_cases exn_pat_expr_list in
  let static_catch body val_ids handler =
    let static_exception_id = next_negative_raise_count () in
    Lstaticcatch
      (Ltrywith (Lstaticraise (static_exception_id, body), id,
                 Matching.for_trywith (Lvar id) exn_cases),
       (static_exception_id, val_ids),
       handler)
  in
  match arg, exn_cases with
  | {exp_desc = Texp_tuple argl}, [] ->
    Matching.for_multiple_match e.exp_loc (transl_list argl) cases partial
  | {exp_desc = Texp_tuple argl}, _ :: _ ->
    let val_ids = List.map (fun _ -> name_pattern "val" []) argl in
    let lvars = List.map (fun id -> Lvar id) val_ids in
    static_catch (transl_list argl) val_ids
      (Matching.for_multiple_match e.exp_loc lvars cases partial)
  | arg, [] ->
    Matching.for_function e.exp_loc None (transl_exp arg) cases partial
  | arg, _ :: _ ->
    let val_id = name_pattern "val" pat_expr_list in
    static_catch [transl_exp arg] [val_id]
      (Matching.for_function e.exp_loc None (Lvar val_id) cases partial)


(* Wrapper for class compilation *)

(*
let transl_exp = transl_exp_wrap

let transl_let rec_flag pat_expr_list body =
  match pat_expr_list with
    [] -> body
  | (_, expr) :: _ ->
      Translobj.oo_wrap expr.exp_env false
        (transl_let rec_flag pat_expr_list) body
*)

(* Error report *)

open Format

let report_error ppf = function
  | Illegal_letrec_pat ->
      fprintf ppf
        "Only variables are allowed as left-hand side of `let rec'"
  | Illegal_letrec_expr ->
      fprintf ppf
        "This kind of expression is not allowed as right-hand side of `let rec'"
  | Free_super_var ->
      fprintf ppf
        "Ancestor names can only be used to select inherited methods"
  | Unknown_builtin_primitive prim_name ->
    fprintf ppf  "Unknown builtin primitive \"%s\"" prim_name

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
          Some (Location.error_of_printer loc report_error err)
      | _ ->
        None
    )
