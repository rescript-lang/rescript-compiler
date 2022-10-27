(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Format
open Asttypes
open Primitive
open Lambda


let rec struct_const ppf = function
  | Const_base(Const_int n) -> fprintf ppf "%i" n
  | Const_base(Const_char i) -> fprintf ppf "%s" (Pprintast.string_of_int_as_char i)
  | Const_base(Const_string (s, _)) -> fprintf ppf "%S" s
  | Const_immstring s -> fprintf ppf "#%S" s
  | Const_base(Const_float f) -> fprintf ppf "%s" f
  | Const_base(Const_int32 n) -> fprintf ppf "%lil" n
  | Const_base(Const_int64 n) -> fprintf ppf "%LiL" n
  | Const_base(Const_nativeint n) -> fprintf ppf "%nin" n
  | Const_pointer (n,_) -> fprintf ppf "%ia" n
  | Const_block(tag_info, []) ->
      let tag = Lambda.tag_of_tag_info tag_info in 
      fprintf ppf "[%i]" tag
  | Const_block(tag_info,sc1::scl) ->
      let tag = Lambda.tag_of_tag_info tag_info in 
      let sconsts ppf scl =
        List.iter (fun sc -> fprintf ppf "@ %a" struct_const sc) scl in
      fprintf ppf "@[<1>[%i:@ @[%a%a@]]@]" tag struct_const sc1 sconsts scl
  | Const_float_array [] ->
      fprintf ppf "[| |]"
  | Const_float_array (f1 :: fl) ->
      let floats ppf fl =
        List.iter (fun f -> fprintf ppf "@ %s" f) fl in
      fprintf ppf "@[<1>[|@[%s%a@]|]@]" f1 floats fl

  | Const_false -> fprintf ppf "false"      
  | Const_true -> fprintf ppf "true"
let boxed_integer_name = function
  | Pnativeint -> "nativeint"
  | Pint32 -> "int32"
  | Pint64 -> "int64"

let value_kind = function
  | Pgenval -> ""

(* let field_kind = function
  | Pgenval -> "*"
  | Pintval -> "int"
  | Pfloatval -> "float"
  | Pboxedintval bi -> boxed_integer_name bi *)

let print_boxed_integer_conversion ppf bi1 bi2 =
  fprintf ppf "%s_of_%s" (boxed_integer_name bi2) (boxed_integer_name bi1)

let boxed_integer_mark name = function
  | Pnativeint -> Printf.sprintf "Nativeint.%s" name
  | Pint32 -> Printf.sprintf "Int32.%s" name
  | Pint64 -> Printf.sprintf "Int64.%s" name

let print_boxed_integer name ppf bi =
  fprintf ppf "%s" (boxed_integer_mark name bi);;




let string_of_loc_kind = function
  | Loc_FILE -> "loc_FILE"
  | Loc_LINE -> "loc_LINE"
  | Loc_MODULE -> "loc_MODULE"
  | Loc_POS -> "loc_POS"
  | Loc_LOC -> "loc_LOC"

(* let block_shape ppf shape = match shape with
  | None | Some [] -> ()
  | Some l when List.for_all ((=) Pgenval) l -> ()
  | Some [elt] ->
      Format.fprintf ppf " (%s)" (field_kind elt)
  | Some (h :: t) ->
      Format.fprintf ppf " (%s" (field_kind h);
      List.iter (fun elt ->
          Format.fprintf ppf ",%s" (field_kind elt))
        t;
      Format.fprintf ppf ")" *)


let str_of_field_info (fld_info : Lambda.field_dbg_info)= 
  match fld_info with 
  | (Fld_module {name } | Fld_record {name} | Fld_record_inline {name} | Fld_record_extension {name})
    -> name
  | Fld_tuple -> "[]"
  | Fld_poly_var_tag->"`"
  | Fld_poly_var_content -> "#"
  | Fld_extension -> "ext"
  | Fld_variant -> "var"
  | Fld_cons -> "cons"
  | Fld_array -> "[||]" 
let print_taginfo ppf = function
  | Blk_extension -> fprintf ppf "ext" 
  | Blk_record_ext {fields = ss} -> fprintf ppf "[%s]" (String.concat ";" (Array.to_list ss) )
  | Blk_tuple -> fprintf ppf "tuple"
  | Blk_constructor {name ;num_nonconst} -> fprintf ppf "%s/%i" name num_nonconst
  | Blk_poly_var name -> fprintf ppf "`%s" name 
  | Blk_record  {fields = ss} ->  fprintf ppf "[%s]" (String.concat ";" (Array.to_list ss) )
  | Blk_module ss ->  fprintf ppf "[%s]" (String.concat ";"  ss) 
  | Blk_some -> fprintf ppf "some"
  | Blk_some_not_nested -> fprintf ppf "some_not_nested" 
  | Blk_lazy_general -> fprintf ppf "lazy_general"
  | Blk_module_export _ -> fprintf ppf "module/exports"
  | Blk_record_inlined {fields = ss }
    -> fprintf ppf "[%s]" (String.concat ";" (Array.to_list ss) )

let primitive ppf = function
  | Puncurried_apply -> fprintf ppf "@app"
  | Pidentity -> fprintf ppf "id"
  | Pbytes_to_string -> fprintf ppf "bytes_to_string"
  | Pignore -> fprintf ppf "ignore"
  | Prevapply -> fprintf ppf "revapply"
  | Pdirapply -> fprintf ppf "dirapply"
  | Ploc kind -> fprintf ppf "%s" (string_of_loc_kind kind)
  | Pgetglobal id -> fprintf ppf "global %a" Ident.print id
  | Pmakeblock(taginfo) ->
      fprintf ppf "makeblock %a" print_taginfo taginfo
  | Pfield (n, fld) -> fprintf ppf "field:%s/%i" (str_of_field_info fld) n      
  | Psetfield(n,  _) ->
      fprintf ppf "setfield %i"   n
  | Pduprecord -> fprintf ppf "duprecord"
  | Plazyforce -> fprintf ppf "force"
  | Pccall p -> fprintf ppf "%s" p.prim_name
  | Praise k -> fprintf ppf "%s" (Lambda.raise_kind k)
  | Psequand -> fprintf ppf "&&"
  | Psequor -> fprintf ppf "||"
  | Pnot -> fprintf ppf "not"
  | Pnegint -> fprintf ppf "~"
  | Paddint -> fprintf ppf "+"
  | Psubint -> fprintf ppf "-"
  | Pmulint -> fprintf ppf "*"
  | Pdivint Safe -> fprintf ppf "/"
  | Pdivint Unsafe -> fprintf ppf "/u"
  | Pmodint Safe -> fprintf ppf "mod"
  | Pmodint Unsafe -> fprintf ppf "mod_unsafe"
  | Pandint -> fprintf ppf "and"
  | Porint -> fprintf ppf "or"
  | Pxorint -> fprintf ppf "xor"
  | Plslint -> fprintf ppf "lsl"
  | Plsrint -> fprintf ppf "lsr"
  | Pasrint -> fprintf ppf "asr"
  | Pintcomp(Ceq) -> fprintf ppf "=="
  | Pintcomp(Cneq) -> fprintf ppf "!="
  | Pintcomp(Clt) -> fprintf ppf "<"
  | Pintcomp(Cle) -> fprintf ppf "<="
  | Pintcomp(Cgt) -> fprintf ppf ">"
  | Pintcomp(Cge) -> fprintf ppf ">="
  | Poffsetint n -> fprintf ppf "%i+" n
  | Poffsetref n -> fprintf ppf "+:=%i"n
  | Pintoffloat -> fprintf ppf "int_of_float"
  | Pfloatofint -> fprintf ppf "float_of_int"
  | Pnegfloat -> fprintf ppf "~."
  | Pabsfloat -> fprintf ppf "abs."
  | Paddfloat -> fprintf ppf "+."
  | Psubfloat -> fprintf ppf "-."
  | Pmulfloat -> fprintf ppf "*."
  | Pdivfloat -> fprintf ppf "/."
  | Pfloatcomp(Ceq) -> fprintf ppf "==."
  | Pfloatcomp(Cneq) -> fprintf ppf "!=."
  | Pfloatcomp(Clt) -> fprintf ppf "<."
  | Pfloatcomp(Cle) -> fprintf ppf "<=."
  | Pfloatcomp(Cgt) -> fprintf ppf ">."
  | Pfloatcomp(Cge) -> fprintf ppf ">=."
  | Pstringlength -> fprintf ppf "string.length"
  | Pstringrefu -> fprintf ppf "string.unsafe_get"
  | Pstringrefs -> fprintf ppf "string.get"
  | Pbyteslength -> fprintf ppf "bytes.length"
  | Pbytesrefu -> fprintf ppf "bytes.unsafe_get"
  | Pbytessetu -> fprintf ppf "bytes.unsafe_set"
  | Pbytesrefs -> fprintf ppf "bytes.get"
  | Pbytessets -> fprintf ppf "bytes.set"

  | Parraylength  -> fprintf ppf "array.length" 
  | Pmakearray Mutable -> fprintf ppf "makearray" 
  | Pmakearray Immutable -> fprintf ppf "makearray_imm" 
  | Parrayrefu -> fprintf ppf "array.unsafe_get" 
  | Parraysetu -> fprintf ppf "array.unsafe_set" 
  | Parrayrefs -> fprintf ppf "array.get" 
  | Parraysets -> fprintf ppf "array.set" 
  | Pctconst c ->
     let const_name = match c with
       | Big_endian -> "big_endian"
       | Word_size -> "word_size"
       | Int_size -> "int_size"
       | Max_wosize -> "max_wosize"
       | Ostype_unix -> "ostype_unix"
       | Ostype_win32 -> "ostype_win32"
       | Ostype_cygwin -> "ostype_cygwin"
       | Backend_type -> "backend_type" in
     fprintf ppf "sys.constant_%s" const_name
  | Pisint -> fprintf ppf "isint"
  | Pisout -> fprintf ppf "isout"
  | Pbintofint bi -> print_boxed_integer "of_int" ppf bi
  | Pintofbint bi -> print_boxed_integer "to_int" ppf bi
  | Pcvtbint (bi1, bi2) -> print_boxed_integer_conversion ppf bi1 bi2
  | Pnegbint bi -> print_boxed_integer "neg" ppf bi
  | Paddbint bi -> print_boxed_integer "add" ppf bi
  | Psubbint bi -> print_boxed_integer "sub" ppf bi
  | Pmulbint bi -> print_boxed_integer "mul" ppf bi
  | Pdivbint { size = bi; is_safe = Safe } ->
      print_boxed_integer "div" ppf bi
  | Pdivbint { size = bi; is_safe = Unsafe } ->
      print_boxed_integer "div_unsafe" ppf bi
  | Pmodbint { size = bi; is_safe = Safe } ->
      print_boxed_integer "mod" ppf bi
  | Pmodbint { size = bi; is_safe = Unsafe } ->
      print_boxed_integer "mod_unsafe" ppf bi
  | Pandbint bi -> print_boxed_integer "and" ppf bi
  | Porbint bi -> print_boxed_integer "or" ppf bi
  | Pxorbint bi -> print_boxed_integer "xor" ppf bi
  | Plslbint bi -> print_boxed_integer "lsl" ppf bi
  | Plsrbint bi -> print_boxed_integer "lsr" ppf bi
  | Pasrbint bi -> print_boxed_integer "asr" ppf bi
  | Pbintcomp(bi, Ceq) -> print_boxed_integer "==" ppf bi
  | Pbintcomp(bi, Cneq) -> print_boxed_integer "!=" ppf bi
  | Pbintcomp(bi, Clt) -> print_boxed_integer "<" ppf bi
  | Pbintcomp(bi, Cgt) -> print_boxed_integer ">" ppf bi
  | Pbintcomp(bi, Cle) -> print_boxed_integer "<=" ppf bi
  | Pbintcomp(bi, Cge) -> print_boxed_integer ">=" ppf bi
  | Popaque -> fprintf ppf "opaque"
  | Pcreate_extension s -> fprintf ppf "extension[%s]" s   
let name_of_primitive = function
  | Puncurried_apply -> "Puncurried_apply"
  | Pidentity -> "Pidentity"
  | Pbytes_to_string -> "Pbytes_to_string"
  | Pignore -> "Pignore"
  | Prevapply -> "Prevapply"
  | Pdirapply -> "Pdirapply"
  | Ploc _ -> "Ploc"
  | Pgetglobal _ -> "Pgetglobal"
  | Pmakeblock _ -> "Pmakeblock"
  | Pfield _ -> "Pfield"
  | Psetfield _ -> "Psetfield"
  | Pduprecord  -> "Pduprecord"
  | Plazyforce -> "Plazyforce"
  | Pccall _ -> "Pccall"
  | Praise _ -> "Praise"
  | Psequand -> "Psequand"
  | Psequor -> "Psequor"
  | Pnot -> "Pnot"
  | Pnegint -> "Pnegint"
  | Paddint -> "Paddint"
  | Psubint -> "Psubint"
  | Pmulint -> "Pmulint"
  | Pdivint _ -> "Pdivint"
  | Pmodint _ -> "Pmodint"
  | Pandint -> "Pandint"
  | Porint -> "Porint"
  | Pxorint -> "Pxorint"
  | Plslint -> "Plslint"
  | Plsrint -> "Plsrint"
  | Pasrint -> "Pasrint"
  | Pintcomp _ -> "Pintcomp"
  | Poffsetint _ -> "Poffsetint"
  | Poffsetref _ -> "Poffsetref"
  | Pintoffloat -> "Pintoffloat"
  | Pfloatofint -> "Pfloatofint"
  | Pnegfloat -> "Pnegfloat"
  | Pabsfloat -> "Pabsfloat"
  | Paddfloat -> "Paddfloat"
  | Psubfloat -> "Psubfloat"
  | Pmulfloat -> "Pmulfloat"
  | Pdivfloat -> "Pdivfloat"
  | Pfloatcomp _ -> "Pfloatcomp"
  | Pstringlength -> "Pstringlength"
  | Pstringrefu -> "Pstringrefu"
  | Pstringrefs -> "Pstringrefs"
  | Pbyteslength -> "Pbyteslength"
  | Pbytesrefu -> "Pbytesrefu"
  | Pbytessetu -> "Pbytessetu"
  | Pbytesrefs -> "Pbytesrefs"
  | Pbytessets -> "Pbytessets"
  | Parraylength -> "Parraylength"
  | Pmakearray _-> "Pmakearray"
  | Parrayrefu -> "Parrayrefu"
  | Parraysetu -> "Parraysetu"
  | Parrayrefs -> "Parrayrefs"
  | Parraysets -> "Parraysets"
  | Pctconst _ -> "Pctconst"
  | Pisint -> "Pisint"
  | Pisout -> "Pisout"
  | Pbintofint _ -> "Pbintofint"
  | Pintofbint _ -> "Pintofbint"
  | Pcvtbint _ -> "Pcvtbint"
  | Pnegbint _ -> "Pnegbint"
  | Paddbint _ -> "Paddbint"
  | Psubbint _ -> "Psubbint"
  | Pmulbint _ -> "Pmulbint"
  | Pdivbint _ -> "Pdivbint"
  | Pmodbint _ -> "Pmodbint"
  | Pandbint _ -> "Pandbint"
  | Porbint _ -> "Porbint"
  | Pxorbint _ -> "Pxorbint"
  | Plslbint _ -> "Plslbint"
  | Plsrbint _ -> "Plsrbint"
  | Pasrbint _ -> "Pasrbint"
  | Pbintcomp _ -> "Pbintcomp"
  | Popaque -> "Popaque"
  | Pcreate_extension _ -> "Pcreate_extension"

let function_attribute ppf { inline; is_a_functor; stub; return_unit } =
  if is_a_functor then
    fprintf ppf "is_a_functor@ ";
  if stub then
    fprintf ppf "stub@ ";
  if return_unit then 
    fprintf ppf "void@ ";  
  begin match inline with
  | Default_inline -> ()
  | Always_inline -> fprintf ppf "always_inline@ "
  | Never_inline -> fprintf ppf "never_inline@ "
  end


let apply_inlined_attribute ppf = function
  | Default_inline -> ()
  | Always_inline -> fprintf ppf " always_inline"
  | Never_inline -> fprintf ppf " never_inline"


let rec lam ppf = function
  | Lvar id ->
      Ident.print ppf id
  | Lconst cst ->
      struct_const ppf cst
  | Lapply ap ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(apply@ %a%a%a)@]" lam ap.ap_func lams ap.ap_args
        apply_inlined_attribute ap.ap_inlined

  | Lfunction{ params; body; attr} ->
      let pr_params ppf params =
            List.iter (fun param -> fprintf ppf "@ %a" Ident.print param) params
      in
      fprintf ppf "@[<2>(function%a@ %a%a)@]" pr_params params
        function_attribute attr lam body
  | Llet(str, k, id, arg, body) ->
      let kind = function
          Alias -> "a" | Strict -> "" | StrictOpt -> "o" | Variable -> "v"
      in
      let rec letbody = function
        | Llet(str, k, id, arg, body) ->
            fprintf ppf "@ @[<2>%a =%s%s@ %a@]"
              Ident.print id (kind str) (value_kind k) lam arg;
            letbody body
        | expr -> expr in
      fprintf ppf "@[<2>(let@ @[<hv 1>(@[<2>%a =%s%s@ %a@]"
        Ident.print id (kind str) (value_kind k) lam arg;
      let expr = letbody body in
      fprintf ppf ")@]@ %a)@]" lam expr
  | Lletrec(id_arg_list, body) ->
      let bindings ppf id_arg_list =
        let spc = ref false in
        List.iter
          (fun (id, l) ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<2>%a@ %a@]" Ident.print id lam l)
          id_arg_list in
      fprintf ppf
        "@[<2>(letrec@ (@[<hv 1>%a@])@ %a)@]" bindings id_arg_list lam body
  | Lprim(prim, largs, _) ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(%a%a)@]" primitive prim lams largs
  | Lswitch(larg, sw, _loc) ->
      let switch ppf sw =
        let spc = ref false in
        List.iter
         (fun (n, l) ->
           if !spc then fprintf ppf "@ " else spc := true;
           fprintf ppf "@[<hv 1>case int %i:@ %a@]" n lam l)
         sw.sw_consts;
        List.iter
          (fun (n, l) ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>case tag %i:@ %a@]" n lam l)
          sw.sw_blocks ;
        begin match sw.sw_failaction with
        | None  -> ()
        | Some l ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>default:@ %a@]" lam l
        end in
      fprintf ppf
       "@[<1>(%s %a@ @[<v 0>%a@])@]"
       (match sw.sw_failaction with None -> "switch*" | _ -> "switch")
       lam larg switch sw
  | Lstringswitch(arg, cases, default, _) ->
      let switch ppf cases =
        let spc = ref false in
        List.iter
         (fun (s, l) ->
           if !spc then fprintf ppf "@ " else spc := true;
           fprintf ppf "@[<hv 1>case \"%s\":@ %a@]" (String.escaped s) lam l)
          cases;
        begin match default with
        | Some default ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>default:@ %a@]" lam default
        | None -> ()
        end in
      fprintf ppf
       "@[<1>(stringswitch %a@ @[<v 0>%a@])@]" lam arg switch cases
  | Lstaticraise (i, ls)  ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(exit@ %d%a)@]" i lams ls;
  | Lstaticcatch(lbody, (i, vars), lhandler) ->
      fprintf ppf "@[<2>(catch@ %a@;<1 -1>with (%d%a)@ %a)@]"
        lam lbody i
        (fun ppf vars -> match vars with
          | [] -> ()
          | _ ->
              List.iter
                (fun x -> fprintf ppf " %a" Ident.print x)
                vars)
        vars
        lam lhandler
  | Ltrywith(lbody, param, lhandler) ->
      fprintf ppf "@[<2>(try@ %a@;<1 -1>with %a@ %a)@]"
        lam lbody Ident.print param lam lhandler
  | Lifthenelse(lcond, lif, lelse) ->
      fprintf ppf "@[<2>(if@ %a@ %a@ %a)@]" lam lcond lam lif lam lelse
  | Lsequence(l1, l2) ->
      fprintf ppf "@[<2>(seq@ %a@ %a)@]" lam l1 sequence l2
  | Lwhile(lcond, lbody) ->
      fprintf ppf "@[<2>(while@ %a@ %a)@]" lam lcond lam lbody
  | Lfor(param, lo, hi, dir, body) ->
      fprintf ppf "@[<2>(for %a@ %a@ %s@ %a@ %a)@]"
       Ident.print param lam lo
       (match dir with Upto -> "to" | Downto -> "downto")
       lam hi lam body
  | Lassign(id, expr) ->
      fprintf ppf "@[<2>(assign@ %a@ %a)@]" Ident.print id lam expr
  | Lsend (name,obj,  _) ->
      fprintf ppf "@[<2>(send%s@ %a@ )@]"  name lam obj

and sequence ppf = function
  | Lsequence(l1, l2) ->
      fprintf ppf "%a@ %a" sequence l1 sequence l2
  | l ->
      lam ppf l

let structured_constant = struct_const

let lambda = lam


