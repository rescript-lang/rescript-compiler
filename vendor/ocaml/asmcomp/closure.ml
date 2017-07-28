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

(* Introduction of closures, uncurrying, recognition of direct calls *)

open Misc
open Asttypes
open Primitive
open Lambda
open Switch
open Clambda

module Storer =
  Switch.Store
    (struct
      type t = lambda
      type key = lambda
      let make_key =  Lambda.make_key
    end)

(* Auxiliaries for compiling functions *)

let rec split_list n l =
  if n <= 0 then ([], l) else begin
    match l with
      [] -> fatal_error "Closure.split_list"
    | a::l -> let (l1, l2) = split_list (n-1) l in (a::l1, l2)
  end

let rec build_closure_env env_param pos = function
    [] -> Tbl.empty
  | id :: rem ->
      Tbl.add id (Uprim(Pfield (pos, Fld_na), [Uvar env_param], Debuginfo.none))
              (build_closure_env env_param (pos+1) rem)

(* Auxiliary for accessing globals.  We change the name of the global
   to the name of the corresponding asm symbol.  This is done here
   and no longer in Cmmgen so that approximations stored in .cmx files
   contain the right names if the -for-pack option is active. *)

let getglobal id =
  Uprim(Pgetglobal (Ident.create_persistent (Compilenv.symbol_for_global id)),
        [], Debuginfo.none)

(* Check if a variable occurs in a [clambda] term. *)

let occurs_var var u =
  let rec occurs = function
      Uvar v -> v = var
    | Uconst _ -> false
    | Udirect_apply(lbl, args, _) -> List.exists occurs args
    | Ugeneric_apply(funct, args, _) -> occurs funct || List.exists occurs args
    | Uclosure(fundecls, clos) -> List.exists occurs clos
    | Uoffset(u, ofs) -> occurs u
    | Ulet(id, def, body) -> occurs def || occurs body
    | Uletrec(decls, body) ->
        List.exists (fun (id, u) -> occurs u) decls || occurs body
    | Uprim(p, args, _) -> List.exists occurs args
    | Uswitch(arg, s) ->
        occurs arg ||
        occurs_array s.us_actions_consts || occurs_array s.us_actions_blocks
    | Ustringswitch(arg,sw,d) ->
        occurs arg ||
        List.exists (fun (_,e) -> occurs e) sw ||
        (match d with None -> false | Some d -> occurs d)
    | Ustaticfail (_, args) -> List.exists occurs args
    | Ucatch(_, _, body, hdlr) -> occurs body || occurs hdlr
    | Utrywith(body, exn, hdlr) -> occurs body || occurs hdlr
    | Uifthenelse(cond, ifso, ifnot) ->
        occurs cond || occurs ifso || occurs ifnot
    | Usequence(u1, u2) -> occurs u1 || occurs u2
    | Uwhile(cond, body) -> occurs cond || occurs body
    | Ufor(id, lo, hi, dir, body) -> occurs lo || occurs hi || occurs body
    | Uassign(id, u) -> id = var || occurs u
    | Usend(_, met, obj, args, _) ->
        occurs met || occurs obj || List.exists occurs args
  and occurs_array a =
    try
      for i = 0 to Array.length a - 1 do
        if occurs a.(i) then raise Exit
      done;
      false
    with Exit ->
      true
  in occurs u

(* Split a function with default parameters into a wrapper and an
   inner function.  The wrapper fills in missing optional parameters
   with their default value and tail-calls the inner function.  The
   wrapper can then hopefully be inlined on most call sites to avoid
   the overhead associated with boxing an optional argument with a
   'Some' constructor, only to deconstruct it immediately in the
   function's body. *)

let split_default_wrapper fun_id kind params body =
  let rec aux map = function
    | Llet(Strict, id, (Lifthenelse(Lvar optparam, _, _) as def), rest) when
        Ident.name optparam = "*opt*" && List.mem optparam params
          && not (List.mem_assoc optparam map)
      ->
        let wrapper_body, inner = aux ((optparam, id) :: map) rest in
        Llet(Strict, id, def, wrapper_body), inner
    | _ when map = [] -> raise Exit
    | body ->
        (* Check that those *opt* identifiers don't appear in the remaining
           body. This should not appear, but let's be on the safe side. *)
        let fv = Lambda.free_variables body in
        List.iter (fun (id, _) -> if IdentSet.mem id fv then raise Exit) map;

        let inner_id = Ident.create (Ident.name fun_id ^ "_inner") in
        let map_param p = try List.assoc p map with Not_found -> p in
        let args = List.map (fun p -> Lvar (map_param p)) params in
        let wrapper_body = Lapply (Lvar inner_id, args, Location.none) in

        let inner_params = List.map map_param params in
        let new_ids = List.map Ident.rename inner_params in
        let subst = List.fold_left2
            (fun s id new_id ->
               Ident.add id (Lvar new_id) s)
            Ident.empty inner_params new_ids
        in
        let body = Lambda.subst_lambda subst body in
        let inner_fun = Lfunction(Curried, new_ids, body) in
        (wrapper_body, (inner_id, inner_fun))
  in
  try
    let wrapper_body, inner = aux [] body in
    [(fun_id, Lfunction(kind, params, wrapper_body)); inner]
  with Exit ->
    [(fun_id, Lfunction(kind, params, body))]


(* Determine whether the estimated size of a clambda term is below
   some threshold *)

let prim_size prim args =
  match prim with
  | (Pidentity | Pbytes_to_string | Pbytes_of_string ) -> 0
  | Pgetglobal id -> 1
  | Psetglobal id -> 1
  | Pmakeblock(tag, _, mut) -> 5 + List.length args
  | Pfield _ -> 1
  | Psetfield(f, isptr,_) -> if isptr then 4 else 1
  | Pfloatfield _ -> 1
  | Psetfloatfield _ -> 1
  | Pduprecord _ -> 10 + List.length args
  | Pccall p -> (if p.prim_alloc then 10 else 4) + List.length args
  | Praise _ -> 4
  | Pstringlength | Pbyteslength -> 5
  | Pstringrefs | Pstringsets | Pbytesrefs | Pbytessets -> 6
  | Pmakearray kind -> 5 + List.length args
  | Parraylength kind -> if kind = Pgenarray then 6 else 2
  | Parrayrefu kind -> if kind = Pgenarray then 12 else 2
  | Parraysetu kind -> if kind = Pgenarray then 16 else 4
  | Parrayrefs kind -> if kind = Pgenarray then 18 else 8
  | Parraysets kind -> if kind = Pgenarray then 22 else 10
  | Pbittest -> 3
  | Pbigarrayref(_, ndims, _, _) -> 4 + ndims * 6
  | Pbigarrayset(_, ndims, _, _) -> 4 + ndims * 6
  | _ -> 2 (* arithmetic and comparisons *)

(* Very raw approximation of switch cost *)

let lambda_smaller lam threshold =
  let size = ref 0 in
  let rec lambda_size lam =
    if !size > threshold then raise Exit;
    match lam with
      Uvar v -> ()
    | Uconst _ -> incr size
    | Udirect_apply(fn, args, _) ->
        size := !size + 4; lambda_list_size args
    | Ugeneric_apply(fn, args, _) ->
        size := !size + 6; lambda_size fn; lambda_list_size args
    | Uclosure(defs, vars) ->
        raise Exit (* inlining would duplicate function definitions *)
    | Uoffset(lam, ofs) ->
        incr size; lambda_size lam
    | Ulet(id, lam, body) ->
        lambda_size lam; lambda_size body
    | Uletrec(bindings, body) ->
        raise Exit (* usually too large *)
    | Uprim(prim, args, _) ->
        size := !size + prim_size prim args;
        lambda_list_size args
    | Uswitch(lam, cases) ->
        if Array.length cases.us_actions_consts > 1 then size := !size + 5 ;
        if Array.length cases.us_actions_blocks > 1 then size := !size + 5 ;
        lambda_size lam;
        lambda_array_size cases.us_actions_consts ;
        lambda_array_size cases.us_actions_blocks
    | Ustringswitch (lam,sw,d) ->
        lambda_size lam ;
       (* as ifthenelse *)
        List.iter
          (fun (_,lam) ->
            size := !size+2 ;
            lambda_size lam)
          sw ;
        Misc.may lambda_size d
    | Ustaticfail (_,args) -> lambda_list_size args
    | Ucatch(_, _, body, handler) ->
        incr size; lambda_size body; lambda_size handler
    | Utrywith(body, id, handler) ->
        size := !size + 8; lambda_size body; lambda_size handler
    | Uifthenelse(cond, ifso, ifnot) ->
        size := !size + 2;
        lambda_size cond; lambda_size ifso; lambda_size ifnot
    | Usequence(lam1, lam2) ->
        lambda_size lam1; lambda_size lam2
    | Uwhile(cond, body) ->
        size := !size + 2; lambda_size cond; lambda_size body
    | Ufor(id, low, high, dir, body) ->
        size := !size + 4; lambda_size low; lambda_size high; lambda_size body
    | Uassign(id, lam) ->
        incr size;  lambda_size lam
    | Usend(_, met, obj, args, _) ->
        size := !size + 8;
        lambda_size met; lambda_size obj; lambda_list_size args
  and lambda_list_size l = List.iter lambda_size l
  and lambda_array_size a = Array.iter lambda_size a in
  try
    lambda_size lam; !size <= threshold
  with Exit ->
    false

(* Check if a clambda term is ``pure'',
   that is without side-effects *and* not containing function definitions *)

let rec is_pure_clambda = function
    Uvar v -> true
  | Uconst _ -> true
  | Uprim((Psetglobal _ | Psetfield _ | Psetfloatfield _ | Pduprecord _ |
           Pccall _ | Praise _ | Poffsetref _ | Pstringsetu | Pstringsets |
           Pbytessetu | Pbytessets |
           Parraysetu _ | Parraysets _ | Pbigarrayset _), _, _) -> false
  | Uprim(p, args, _) -> List.for_all is_pure_clambda args
  | _ -> false

(* Simplify primitive operations on known arguments *)

let make_const c = (Uconst c, Value_const c)
let make_const_ref c =
  make_const(Uconst_ref(Compilenv.new_structured_constant ~shared:true c, c))
let make_const_int n = make_const (Uconst_int n)
let make_const_ptr n = make_const (Uconst_ptr n)
let make_const_bool b = make_const_ptr(if b then 1 else 0)
let make_comparison cmp x y =
  make_const_bool
    (match cmp with
       Ceq -> x = y
     | Cneq -> x <> y
     | Clt -> x < y
     | Cgt -> x > y
     | Cle -> x <= y
     | Cge -> x >= y)
let make_const_float n = make_const_ref (Uconst_float n)
let make_const_natint n = make_const_ref (Uconst_nativeint n)
let make_const_int32 n = make_const_ref (Uconst_int32 n)
let make_const_int64 n = make_const_ref (Uconst_int64 n)

(* The [fpc] parameter is true if constant propagation of
   floating-point computations is allowed *)

let simplif_arith_prim_pure fpc p (args, approxs) dbg =
  let default = (Uprim(p, args, dbg), Value_unknown) in
  match approxs with
  (* int (or enumerated type) *)
  | [ Value_const(Uconst_int n1 | Uconst_ptr n1) ] ->
      begin match p with
      | Pnot -> make_const_bool (n1 = 0)
      | Pnegint -> make_const_int (- n1)
      | Poffsetint n -> make_const_int (n + n1)
      | Pfloatofint when fpc -> make_const_float (float_of_int n1)
      | Pbintofint Pnativeint -> make_const_natint (Nativeint.of_int n1)
      | Pbintofint Pint32 -> make_const_int32 (Int32.of_int n1)
      | Pbintofint Pint64 -> make_const_int64 (Int64.of_int n1)
      | Pbswap16 -> make_const_int (((n1 land 0xff) lsl 8)
                                    lor ((n1 land 0xff00) lsr 8))
      | _ -> default
      end
  (* int (or enumerated type), int (or enumerated type) *)
  | [ Value_const(Uconst_int n1 | Uconst_ptr n1);
      Value_const(Uconst_int n2 | Uconst_ptr n2) ] ->
      begin match p with
      | Psequand -> make_const_bool (n1 <> 0 && n2 <> 0)
      | Psequor -> make_const_bool (n1 <> 0 || n2 <> 0)
      | Paddint -> make_const_int (n1 + n2)
      | Psubint -> make_const_int (n1 - n2)
      | Pmulint -> make_const_int (n1 * n2)
      | Pdivint when n2 <> 0 -> make_const_int (n1 / n2)
      | Pmodint when n2 <> 0 -> make_const_int (n1 mod n2)
      | Pandint -> make_const_int (n1 land n2)
      | Porint -> make_const_int (n1 lor n2)
      | Pxorint -> make_const_int (n1 lxor n2)
      | Plslint when 0 <= n2 && n2 < 8 * Arch.size_int ->
          make_const_int (n1 lsl n2)
      | Plsrint when 0 <= n2 && n2 < 8 * Arch.size_int ->
          make_const_int (n1 lsr n2)
      | Pasrint when 0 <= n2 && n2 < 8 * Arch.size_int ->
          make_const_int (n1 asr n2)
      | Pintcomp c -> make_comparison c n1 n2
      | _ -> default
      end
  (* float *)
  | [Value_const(Uconst_ref(_, Uconst_float n1))] when fpc ->
      begin match p with
      | Pintoffloat -> make_const_int (int_of_float n1)
      | Pnegfloat -> make_const_float (-. n1)
      | Pabsfloat -> make_const_float (abs_float n1)
      | _ -> default
      end
  (* float, float *)
  | [Value_const(Uconst_ref(_, Uconst_float n1));
     Value_const(Uconst_ref(_, Uconst_float n2))] when fpc ->
      begin match p with
      | Paddfloat -> make_const_float (n1 +. n2)
      | Psubfloat -> make_const_float (n1 -. n2)
      | Pmulfloat -> make_const_float (n1 *. n2)
      | Pdivfloat -> make_const_float (n1 /. n2)
      | Pfloatcomp c  -> make_comparison c n1 n2
      | _ -> default
      end
  (* nativeint *)
  | [Value_const(Uconst_ref(_, Uconst_nativeint n))] ->
      begin match p with
      | Pintofbint Pnativeint -> make_const_int (Nativeint.to_int n)
      | Pcvtbint(Pnativeint, Pint32) -> make_const_int32 (Nativeint.to_int32 n)
      | Pcvtbint(Pnativeint, Pint64) -> make_const_int64 (Int64.of_nativeint n)
      | Pnegbint Pnativeint -> make_const_natint (Nativeint.neg n)
      | _ -> default
      end
  (* nativeint, nativeint *)
  | [Value_const(Uconst_ref(_, Uconst_nativeint n1));
     Value_const(Uconst_ref(_, Uconst_nativeint n2))] ->
      begin match p with
      | Paddbint Pnativeint -> make_const_natint (Nativeint.add n1 n2)
      | Psubbint Pnativeint -> make_const_natint (Nativeint.sub n1 n2)
      | Pmulbint Pnativeint -> make_const_natint (Nativeint.mul n1 n2)
      | Pdivbint Pnativeint when n2 <> 0n ->
          make_const_natint (Nativeint.div n1 n2)
      | Pmodbint Pnativeint when n2 <> 0n ->
          make_const_natint (Nativeint.rem n1 n2)
      | Pandbint Pnativeint -> make_const_natint (Nativeint.logand n1 n2)
      | Porbint Pnativeint ->  make_const_natint (Nativeint.logor n1 n2)
      | Pxorbint Pnativeint -> make_const_natint (Nativeint.logxor n1 n2)
      | Pbintcomp(Pnativeint, c)  -> make_comparison c n1 n2
      | _ -> default
      end
  (* nativeint, int *)
  | [Value_const(Uconst_ref(_, Uconst_nativeint n1));
     Value_const(Uconst_int n2)] ->
      begin match p with
      | Plslbint Pnativeint when 0 <= n2 && n2 < 8 * Arch.size_int ->
          make_const_natint (Nativeint.shift_left n1 n2)
      | Plsrbint Pnativeint when 0 <= n2 && n2 < 8 * Arch.size_int ->
          make_const_natint (Nativeint.shift_right_logical n1 n2)
      | Pasrbint Pnativeint when 0 <= n2 && n2 < 8 * Arch.size_int ->
          make_const_natint (Nativeint.shift_right n1 n2)
      | _ -> default
      end
  (* int32 *)
  | [Value_const(Uconst_ref(_, Uconst_int32 n))] ->
      begin match p with
      | Pintofbint Pint32 -> make_const_int (Int32.to_int n)
      | Pcvtbint(Pint32, Pnativeint) -> make_const_natint (Nativeint.of_int32 n)
      | Pcvtbint(Pint32, Pint64) -> make_const_int64 (Int64.of_int32 n)
      | Pnegbint Pint32 -> make_const_int32 (Int32.neg n)
      | _ -> default
      end
  (* int32, int32 *)
  | [Value_const(Uconst_ref(_, Uconst_int32 n1));
     Value_const(Uconst_ref(_, Uconst_int32 n2))] ->
      begin match p with
      | Paddbint Pint32 -> make_const_int32 (Int32.add n1 n2)
      | Psubbint Pint32 -> make_const_int32 (Int32.sub n1 n2)
      | Pmulbint Pint32 -> make_const_int32 (Int32.mul n1 n2)
      | Pdivbint Pint32 when n2 <> 0l -> make_const_int32 (Int32.div n1 n2)
      | Pmodbint Pint32 when n2 <> 0l -> make_const_int32 (Int32.rem n1 n2)
      | Pandbint Pint32 -> make_const_int32 (Int32.logand n1 n2)
      | Porbint Pint32 -> make_const_int32 (Int32.logor n1 n2)
      | Pxorbint Pint32 -> make_const_int32 (Int32.logxor n1 n2)
      | Pbintcomp(Pint32, c) -> make_comparison c n1 n2
      | _ -> default
      end
  (* int32, int *)
  | [Value_const(Uconst_ref(_, Uconst_int32 n1));
     Value_const(Uconst_int n2)] ->
      begin match p with
      | Plslbint Pint32 when 0 <= n2 && n2 < 32 ->
          make_const_int32 (Int32.shift_left n1 n2)
      | Plsrbint Pint32 when 0 <= n2 && n2 < 32 ->
          make_const_int32 (Int32.shift_right_logical n1 n2)
      | Pasrbint Pint32 when 0 <= n2 && n2 < 32 ->
          make_const_int32 (Int32.shift_right n1 n2)
      | _ -> default
      end
  (* int64 *)
  | [Value_const(Uconst_ref(_, Uconst_int64 n))] ->
      begin match p with
      | Pintofbint Pint64 -> make_const_int (Int64.to_int n)
      | Pcvtbint(Pint64, Pint32) -> make_const_int32 (Int64.to_int32 n)
      | Pcvtbint(Pint64, Pnativeint) -> make_const_natint (Int64.to_nativeint n)
      | Pnegbint Pint64 -> make_const_int64 (Int64.neg n)
      | _ -> default
      end
  (* int64, int64 *)
  | [Value_const(Uconst_ref(_, Uconst_int64 n1));
     Value_const(Uconst_ref(_, Uconst_int64 n2))] ->
      begin match p with
      | Paddbint Pint64 -> make_const_int64 (Int64.add n1 n2)
      | Psubbint Pint64 -> make_const_int64 (Int64.sub n1 n2)
      | Pmulbint Pint64 -> make_const_int64 (Int64.mul n1 n2)
      | Pdivbint Pint64 when n2 <> 0L -> make_const_int64 (Int64.div n1 n2)
      | Pmodbint Pint64 when n2 <> 0L -> make_const_int64 (Int64.rem n1 n2)
      | Pandbint Pint64 -> make_const_int64 (Int64.logand n1 n2)
      | Porbint Pint64 -> make_const_int64 (Int64.logor n1 n2)
      | Pxorbint Pint64 -> make_const_int64 (Int64.logxor n1 n2)
      | Pbintcomp(Pint64, c) -> make_comparison c n1 n2
      | _ -> default
      end
  (* int64, int *)
  | [Value_const(Uconst_ref(_, Uconst_int64 n1));
     Value_const(Uconst_int n2)] ->
      begin match p with
      | Plslbint Pint64 when 0 <= n2 && n2 < 64 ->
          make_const_int64 (Int64.shift_left n1 n2)
      | Plsrbint Pint64 when 0 <= n2 && n2 < 64 ->
          make_const_int64 (Int64.shift_right_logical n1 n2)
      | Pasrbint Pint64 when 0 <= n2 && n2 < 64 ->
          make_const_int64 (Int64.shift_right n1 n2)
      | _ -> default
      end
  (* TODO: Pbbswap *)
  (* Catch-all *)
  | _ ->
     default

let field_approx n = function
  | Value_tuple a when n < Array.length a -> a.(n)
  | Value_const (Uconst_ref(_, Uconst_block(_, l))) when n < List.length l ->
      Value_const (List.nth l n)
  | _ -> Value_unknown

let simplif_prim_pure fpc p (args, approxs) dbg =
  match p, args, approxs with
  (* Block construction *)
  | Pmakeblock(tag, _, Immutable), _, _ ->
      let field = function
        | Value_const c -> c
        | _ -> raise Exit
      in
      begin try
        let cst = Uconst_block (tag, List.map field approxs) in
        let name =
          Compilenv.new_structured_constant cst ~shared:true
        in
        make_const (Uconst_ref (name, cst))
      with Exit ->
        (Uprim(p, args, dbg), Value_tuple (Array.of_list approxs))
      end
  (* Field access *)
  | Pfield (n,_), _, [ Value_const(Uconst_ref(_, Uconst_block(_, l))) ]
    when n < List.length l ->
      make_const (List.nth l n)
  | Pfield (n,_), [ Uprim(Pmakeblock _, ul, _) ], [approx]
    when n < List.length ul ->
      (List.nth ul n, field_approx n approx)
  (* Strings *)
  | (Pstringlength | Pbyteslength), _, [ Value_const(Uconst_ref(_, Uconst_string s)) ] ->
      make_const_int (String.length s)
  (* Identity *)
  | (Pidentity | Pbytes_of_string | Pbytes_to_string ), [arg1], [app1] ->
      (arg1, app1)
  (* Kind test *)
  | Pisint, _, [a1] ->
      begin match a1 with
      | Value_const(Uconst_int _ | Uconst_ptr _) -> make_const_bool true
      | Value_const(Uconst_ref _) -> make_const_bool false
      | Value_closure _ | Value_tuple _ -> make_const_bool false
      | _ -> (Uprim(p, args, dbg), Value_unknown)
      end
  (* Compile-time constants *)
  | Pctconst c, _, _ ->
      begin match c with
        | Big_endian -> make_const_bool Arch.big_endian
        | Word_size -> make_const_int (8*Arch.size_int)
        | Ostype_unix -> make_const_bool (Sys.os_type = "Unix")
        | Ostype_win32 -> make_const_bool (Sys.os_type = "Win32")
        | Ostype_cygwin -> make_const_bool (Sys.os_type = "Cygwin")
      end
  (* Catch-all *)
  | _ ->
      simplif_arith_prim_pure fpc p (args, approxs) dbg

let simplif_prim fpc p (args, approxs as args_approxs) dbg =
  if List.for_all is_pure_clambda args
  then simplif_prim_pure fpc p args_approxs dbg
  else
    (* XXX : always return the same approxs as simplif_prim_pure? *)
    let approx =
      match p with
      | Pmakeblock(_, _, Immutable) ->
          Value_tuple (Array.of_list approxs)
      | _ ->
          Value_unknown
    in
    (Uprim(p, args, dbg), approx)

(* Substitute variables in a [ulambda] term (a body of an inlined function)
   and perform some more simplifications on integer primitives.
   Also perform alpha-conversion on let-bound identifiers to avoid
   clashes with locally-generated identifiers.
   The variables must not be assigned in the term.
   This is used to substitute "trivial" arguments for parameters
   during inline expansion, and also for the translation of let rec
   over functions. *)

let approx_ulam = function
    Uconst c -> Value_const c
  | _ -> Value_unknown

let rec substitute fpc sb ulam =
  match ulam with
    Uvar v ->
      begin try Tbl.find v sb with Not_found -> ulam end
  | Uconst _ -> ulam
  | Udirect_apply(lbl, args, dbg) ->
      Udirect_apply(lbl, List.map (substitute fpc sb) args, dbg)
  | Ugeneric_apply(fn, args, dbg) ->
      Ugeneric_apply(substitute fpc sb fn,
                     List.map (substitute fpc sb) args, dbg)
  | Uclosure(defs, env) ->
      (* Question: should we rename function labels as well?  Otherwise,
         there is a risk that function labels are not globally unique.
         This should not happen in the current system because:
         - Inlined function bodies contain no Uclosure nodes
           (cf. function [lambda_smaller])
         - When we substitute offsets for idents bound by let rec
           in [close], case [Lletrec], we discard the original
           let rec body and use only the substituted term. *)
      Uclosure(defs, List.map (substitute fpc sb) env)
  | Uoffset(u, ofs) -> Uoffset(substitute fpc sb u, ofs)
  | Ulet(id, u1, u2) ->
      let id' = Ident.rename id in
      Ulet(id', substitute fpc sb u1,
           substitute fpc (Tbl.add id (Uvar id') sb) u2)
  | Uletrec(bindings, body) ->
      let bindings1 =
        List.map (fun (id, rhs) -> (id, Ident.rename id, rhs)) bindings in
      let sb' =
        List.fold_right
          (fun (id, id', _) s -> Tbl.add id (Uvar id') s)
          bindings1 sb in
      Uletrec(
        List.map
           (fun (id, id', rhs) -> (id', substitute fpc sb' rhs))
           bindings1,
        substitute fpc sb' body)
  | Uprim(p, args, dbg) ->
      let sargs =
        List.map (substitute fpc sb) args in
      let (res, _) =
        simplif_prim fpc p (sargs, List.map approx_ulam sargs) dbg in
      res
  | Uswitch(arg, sw) ->
      Uswitch(substitute fpc sb arg,
              { sw with
                us_actions_consts =
                  Array.map (substitute fpc sb) sw.us_actions_consts;
                us_actions_blocks =
                  Array.map (substitute fpc sb) sw.us_actions_blocks;
               })
  | Ustringswitch(arg,sw,d) ->
      Ustringswitch
        (substitute fpc sb arg,
         List.map (fun (s,act) -> s,substitute fpc sb act) sw,
         Misc.may_map (substitute fpc sb) d)
  | Ustaticfail (nfail, args) ->
      Ustaticfail (nfail, List.map (substitute fpc sb) args)
  | Ucatch(nfail, ids, u1, u2) ->
      Ucatch(nfail, ids, substitute fpc sb u1, substitute fpc sb u2)
  | Utrywith(u1, id, u2) ->
      let id' = Ident.rename id in
      Utrywith(substitute fpc sb u1, id',
               substitute fpc (Tbl.add id (Uvar id') sb) u2)
  | Uifthenelse(u1, u2, u3) ->
      begin match substitute fpc sb u1 with
        Uconst (Uconst_ptr n) ->
          if n <> 0 then substitute fpc sb u2 else substitute fpc sb u3
      | Uprim(Pmakeblock _, _, _) ->
          substitute fpc sb u2
      | su1 ->
          Uifthenelse(su1, substitute fpc sb u2, substitute fpc sb u3)
      end
  | Usequence(u1, u2) ->
      Usequence(substitute fpc sb u1, substitute fpc sb u2)
  | Uwhile(u1, u2) ->
      Uwhile(substitute fpc sb u1, substitute fpc sb u2)
  | Ufor(id, u1, u2, dir, u3) ->
      let id' = Ident.rename id in
      Ufor(id', substitute fpc sb u1, substitute fpc sb u2, dir,
           substitute fpc (Tbl.add id (Uvar id') sb) u3)
  | Uassign(id, u) ->
      let id' =
        try
          match Tbl.find id sb with Uvar i -> i | _ -> assert false
        with Not_found ->
          id in
      Uassign(id', substitute fpc sb u)
  | Usend(k, u1, u2, ul, dbg) ->
      Usend(k, substitute fpc sb u1, substitute fpc sb u2,
            List.map (substitute fpc sb) ul, dbg)

(* Perform an inline expansion *)

let is_simple_argument = function
  | Uvar _  | Uconst _ -> true
  | _ -> false

let no_effects = function
  | Uclosure _ -> true
  | u -> is_simple_argument u

let rec bind_params_rec fpc subst params args body =
  match (params, args) with
    ([], []) -> substitute fpc subst body
  | (p1 :: pl, a1 :: al) ->
      if is_simple_argument a1 then
        bind_params_rec fpc (Tbl.add p1 a1 subst) pl al body
      else begin
        let p1' = Ident.rename p1 in
        let u1, u2 =
          match Ident.name p1, a1 with
          | "*opt*", Uprim(Pmakeblock(0, tag_info, Immutable), [a], dbg) ->
              a, Uprim(Pmakeblock(0, tag_info, Immutable), [Uvar p1'], dbg)
          | _ ->
              a1, Uvar p1'
        in
        let body' =
          bind_params_rec fpc (Tbl.add p1 u2 subst) pl al body in
        if occurs_var p1 body then Ulet(p1', u1, body')
        else if no_effects a1 then body'
        else Usequence(a1, body')
      end
  | (_, _) -> assert false

let bind_params fpc params args body =
  (* Reverse parameters and arguments to preserve right-to-left
     evaluation order (PR#2910). *)
  bind_params_rec fpc Tbl.empty (List.rev params) (List.rev args) body

(* Check if a lambda term is ``pure'',
   that is without side-effects *and* not containing function definitions *)

let rec is_pure = function
    Lvar v -> true
  | Lconst cst -> true
  | Lprim((Psetglobal _ | Psetfield _ | Psetfloatfield _ | Pduprecord _ |
           Pccall _ | Praise _ | Poffsetref _ | Pstringsetu | Pstringsets |
           Pbytessetu | Pbytessets |
           Parraysetu _ | Parraysets _ | Pbigarrayset _), _,_) -> false
  | Lprim(p, args,_) -> List.for_all is_pure args
  | Levent(lam, ev) -> is_pure lam
  | _ -> false

(* Generate a direct application *)

let direct_apply fundesc funct ufunct uargs =
  let app_args =
    if fundesc.fun_closed then uargs else uargs @ [ufunct] in
  let app =
    match fundesc.fun_inline with
    | None ->
        Udirect_apply(fundesc.fun_label, app_args, Debuginfo.none)
    | Some(params, body) ->
        bind_params fundesc.fun_float_const_prop params app_args body in
  (* If ufunct can contain side-effects or function definitions,
     we must make sure that it is evaluated exactly once.
     If the function is not closed, we evaluate ufunct as part of the
     arguments.
     If the function is closed, we force the evaluation of ufunct first. *)
  if not fundesc.fun_closed || is_pure funct
  then app
  else Usequence(ufunct, app)

(* Add [Value_integer] or [Value_constptr] info to the approximation
   of an application *)

let strengthen_approx appl approx =
  match approx_ulam appl with
    (Value_const _) as intapprox ->
      intapprox
  | _ -> approx

(* If a term has approximation Value_integer or Value_constptr and is pure,
   replace it by an integer constant *)

let check_constant_result lam ulam approx =
  match approx with
    Value_const c when is_pure lam -> make_const c
  | Value_global_field (id, i) when is_pure lam ->
      begin match ulam with
      | Uprim(Pfield _, [Uprim(Pgetglobal _, _, _)], _) -> (ulam, approx)
      | _ ->
          let glb =
            Uprim(Pgetglobal (Ident.create_persistent id), [], Debuginfo.none)
          in
          Uprim(Pfield (i, Fld_na), [glb], Debuginfo.none), approx
      end
  | _ -> (ulam, approx)

(* Evaluate an expression with known value for its side effects only,
   or discard it if it's pure *)

let sequence_constant_expr lam ulam1 (ulam2, approx2 as res2) =
  if is_pure lam then res2 else (Usequence(ulam1, ulam2), approx2)

(* Maintain the approximation of the global structure being defined *)

let global_approx = ref([||] : value_approximation array)

(* Maintain the nesting depth for functions *)

let function_nesting_depth = ref 0
let excessive_function_nesting_depth = 5

(* Decorate clambda term with debug information *)

let rec add_debug_info ev u =
  match ev.lev_kind with
  | Lev_after _ ->
      begin match u with
      | Udirect_apply(lbl, args, dinfo) ->
          Udirect_apply(lbl, args, Debuginfo.from_call ev)
      | Ugeneric_apply(Udirect_apply(lbl, args1, dinfo1),
                       args2, dinfo2) ->
          Ugeneric_apply(Udirect_apply(lbl, args1, Debuginfo.from_call ev),
                         args2, Debuginfo.from_call ev)
      | Ugeneric_apply(fn, args, dinfo) ->
          Ugeneric_apply(fn, args, Debuginfo.from_call ev)
      | Uprim(Praise k, args, dinfo) ->
          Uprim(Praise k, args, Debuginfo.from_call ev)
      | Uprim(p, args, dinfo) ->
          Uprim(p, args, Debuginfo.from_call ev)
      | Usend(kind, u1, u2, args, dinfo) ->
          Usend(kind, u1, u2, args, Debuginfo.from_call ev)
      | Usequence(u1, u2) ->
          Usequence(u1, add_debug_info ev u2)
      | _ -> u
      end
  | _ -> u

(* Uncurry an expression and explicitate closures.
   Also return the approximation of the expression.
   The approximation environment [fenv] maps idents to approximations.
   Idents not bound in [fenv] approximate to [Value_unknown].
   The closure environment [cenv] maps idents to [ulambda] terms.
   It is used to substitute environment accesses for free identifiers. *)

exception NotClosed

let close_approx_var fenv cenv id =
  let approx = try Tbl.find id fenv with Not_found -> Value_unknown in
  match approx with
    Value_const c -> make_const c
  | approx ->
      let subst = try Tbl.find id cenv with Not_found -> Uvar id in
      (subst, approx)

let close_var fenv cenv id =
  let (ulam, app) = close_approx_var fenv cenv id in ulam

let rec close fenv cenv = function
    Lvar id ->
      close_approx_var fenv cenv id
  | Lconst cst ->
      let str ?(shared = true) cst =
        let name =
          Compilenv.new_structured_constant cst ~shared
        in
        Uconst_ref (name, cst)
      in
      let rec transl = function
        | Const_base(Const_int n) -> Uconst_int n
        | Const_base(Const_char c) -> Uconst_int (Char.code c)
        | Const_pointer (n,_) -> Uconst_ptr n
        | Const_block (tag, _, fields) ->
            str (Uconst_block (tag, List.map transl fields))
        | Const_float_array sl ->
            (* constant float arrays are really immutable *)
            str (Uconst_float_array (List.map float_of_string sl))
        | Const_immstring s ->
            str (Uconst_string s)
        | Const_base (Const_string (s, _)) ->
              (* strings (even literal ones) are mutable! *)
              (* of course, the empty string is really immutable *)
            str ~shared:false(*(String.length s = 0)*) (Uconst_string s)
        | Const_base(Const_float x) -> str (Uconst_float (float_of_string x))
        | Const_base(Const_int32 x) -> str (Uconst_int32 x)
        | Const_base(Const_int64 x) -> str (Uconst_int64 x)
        | Const_base(Const_nativeint x) -> str (Uconst_nativeint x)
      in
      make_const (transl cst)
  | Lfunction(kind, params, body) as funct ->
      close_one_function fenv cenv (Ident.create "fun") funct

    (* We convert [f a] to [let a' = a in fun b c -> f a' b c]
       when fun_arity > nargs *)
  | Lapply(funct, args, loc) ->
      let nargs = List.length args in
      begin match (close fenv cenv funct, close_list fenv cenv args) with
        ((ufunct, Value_closure(fundesc, approx_res)),
         [Uprim(Pmakeblock(_,_,  _), uargs, _)])
        when List.length uargs = - fundesc.fun_arity ->
          let app = direct_apply fundesc funct ufunct uargs in
          (app, strengthen_approx app approx_res)
      | ((ufunct, Value_closure(fundesc, approx_res)), uargs)
        when nargs = fundesc.fun_arity ->
          let app = direct_apply fundesc funct ufunct uargs in
          (app, strengthen_approx app approx_res)

      | ((ufunct, Value_closure(fundesc, approx_res)), uargs)
          when nargs < fundesc.fun_arity ->
        let first_args = List.map (fun arg ->
          (Ident.create "arg", arg) ) uargs in
        let final_args =
          Array.to_list (Array.init (fundesc.fun_arity - nargs)
                                    (fun _ -> Ident.create "arg")) in
        let rec iter args body =
          match args with
              [] -> body
            | (arg1, arg2) :: args ->
              iter args
                (Ulet ( arg1, arg2, body))
        in
        let internal_args =
          (List.map (fun (arg1, arg2) -> Lvar arg1) first_args)
          @ (List.map (fun arg -> Lvar arg ) final_args)
        in
        let (new_fun, approx) = close fenv cenv
          (Lfunction(
            Curried, final_args, Lapply(funct, internal_args, loc)))
        in
        let new_fun = iter first_args new_fun in
        (new_fun, approx)

      | ((ufunct, Value_closure(fundesc, approx_res)), uargs)
        when fundesc.fun_arity > 0 && nargs > fundesc.fun_arity ->
          let (first_args, rem_args) = split_list fundesc.fun_arity uargs in
          (Ugeneric_apply(direct_apply fundesc funct ufunct first_args,
                          rem_args, Debuginfo.none),
           Value_unknown)
      | ((ufunct, _), uargs) ->
          (Ugeneric_apply(ufunct, uargs, Debuginfo.none), Value_unknown)
      end
  | Lsend(kind, met, obj, args, _) ->
      let (umet, _) = close fenv cenv met in
      let (uobj, _) = close fenv cenv obj in
      (Usend(kind, umet, uobj, close_list fenv cenv args, Debuginfo.none),
       Value_unknown)
  | Llet(str, id, lam, body) ->
      let (ulam, alam) = close_named fenv cenv id lam in
      begin match (str, alam) with
        (Variable, _) ->
          let (ubody, abody) = close fenv cenv body in
          (Ulet(id, ulam, ubody), abody)
      | (_, Value_const _)
        when str = Alias || is_pure lam ->
          close (Tbl.add id alam fenv) cenv body
      | (_, _) ->
          let (ubody, abody) = close (Tbl.add id alam fenv) cenv body in
          (Ulet(id, ulam, ubody), abody)
      end
  | Lletrec(defs, body) ->
      if List.for_all
           (function (id, Lfunction(_, _, _)) -> true | _ -> false)
           defs
      then begin
        (* Simple case: only function definitions *)
        let (clos, infos) = close_functions fenv cenv defs in
        let clos_ident = Ident.create "clos" in
        let fenv_body =
          List.fold_right
            (fun (id, pos, approx) fenv -> Tbl.add id approx fenv)
            infos fenv in
        let (ubody, approx) = close fenv_body cenv body in
        let sb =
          List.fold_right
            (fun (id, pos, approx) sb ->
              Tbl.add id (Uoffset(Uvar clos_ident, pos)) sb)
            infos Tbl.empty in
        (Ulet(clos_ident, clos, substitute !Clflags.float_const_prop sb ubody),
         approx)
      end else begin
        (* General case: recursive definition of values *)
        let rec clos_defs = function
          [] -> ([], fenv)
        | (id, lam) :: rem ->
            let (udefs, fenv_body) = clos_defs rem in
            let (ulam, approx) = close_named fenv cenv id lam in
            ((id, ulam) :: udefs, Tbl.add id approx fenv_body) in
        let (udefs, fenv_body) = clos_defs defs in
        let (ubody, approx) = close fenv_body cenv body in
        (Uletrec(udefs, ubody), approx)
      end
  | Lprim(Pdirapply ,[funct;arg], loc)
  | Lprim(Prevapply ,[arg;funct], loc) ->
      close fenv cenv (Lapply(funct, [arg], loc))
  | Lprim(Pgetglobal id, [], _) as lam ->
      check_constant_result lam
                            (getglobal id)
                            (Compilenv.global_approx id)
  | Lprim((Pfield (n, _) as field), [lam], _) ->
      let (ulam, approx) = close fenv cenv lam in
      check_constant_result lam (Uprim(field, [ulam], Debuginfo.none))
                            (field_approx n approx)
  | Lprim(Psetfield(n, _,dbg_info), [Lprim(Pgetglobal id, [], _); lam], _) ->
      let (ulam, approx) = close fenv cenv lam in
      if approx <> Value_unknown then
        (!global_approx).(n) <- approx;
      (Uprim(Psetfield(n, false, dbg_info), [getglobal id; ulam], Debuginfo.none),
       Value_unknown)
  | Lprim(Praise k, [Levent(arg, ev)], _) ->
      let (ulam, approx) = close fenv cenv arg in
      (Uprim(Praise k, [ulam], Debuginfo.from_raise ev),
       Value_unknown)
  | Lprim(p, args, _) ->
      simplif_prim !Clflags.float_const_prop
                   p (close_list_approx fenv cenv args) Debuginfo.none
  | Lswitch(arg, sw) ->
      let fn fail =
        let (uarg, _) = close fenv cenv arg in
        let const_index, const_actions, fconst =
          close_switch arg fenv cenv sw.sw_consts sw.sw_numconsts fail
        and block_index, block_actions, fblock =
          close_switch arg fenv cenv sw.sw_blocks sw.sw_numblocks fail in
        let ulam =
          Uswitch
            (uarg,
             {us_index_consts = const_index;
              us_actions_consts = const_actions;
              us_index_blocks = block_index;
              us_actions_blocks = block_actions})  in
        (fconst (fblock ulam),Value_unknown) in
(* NB: failaction might get copied, thus it should be some Lstaticraise *)
      let fail = sw.sw_failaction in
      begin match fail with
      | None|Some (Lstaticraise (_,_)) -> fn fail
      | Some lamfail ->
          if
            (sw.sw_numconsts - List.length sw.sw_consts) +
            (sw.sw_numblocks - List.length sw.sw_blocks) > 1
          then
            let i = next_raise_count () in
            let ubody,_ = fn (Some (Lstaticraise (i,[])))
            and uhandler,_ = close fenv cenv lamfail in
            Ucatch (i,[],ubody,uhandler),Value_unknown
          else fn fail
      end
  | Lstringswitch(arg,sw,d,_) ->
      let uarg,_ = close fenv cenv arg in
      let usw =
        List.map
          (fun (s,act) ->
            let uact,_ = close fenv cenv act in
            s,uact)
          sw in
      let ud =
        Misc.may_map
          (fun d ->
            let ud,_ = close fenv cenv d in
            ud) d in
      Ustringswitch (uarg,usw,ud),Value_unknown
  | Lstaticraise (i, args) ->
      (Ustaticfail (i, close_list fenv cenv args), Value_unknown)
  | Lstaticcatch(body, (i, vars), handler) ->
      let (ubody, _) = close fenv cenv body in
      let (uhandler, _) = close fenv cenv handler in
      (Ucatch(i, vars, ubody, uhandler), Value_unknown)
  | Ltrywith(body, id, handler) ->
      let (ubody, _) = close fenv cenv body in
      let (uhandler, _) = close fenv cenv handler in
      (Utrywith(ubody, id, uhandler), Value_unknown)
  | Lifthenelse(arg, ifso, ifnot) ->
      begin match close fenv cenv arg with
        (uarg, Value_const (Uconst_ptr n)) ->
          sequence_constant_expr arg uarg
            (close fenv cenv (if n = 0 then ifnot else ifso))
      | (uarg, _ ) ->
          let (uifso, _) = close fenv cenv ifso in
          let (uifnot, _) = close fenv cenv ifnot in
          (Uifthenelse(uarg, uifso, uifnot), Value_unknown)
      end
  | Lsequence(lam1, lam2) ->
      let (ulam1, _) = close fenv cenv lam1 in
      let (ulam2, approx) = close fenv cenv lam2 in
      (Usequence(ulam1, ulam2), approx)
  | Lwhile(cond, body) ->
      let (ucond, _) = close fenv cenv cond in
      let (ubody, _) = close fenv cenv body in
      (Uwhile(ucond, ubody), Value_unknown)
  | Lfor(id, lo, hi, dir, body) ->
      let (ulo, _) = close fenv cenv lo in
      let (uhi, _) = close fenv cenv hi in
      let (ubody, _) = close fenv cenv body in
      (Ufor(id, ulo, uhi, dir, ubody), Value_unknown)
  | Lassign(id, lam) ->
      let (ulam, _) = close fenv cenv lam in
      (Uassign(id, ulam), Value_unknown)
  | Levent(lam, ev) ->
      let (ulam, approx) = close fenv cenv lam in
      (add_debug_info ev ulam, approx)
  | Lifused _ ->
      assert false

and close_list fenv cenv = function
    [] -> []
  | lam :: rem ->
      let (ulam, _) = close fenv cenv lam in
      ulam :: close_list fenv cenv rem

and close_list_approx fenv cenv = function
    [] -> ([], [])
  | lam :: rem ->
      let (ulam, approx) = close fenv cenv lam in
      let (ulams, approxs) = close_list_approx fenv cenv rem in
      (ulam :: ulams, approx :: approxs)

and close_named fenv cenv id = function
    Lfunction(kind, params, body) as funct ->
      close_one_function fenv cenv id funct
  | lam ->
      close fenv cenv lam

(* Build a shared closure for a set of mutually recursive functions *)

and close_functions fenv cenv fun_defs =
  let fun_defs =
    List.flatten
      (List.map
         (function
           | (id, Lfunction(kind, params, body)) ->
               split_default_wrapper id kind params body
           | _ -> assert false
         )
         fun_defs)
  in

  (* Update and check nesting depth *)
  incr function_nesting_depth;
  let initially_closed =
    !function_nesting_depth < excessive_function_nesting_depth in
  (* Determine the free variables of the functions *)
  let fv =
    IdentSet.elements (free_variables (Lletrec(fun_defs, lambda_unit))) in
  (* Build the function descriptors for the functions.
     Initially all functions are assumed not to need their environment
     parameter. *)
  let uncurried_defs =
    List.map
      (function
          (id, Lfunction(kind, params, body)) ->
            let label = Compilenv.make_symbol (Some (Ident.unique_name id)) in
            let arity = List.length params in
            let fundesc =
              {fun_label = label;
               fun_arity = (if kind = Tupled then -arity else arity);
               fun_closed = initially_closed;
               fun_inline = None;
               fun_float_const_prop = !Clflags.float_const_prop } in
            (id, params, body, fundesc)
        | (_, _) -> fatal_error "Closure.close_functions")
      fun_defs in
  (* Build an approximate fenv for compiling the functions *)
  let fenv_rec =
    List.fold_right
      (fun (id, params, body, fundesc) fenv ->
        Tbl.add id (Value_closure(fundesc, Value_unknown)) fenv)
      uncurried_defs fenv in
  (* Determine the offsets of each function's closure in the shared block *)
  let env_pos = ref (-1) in
  let clos_offsets =
    List.map
      (fun (id, params, body, fundesc) ->
        let pos = !env_pos + 1 in
        env_pos := !env_pos + 1 + (if fundesc.fun_arity <> 1 then 3 else 2);
        pos)
      uncurried_defs in
  let fv_pos = !env_pos in
  (* This reference will be set to false if the hypothesis that a function
     does not use its environment parameter is invalidated. *)
  let useless_env = ref initially_closed in
  (* Translate each function definition *)
  let clos_fundef (id, params, body, fundesc) env_pos =
    let dbg = match body with
      | Levent (_,({lev_kind=Lev_function} as ev)) -> Debuginfo.from_call ev
      | _ -> Debuginfo.none in
    let env_param = Ident.create "env" in
    let cenv_fv =
      build_closure_env env_param (fv_pos - env_pos) fv in
    let cenv_body =
      List.fold_right2
        (fun (id, params, body, fundesc) pos env ->
          Tbl.add id (Uoffset(Uvar env_param, pos - env_pos)) env)
        uncurried_defs clos_offsets cenv_fv in
    let (ubody, approx) = close fenv_rec cenv_body body in
    if !useless_env && occurs_var env_param ubody then raise NotClosed;
    let fun_params = if !useless_env then params else params @ [env_param] in
    let f =
      {
        label  = fundesc.fun_label;
        arity  = fundesc.fun_arity;
        params = fun_params;
        body   = ubody;
        dbg;
      }
    in
    (* give more chance of function with default parameters (i.e.
       their wrapper functions) to be inlined *)
    let n =
      List.fold_left
        (fun n id -> n + if Ident.name id = "*opt*" then 8 else 1)
        0
        fun_params
    in
    if lambda_smaller ubody
        (!Clflags.inline_threshold + n)
    then fundesc.fun_inline <- Some(fun_params, ubody);

    (f, (id, env_pos, Value_closure(fundesc, approx))) in
  (* Translate all function definitions. *)
  let clos_info_list =
    if initially_closed then begin
      let snap = Compilenv.snapshot () in
      try List.map2 clos_fundef uncurried_defs clos_offsets
      with NotClosed ->
      (* If the hypothesis that the environment parameters are useless has been
         invalidated, then set [fun_closed] to false in all descriptions and
         recompile *)
        Compilenv.backtrack snap; (* PR#6337 *)
        List.iter
          (fun (id, params, body, fundesc) ->
             fundesc.fun_closed <- false;
             fundesc.fun_inline <- None;
          )
          uncurried_defs;
        useless_env := false;
        List.map2 clos_fundef uncurried_defs clos_offsets
    end else
      (* Excessive closure nesting: assume environment parameter is used *)
        List.map2 clos_fundef uncurried_defs clos_offsets
    in
  (* Update nesting depth *)
  decr function_nesting_depth;
  (* Return the Uclosure node and the list of all identifiers defined,
     with offsets and approximations. *)
  let (clos, infos) = List.split clos_info_list in
  let fv = if !useless_env then [] else fv in
  (Uclosure(clos, List.map (close_var fenv cenv) fv), infos)

(* Same, for one non-recursive function *)

and close_one_function fenv cenv id funct =
  match close_functions fenv cenv [id, funct] with
  | (clos, (i, _, approx) :: _) when id = i -> (clos, approx)
  | _ -> fatal_error "Closure.close_one_function"

(* Close a switch *)

and close_switch arg fenv cenv cases num_keys default =
  let ncases = List.length cases in
  let index = Array.make num_keys 0
  and store = Storer.mk_store () in

  (* First default case *)
  begin match default with
  | Some def when ncases < num_keys ->
      assert (store.act_store def = 0)
  | _ -> ()
  end ;
  (* Then all other cases *)
  List.iter
    (fun (key,lam) ->
     index.(key) <- store.act_store lam)
    cases ;

  (*  Explicit sharing with catch/exit, as switcher compilation may
      later unshare *)
  let acts = store.act_get_shared () in
  let hs = ref (fun e -> e) in

  (* Compile actions *)
  let actions =
    Array.map
      (function
        | Single lam|Shared (Lstaticraise (_,[]) as lam) ->
            let ulam,_ = close fenv cenv lam in
            ulam
        | Shared lam ->
            let ulam,_ = close fenv cenv lam in
            let i = next_raise_count () in
(*
            let string_of_lambda e =
              Printlambda.lambda Format.str_formatter e ;
              Format.flush_str_formatter () in
            Printf.eprintf "SHARE CLOSURE %i [%s]\n%s\n" i
                (string_of_lambda arg)
                (string_of_lambda lam) ;
*)
            let ohs = !hs in
            hs := (fun e -> Ucatch (i,[],ohs e,ulam)) ;
            Ustaticfail (i,[]))
      acts in
  match actions with
  | [| |] -> [| |], [| |], !hs (* May happen when default is None *)
  | _     -> index, actions, !hs


(* Collect exported symbols for structured constants *)

let collect_exported_structured_constants a =
  let rec approx = function
    | Value_closure (fd, a) ->
        approx a;
        begin match fd.fun_inline with
        | Some (_, u) -> ulam u
        | None -> ()
        end
    | Value_tuple a -> Array.iter approx a
    | Value_const c -> const c
    | Value_unknown | Value_global_field _ -> ()
  and const = function
    | Uconst_ref (s, c) ->
        Compilenv.add_exported_constant s;
        structured_constant c
    | Uconst_int _ | Uconst_ptr _ -> ()
  and structured_constant = function
    | Uconst_block (_, ul) -> List.iter const ul
    | Uconst_float _ | Uconst_int32 _
    | Uconst_int64 _ | Uconst_nativeint _
    | Uconst_float_array _ | Uconst_string _ -> ()
  and ulam = function
    | Uvar _ -> ()
    | Uconst c -> const c
    | Udirect_apply (_, ul, _) -> List.iter ulam ul
    | Ugeneric_apply (u, ul, _) -> ulam u; List.iter ulam ul
    | Uclosure (fl, ul) ->
        List.iter (fun f -> ulam f.body) fl;
        List.iter ulam ul
    | Uoffset(u, _) -> ulam u
    | Ulet (_, u1, u2) -> ulam u1; ulam u2
    | Uletrec (l, u) -> List.iter (fun (_, u) -> ulam u) l; ulam u
    | Uprim (_, ul, _) -> List.iter ulam ul
    | Uswitch (u, sl) ->
        ulam u;
        Array.iter ulam sl.us_actions_consts;
        Array.iter ulam sl.us_actions_blocks
    | Ustringswitch (u,sw,d) ->
        ulam u ;
        List.iter (fun (_,act) -> ulam act) sw ;
        Misc.may ulam d
    | Ustaticfail (_, ul) -> List.iter ulam ul
    | Ucatch (_, _, u1, u2)
    | Utrywith (u1, _, u2)
    | Usequence (u1, u2)
    | Uwhile (u1, u2)  -> ulam u1; ulam u2
    | Uifthenelse (u1, u2, u3)
    | Ufor (_, u1, u2, _, u3) -> ulam u1; ulam u2; ulam u3
    | Uassign (_, u) -> ulam u
    | Usend (_, u1, u2, ul, _) -> ulam u1; ulam u2; List.iter ulam ul
  in
  approx a

let reset () =
  global_approx := [||];
  function_nesting_depth := 0

(* The entry point *)

let intro size lam =
  reset ();
  let id = Compilenv.make_symbol None in
  global_approx := Array.init size (fun i -> Value_global_field (id, i));
  Compilenv.set_global_approx(Value_tuple !global_approx);
  let (ulam, approx) = close Tbl.empty Tbl.empty lam in
  if !Clflags.opaque
  then Compilenv.set_global_approx(Value_unknown)
  else collect_exported_structured_constants (Value_tuple !global_approx);
  global_approx := [||];
  ulam
