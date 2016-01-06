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



let string_of_lambda = Format.asprintf "%a" Printlambda.lambda 

let string_of_primitive = Format.asprintf "%a" Printlambda.primitive

let rec no_side_effects (lam : Lambda.lambda) : bool = 
  match lam with 
  | Lvar _ 
  | Lconst _ 
  | Lfunction _ -> true
  | Lprim (primitive, args) -> 
    List.for_all no_side_effects args && 
    (
      match primitive with 
      | Pccall {prim_name ; _} ->
        begin 
          match prim_name,args with 
          | ("caml_register_named_value"
            (* register to c runtime does not make sense  in ocaml *)
            | "caml_set_oo_id" 
            | "caml_is_js"
            | "caml_int64_float_of_bits" (* more safe to check if arguments are constant *)
            (* non-observable side effect *)    
            | "caml_sys_get_config"
            | "caml_sys_get_argv" (* should be fine *)

            | "caml_create_string" (* TODO: add more *)
            | "caml_make_vect"
            | "caml_obj_dup"
            | "caml_obj_block"
            ), _  -> true 
          | "caml_ml_open_descriptor_in", [Lconst (Const_base (Const_int 0))] -> true 
          | "caml_ml_open_descriptor_out", 
            [Lconst (Const_base (Const_int (1|2))) ]
            -> true
          (* we can not mark it pure
             only when we guarantee this exception is caught...
           *)
          | _ , _-> false
        end 

      | Pidentity 
      | Pbytes_to_string 
      | Pbytes_of_string 
      | Pchar_to_int (* might throw .. *)
      | Pchar_of_int  
      | Pignore 
      | Prevapply _
      | Pdirapply _
      | Ploc _

      | Pgetglobal _ 
      | Pmakeblock _  (* whether it's mutable or not *)
      | Pfield _
      | Pfloatfield _ 
      | Pduprecord _ 
      (* Boolean operations *)
      | Psequand | Psequor | Pnot
      (* Integer operations *)
      | Pnegint | Paddint | Psubint | Pmulint | Pdivint | Pmodint
      | Pandint | Porint | Pxorint
      | Plslint | Plsrint | Pasrint
      | Pintcomp _ 
      (* Float operations *)
      | Pintoffloat | Pfloatofint
      | Pnegfloat | Pabsfloat
      | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat
      | Pfloatcomp _ 
      (* String operations *)
      | Pstringlength 
      | Pstringrefu 
      | Pstringrefs
      | Pbyteslength
      | Pbytesrefu
      | Pbytesrefs
      | Pmakearray _ 
      | Parraylength _ 
      | Parrayrefu _
      | Parrayrefs _ 
      (* Test if the argument is a block or an immediate integer *)
      | Pisint
      (* Test if the (integer) argument is outside an interval *)
      | Pisout
      | Pbintofint _
      | Pintofbint _
      | Pcvtbint _
      | Pnegbint _
      | Paddbint _
      | Psubbint _
      | Pmulbint _
      | Pdivbint _
      | Pmodbint _
      | Pandbint _
      | Porbint _
      | Pxorbint _
      | Plslbint _
      | Plsrbint _
      | Pasrbint _
      | Pbintcomp _
      (* Operations on big arrays: (unsafe, #dimensions, kind, layout) *)
      | Pbigarrayref _ (* TODO it may raise an exception....*)
      (* Compile time constants *)
      | Pctconst _
      (* Integer to external pointer *)
      | Pint_as_pointer
      | Poffsetint _
        -> true
      | Pstringsetu
      | Pstringsets
      | Pbytessetu 
      | Pbytessets
      (* Bitvect operations *)
      | Pbittest
      (* Operations on boxed integers (Nativeint.t, Int32.t, Int64.t) *)
      | Parraysets _
      | Pbigarrayset _
      (* size of the nth dimension of a big array *)
      | Pbigarraydim _
      (* load/set 16,32,64 bits from a string: (unsafe)*)
      | Pstring_load_16 _
      | Pstring_load_32 _
      | Pstring_load_64 _
      | Pstring_set_16 _
      | Pstring_set_32 _
      | Pstring_set_64 _
      (* load/set 16,32,64 bits from a
         (char, int8_unsigned_elt, c_layout) Bigarray.Array1.t : (unsafe) *)
      | Pbigstring_load_16 _
      | Pbigstring_load_32 _
      | Pbigstring_load_64 _
      | Pbigstring_set_16 _
      | Pbigstring_set_32 _
      | Pbigstring_set_64 _
      (* byte swap *)
      | Pbswap16
      | Pbbswap _
      | Parraysetu _ 
      | Poffsetref _ 
      | Praise _ 
      | Plazyforce 
      | Pmark_ocaml_object  (* TODO*)
      | Psetfield _ 
      | Psetfloatfield _
      | Psetglobal _ -> false 
    )
  | Llet (_,_, arg,body) -> no_side_effects arg && no_side_effects body 
  | Lswitch (_,_) -> false 
  | Lstringswitch (_,_,_) -> false
  | Lstaticraise _ -> false
  | Lstaticcatch _ -> false 

  (* | "caml_sys_getenv" , [Lconst(Const_base(Const_string _))] *)
  (*         -> true *)
  (** not enough, we need know that 
      if it [Not_found], there are no other exceptions 
      can be thrown
  *)
  | Ltrywith (Lprim(Pccall{prim_name = "caml_sys_getenv"}, 
                    [Lconst _]),exn,
              Lifthenelse(Lprim(_, [Lvar exn1; 
                                    Lprim(Pgetglobal ({name="Not_found"}),[])]),
                          then_, _)) when Ident.same exn1 exn
    (** we might put this in an optimization pass 
        also make sure when we wrap this in [js] we 
        should follow the same patten, raise [Not_found] 
    *)
    -> no_side_effects then_
  (** It would be nice that we can also analysis some small functions 
      for example [String.contains], 
      [Format.make_queue_elem]
  *)
  | Ltrywith (body,exn,handler) 
    -> no_side_effects body && no_side_effects handler

  | Lifthenelse  (a,b,c) -> 
    no_side_effects a && no_side_effects b && no_side_effects c
  | Lsequence (e0,e1) -> no_side_effects e0 && no_side_effects e1 
  | Lwhile (a,b) -> no_side_effects a && no_side_effects b 
  | Lfor _ -> false 
  | Lassign _ -> false (* actually it depends ... *)
  | Lsend _ -> false 
  | Levent (e,_) -> no_side_effects e 
  | Lifused _ -> false 
  | Lapply _ -> false (* we need purity analysis .. *)
  | Lletrec (bindings, body) ->
    List.for_all (fun (_,b) -> no_side_effects b) bindings && no_side_effects body

(* TODO: not very efficient .. *)
exception Cyclic 
      
let toplogical (get_deps : Ident.t -> Ident_set.t) (libs : Ident.t list) : Ident.t list =
  let rec aux acc later todo round_progress =
    match todo, later with
    | [], [] ->  acc
    | [], _ ->
        if round_progress
        then aux acc todo later false
        else raise Cyclic
    | x::xs, _ ->
        if Ident_set.for_all (fun dep -> x == dep || List.mem dep acc) (get_deps x)
        then aux (x::acc) later xs true
        else aux acc (x::later) xs round_progress
  in
  let starts, todo = List.partition (fun lib -> Ident_set.is_empty @@ get_deps lib) libs in
  aux starts [] todo false

let sort_dag_args  param_args =
  let todos = Ident_map.keys param_args  in
  let idents = Ident_set.of_list  todos in
  let dependencies  : Ident_set.t Ident_map.t = 
    Ident_map.mapi (fun param arg -> Js_fold_basic.depends_j arg idents) param_args in
  try  
    Some (toplogical (fun k -> Ident_map.find k dependencies) todos)
  with Cyclic -> None 

(* 
    Estimate the size of lambda for better inlining 
    threshold is 1000 - so that we 
 *)
exception Too_big_to_inline

let really_big () = raise Too_big_to_inline

let big_lambda = 1000

let rec size (lam : Lambda.lambda) = 
  try 
    match lam with 
    | Lvar _ ->  1
    | Lconst _ -> 1 (* Modify *)
    | Llet(_, _, l1, l2) -> 1 + size l1 + size l2 
    | Lletrec _ -> really_big ()
    | Lprim(_, ll) -> size_lams 1 ll

    (** complicated 
        1. inline this function
        2. ...
        exports.Make=
        function(funarg)
        {var $$let=Make(funarg);
        return [0, $$let[5],... $$let[16]]}
     *)      
    | Lapply(f,
             args, _) -> size_lams (size f) args
    | Lfunction(_, params, l) -> really_big ()
    | Lswitch(_, _) -> really_big ()
    | Lstringswitch(_,_,_) -> really_big ()
    | Lstaticraise (i,ls) -> 
        List.fold_left (fun acc x -> size x + acc) 1 ls 
    | Lstaticcatch(l1, (i,x), l2) -> really_big () 
    | Ltrywith(l1, v, l2) -> really_big ()
    | Lifthenelse(l1, l2, l3) -> 1 + size  l1 + size  l2 +  size  l3
    | Lsequence(l1, l2) -> size  l1  +  size  l2
    | Lwhile(l1, l2) -> really_big ()
    | Lfor(flag, l1, l2, dir, l3) -> really_big () 
    | Lassign (_,v) -> 1 + size v  (* This is side effectful,  be careful *)
    | Lsend _  ->  really_big ()
    | Levent(l, _) -> size l 
    | Lifused(v, l) -> size l 
  with Too_big_to_inline ->  1000 

and size_lams acc (lams : Lambda.lambda list) = 
  List.fold_left (fun acc l -> acc  + size l ) acc lams

(* compared two lambdas in case analysis, note that we only compare some small lambdas
    Actually this patten is quite common in GADT, people have to write duplicated code 
    due to the type system restriction
*)
let rec eq_lambda (l1 : Lambda.lambda) (l2 : Lambda.lambda) =
  match (l1, l2) with
  | Lvar i1, Lvar i2 -> Ident.same i1 i2
  | Lconst c1, Lconst c2 -> c1 = c2 (* *)
  | Lapply (l1,args1,_), Lapply(l2,args2,_) ->
    eq_lambda l1 l2  && List.for_all2 eq_lambda args1 args2
  | Lfunction _ , Lfunction _ -> false (* TODO -- simple functions ?*)
  | Lassign(v0,l0), Lassign(v1,l1) -> Ident.same v0 v1 && eq_lambda l0 l1
  | Lstaticraise(id,ls), Lstaticraise(id1,ls1) -> 
    id = id1 && List.for_all2 eq_lambda ls ls1 
  | Llet (_,_,_,_), Llet (_,_,_,_) -> false 
  | Lletrec _, Lletrec _ -> false 
  | Lprim (p,ls), Lprim (p1,ls1) -> 
    eq_primitive p p1 && List.for_all2 eq_lambda ls ls1
  | Lswitch _, Lswitch _ -> false  
  | Lstringswitch _ , Lstringswitch _ -> false 
  | Lstaticcatch _, Lstaticcatch _ -> false 
  | Ltrywith _, Ltrywith _ -> false 
  | Lifthenelse (a,b,c), Lifthenelse (a0,b0,c0) ->
    eq_lambda a a0 && eq_lambda b b0 && eq_lambda c c0
  | Lsequence (a,b), Lsequence (a0,b0) ->
    eq_lambda a a0 && eq_lambda b b0
  | Lwhile (p,b) , Lwhile (p0,b0) -> eq_lambda p p0 && eq_lambda b b0
  | Lfor (_,_,_,_,_), Lfor (_,_,_,_,_) -> false
  | Lsend _, Lsend _ -> false
  | Levent (v,_), Levent (v0,_) -> eq_lambda v v0
  | Lifused _, Lifused _ -> false 
  |  _,  _ -> false 
and eq_primitive (p : Lambda.primitive) (p1 : Lambda.primitive) = 
  match p, p1 with 
  | Pccall {prim_name = n0},  Pccall {prim_name = n1} -> n0 = n1 
  | _ , _ -> 
    (* FIXME: relies on structure equality *) 
    try p = p1 with _ -> false

let add_required_module (x : Ident.t) (meta : Lam_stats.meta) = 
  meta.required_modules <- Lam_module_ident.of_ml x :: meta.required_modules 

let add_required_modules ( x : Ident.t list) (meta : Lam_stats.meta) = 
  meta.required_modules <- List.map (fun x ->  Lam_module_ident.of_ml x)  x
    @ meta.required_modules

(* Apply a substitution to a lambda-term.
   Assumes that the bound variables of the lambda-term do not
   belong to the domain of the substitution.
   Assumes that the image of the substitution is out of reach
   of the bound variables of the lambda-term (no capture). *)

let subst_lambda s lam =
  let rec subst (x : Lambda.lambda) =
    match x with 
    | Lvar id as l ->
      begin 
        try Ident_map.find id s with Not_found -> l 
      end
    | Lconst sc as l -> l
    | Lapply(fn, args, loc) -> Lapply(subst fn, List.map subst args, loc)
    | Lfunction(kind, params, body) -> Lfunction(kind, params, subst body)
    | Llet(str, id, arg, body) -> Llet(str, id, subst arg, subst body)
    | Lletrec(decl, body) -> Lletrec(List.map subst_decl decl, subst body)
    | Lprim(p, args) -> Lprim(p, List.map subst args)
    | Lswitch(arg, sw) ->
      Lswitch(subst arg,
              {sw with sw_consts = List.map subst_case sw.sw_consts;
                       sw_blocks = List.map subst_case sw.sw_blocks;
                       sw_failaction = subst_opt  sw.sw_failaction; })
    | Lstringswitch (arg,cases,default) ->
      Lstringswitch
        (subst arg,List.map subst_strcase cases,subst_opt default)
    | Lstaticraise (i,args) ->  Lstaticraise (i, List.map subst args)
    | Lstaticcatch(e1, io, e2) -> Lstaticcatch(subst e1, io, subst e2)
    | Ltrywith(e1, exn, e2) -> Ltrywith(subst e1, exn, subst e2)
    | Lifthenelse(e1, e2, e3) -> Lifthenelse(subst e1, subst e2, subst e3)
    | Lsequence(e1, e2) -> Lsequence(subst e1, subst e2)
    | Lwhile(e1, e2) -> Lwhile(subst e1, subst e2)
    | Lfor(v, e1, e2, dir, e3) -> Lfor(v, subst e1, subst e2, dir, subst e3)
    | Lassign(id, e) -> Lassign(id, subst e)
    | Lsend (k, met, obj, args, loc) ->
      Lsend (k, subst met, subst obj, List.map subst args, loc)
    | Levent (lam, evt) -> Levent (subst lam, evt)
    | Lifused (v, e) -> Lifused (v, subst e)
  and subst_decl (id, exp) = (id, subst exp)
  and subst_case (key, case) = (key, subst case)
  and subst_strcase (key, case) = (key, subst case)
  and subst_opt = function
    | None -> None
    | Some e -> Some (subst e)
  in subst lam

(* 
    It's impossible to have a case like below:
   {[
     (let export_f = ... in export_f)
   ]}
    Even so, it's still correct
*)
let refine_let
    ?kind param
    (arg : Lambda.lambda) (l : Lambda.lambda)  : Lambda.lambda =

  match (kind : Lambda.let_kind option), arg, l  with 
  | _, _, Lvar w when Ident.same w param (* let k = xx in k *)
    -> arg (* TODO: optimize here -- it's safe to do substitution here *)
  | _, _, Lprim (fn, [Lvar w]) when Ident.same w param 
                                 &&  (function | Lambda.Pmakeblock _ -> false | _ ->  true) fn
    (* don't inline inside a block *)
    ->  Lprim(fn, [arg])
  (* we can not do this substitution when capttured *)
  (* | _, Lvar _, _ -> (\** let u = h in xxx*\) *)
  (*     (\* assert false *\) *)
  (*     Ext_log.err "@[substitution >> @]@."; *)
  (*     let v= subst_lambda (Ident_map.singleton param arg ) l in *)
  (*     Ext_log.err "@[substitution << @]@."; *)
  (* v *)
  | _, _, Lapply (fn, [Lvar w],info) when Ident.same w param -> 
    (** does not work for multiple args since 
        evaluation order unspecified, does not apply 
        for [js] in general, since the scope of js ir is loosen

        here we remove the definition of [param]
    *)
    Lapply(fn, [arg], info)
  | (Some (Strict | StrictOpt ) | None ),
    ( Lvar _    | Lconst  _ | Lprim (Pfield _ , [Lprim (Pgetglobal _ , [])])) , _ ->
    (* (match arg with  *)
    (* | Lconst _ ->  *)
    (*     Ext_log.err "@[%a %s@]@."  *)
    (*       Ident.print param (string_of_lambda arg) *)
    (* | _ -> ()); *)
    (* No side effect and does not depend on store,
        since function evaluation is always delayed
    *)
    Llet(Alias, param,arg, l)
  | (Some (Strict | StrictOpt ) | None ), (Lfunction _ ), _ ->
    (*It can be promoted to [Alias], however, 
        we don't want to do this, since we don't want the 
        function to be inlined to a block, for example
        {[
          let f = fun _ -> 1 in
          [0, f]
        ]}
        TODO: punish inliner to inline functions 
        into a block 
    *)
    Llet(StrictOpt, param,arg, l)
  (* Not the case, the block itself can have side effects 
      we can apply [no_side_effects] pass 
      | Some Strict, Lprim(Pmakeblock (_,_,Immutable),_) ->  
        Llet(StrictOpt, param, arg, l) 
  *)      
  | Some Strict, _ ,_  when no_side_effects arg ->
    Llet(StrictOpt, param, arg,l)
  | Some Variable, _, _ -> 
    Llet(Variable, param,arg,l) 
  | Some kind, _, _ -> 
    Llet(kind, param,arg,l) 
  | None , _, _ -> 
    Llet(Strict, param, arg , l)

let alias (meta : Lam_stats.meta) (k:Ident.t) (v:Ident.t) 
    (v_kind : Lam_stats.kind) (let_kind : Lambda.let_kind) =
  (** treat rec as Strict, k is assigned to v 
      {[ let k = v ]}
  *)
  begin 
    match v_kind with 
    | NA ->
      begin 
        match Hashtbl.find meta.ident_tbl v  with 
        | exception Not_found -> ()
        | ident_info -> Hashtbl.add meta.ident_tbl k ident_info
      end
    | ident_info -> Hashtbl.add meta.ident_tbl k ident_info
  end ;
  (* share -- it is safe to share most properties,
      for arity, we might be careful, only [Alias] can share,
      since two values have same type, can have different arities
      TODO: check with reference pass, it might break 
      since it will create new identifier, we can avoid such issue??

      actually arity is a dynamic property, for a reference, it can 
      be changed across 
      we should treat
      reference specially. or maybe we should track any 
      mutable reference
  *)
  begin match let_kind with 
    | Alias -> 
      if not @@ List.mem k meta.export_idents 
      then
        Hashtbl.add meta.alias_tbl k v 
    (** For [export_idents], we don't want to do such simplification
        if we do substitution, then it will affect exports...
    *)
    | Strict | StrictOpt(*can discard but not be substitued *) | Variable  -> ()
  end

let beta_reduce params body args =
  List.fold_left2 
    (fun l param arg ->
       refine_let param arg l)
    body params args

type group = 
  | Single of Lambda.let_kind  * Ident.t * Lambda.lambda
  | Recursive of (Ident.t * Lambda.lambda) list
  | Nop of Lambda.lambda 

let pp = Format.fprintf 

let str_of_kind (kind : Lambda.let_kind) = 
  match kind with 
  | Alias -> "a"
  | Strict -> ""
  | StrictOpt -> "o"
  | Variable -> "v" 

let pp_group env fmt ( x : group) =
  match x with
  | Single (kind, id, lam) ->
    Format.fprintf fmt "@[let@ %a@ =%s@ @[<hv>%a@]@ @]" Ident.print id (str_of_kind kind) 
      (Printlambda.env_lambda env) lam
  | Recursive lst -> 
    List.iter (fun (id,lam) -> 
        Format.fprintf fmt
          "@[let %a@ =r@ %a@ @]" Ident.print id (Printlambda.env_lambda env) lam
      ) lst
  | Nop lam -> Printlambda.env_lambda env fmt lam

(* How we destruct the immutable block 
   depend on the block name itself, 
   good hints to do aggressive destructing
   1. the variable is not exported
      like [matched] -- these are blocks constructed temporary
   2. how the variable is used 
      if it is guarateed to be 
      - non export 
      - and non escaped (there is no place it is used as a whole)
      then we can always destruct it 
      if some fields are used in multiple places, we can create 
      a temporary field 

   3. It would be nice that when the block is mutable, its 
       mutable fields are explicit
*)

let element_of_lambda (lam : Lambda.lambda) : Lam_stats.element = 
  match lam with 
  | Lvar _ 
  | Lconst _ 
  | Lprim (Pfield _ , [ Lprim (Pgetglobal _, [])]) -> SimpleForm lam
  (* | Lfunction _  *)
  | _ -> NA 

let kind_of_lambda_block (xs : Lambda.lambda list) : Lam_stats.kind = 
  xs 
  |> List.map element_of_lambda 
  |> (fun ls -> Lam_stats.ImmutableBlock (Array.of_list  ls))

let get lam v i tbl : Lambda.lambda =
  match (Hashtbl.find tbl v  : Lam_stats.kind) with 
  | Module g -> 
    Lprim (Pfield i, [Lprim(Pgetglobal g, [])])
  | ImmutableBlock arr -> 
    begin match arr.(i) with 
      | NA -> lam 
      | SimpleForm l -> l
    end
  | Constant (Const_block (_,_,ls)) -> 
    Lconst (List.nth  ls i)
  | _ -> lam
  | exception Not_found -> lam 

let rec flatten 
    (acc :  group list ) 
    (lam : Lambda.lambda) :  Lambda.lambda *  group list = 
  match lam with 
  | Levent (e,_) -> flatten acc e (* TODO: We stripped event in the beginning*)
  | Llet (str,id,arg,body) -> 
    let (res,l) = flatten acc arg  in
    flatten (Single(str, id, res ) :: l) body
  (* begin *)
  (*   match res with *)
  (*   | Llet _ -> assert false *)
  (*   | Lletrec _-> assert false *)
  (*   | Levent _ -> assert false *)
  (*   | _ ->  *)
  (*       Format.fprintf  Format.err_formatter "%a@." Printlambda.lambda res ; *)
  (*       Format.pp_print_flush Format.err_formatter (); *)
  (*       flatten (Single(str, id, res ) :: l) body *)
  (* end *)
  | Lletrec (bind_args, body) -> 
    (** TODO: more flattening, 
        - also for function compilation, flattening should be done first
        - [compile_group] and [compile] become mutually recursive function
    *)
    (* Printlambda.lambda Format.err_formatter lam ; assert false  *)
    flatten
      (
        Recursive
          (List.map (fun (id, arg ) -> (id, arg)) bind_args)
        :: acc
      )
      body
  | Lsequence (l,r) -> 
    let (res, l)  = flatten acc l in
    flatten (Nop res :: l)  r

  | x ->  
    (*   x = Llet _ -> assert false (* sane check *)*)
    x, acc

(* [groups] are in reverse order *)

let lambda_of_groups result groups = 
  List.fold_left (fun acc x -> 
      match x with 
      | Nop l -> Lambda.Lsequence(l,acc)
      | Single(kind,ident,lam) -> refine_let ~kind ident lam acc
      | Recursive bindings -> Lletrec (bindings,acc)) 
    result groups


(* TODO: 
    refine effectful [ket_kind] to be pure or not
    Be careful of how [Lifused(v,l)] work 
    since its semantics depend on whether v is used or not
    return value are in reverse order, but handled by [lambda_of_groups]
*)
let deep_flatten
    (lam : Lambda.lambda) :  Lambda.lambda  = 
  let rec
    flatten 
      (acc :  group list ) 
      (lam : Lambda.lambda) :  Lambda.lambda *  group list = 
    match lam with 
    | Levent (e,_) -> flatten acc e (* TODO: We stripped event in the beginning*)
    | Llet (str,id,arg,body) -> 
      let (res,l) = flatten acc arg  in
      flatten (Single(str, id, res ) :: l) body
    | Lletrec (bind_args, body) -> 
      (** TODO: more flattening, 
          - also for function compilation, flattening should be done first
          - [compile_group] and [compile] become mutually recursive function
      *)
      (* Printlambda.lambda Format.err_formatter lam ; assert false  *)
      flatten
        (
          (* let rec iter bind_args acc =  *)
          (*   match bind_args with *)
          (*   | [] ->  acc  *)
          (*   | (id,arg) :: rest ->  *)
          (*       flatten acc  *)
          Recursive
            (List.map (fun (id, arg ) -> (id, aux arg)) bind_args)
          :: acc
        )
        body
    | Lsequence (l,r) -> 
      let (res, l)  = flatten acc l in
      flatten (Nop res :: l)  r
    | x ->  
      aux x, acc      

  and aux  (lam : Lambda.lambda) : Lambda.lambda= 
    match lam with 
    | Levent (e,_) -> aux  e (* TODO: We stripped event in the beginning*)
    | Llet _ -> 
      let res, groups = flatten [] lam  
      in lambda_of_groups res groups
    | Lletrec (bind_args, body) ->  
      (** be careful to flatten letrec 
          like below : 
          {[
            let rec even = 
              let odd n =  if n ==1 then true else even (n - 1) in
              fun n -> if n ==0  then true else odd (n - 1)
          ]}
          odd and even are recursive values, since all definitions inside 
          e.g, [odd] can see [even] now, however, it should be fine
          in our case? since ocaml's recursive value does not allow immediate 
          access its value direclty?, seems no
          {[
            let rec even2 = 
              let odd = even2 in
              fun n -> if n ==0  then true else odd (n - 1)
          ]}
      *)
      let module Ident_set = Lambda.IdentSet in
      let rec iter bind_args acc =
        match bind_args with
        | [] ->   acc
        | (id,arg) :: rest ->
          let groups, set = acc in
          let res, groups = flatten groups (aux arg)
          in
          iter rest (Recursive [(id,res)] :: groups, Ident_set.add id set) 
      in
      let groups, collections = iter bind_args ([], Ident_set.empty) in
      (* FIXME:
          here we try to move inner definitions of [recurisve value] upwards
          for example:
         {[
           let rec x = 
             let y = 32 in
             y :: x
           and z = ..
             ---
             le ty = 32 in
           let rec x = y::x
           and z = ..
         ]}
          however, the inner definitions can see [z] and [x], so we
          can not blindly move it in the beginning, however, for 
          recursive value, ocaml does not allow immediate access to 
          recursive value, so what's the best strategy?
          ---
          the motivation is to capture real tail call
      *)
      let (result, _, wrap) = 
        List.fold_left (fun  (acc, set, wrap)  g -> 
            match g with 
            | Recursive [ id, (Lconst _)]
            | Single (Alias, id, ( Lconst _   ))
            | Single ((Alias | Strict | StrictOpt), id, ( Lfunction _ )) -> 
              (** FIXME: 
                   It should be alias and alias will be optimized away
                   in later optmizations, however, 
                   this means if we don't optimize 
                  {[ let u/a = v in ..]}
                   the output would be wrong, we should *optimize 
                   this away right now* instead of delaying it to the 
                   later passes
              *)
              (acc, set, g :: wrap)

            | Single (_, id, ( Lvar bid)) -> 
              (acc, (if Ident_set.mem bid set then Ident_set.add id set else set ), g:: wrap)
            | Single (_, id, lam) ->
              let variables = Lambda.free_variables lam in
              if Ident_set.(is_empty (inter variables collections)) 
              then 
                (acc, set, g :: wrap )
              else 
                ((id, lam ) :: acc , Ident_set.add id set, wrap)
            | Recursive us -> 
              (* could also be from nested [let rec] 
                 like 
                 {[
                   let rec x = 
                     let rec y = 1 :: y in
                     2:: List.hd y:: x 
                 ]}
                 TODO: seems like we should update depenency graph, 

              *)
              (us @ acc , 
               List.fold_left (fun acc (id,_) -> Ident_set.add id acc) set us , 
               wrap)
            | Nop _ -> assert false 
          ) ([], collections, []) groups in
      lambda_of_groups 
        (Lletrec (
            result 
            (* List.map (fun (id,lam) -> (id, aux lam )) bind_args *), 
            aux body)) (List.rev wrap)
    | Lsequence (l,r) -> Lsequence(aux l, aux r)
    | Lconst _ -> lam
    | Lvar _ -> lam 
    (* | Lapply(Lfunction(Curried, params, body), args, _) *)
    (*   when  List.length params = List.length args -> *)
    (*     aux (beta_reduce  params body args) *)
    (* | Lapply(Lfunction(Tupled, params, body), [Lprim(Pmakeblock _, args)], _) *)
    (*     (\** TODO: keep track of this parameter in ocaml trunk, *)
    (*           can we switch to the tupled backend? *\) *)
    (*   when  List.length params = List.length args -> *)
    (*       aux (beta_reduce params body args) *)

    | Lapply(l1, ll, info) -> Lapply(aux l1,List.map aux ll, info)

    (* This kind of simple optimizations should be done each time
       and as early as possible *) 

    | Lprim(Pidentity, [l]) -> l 
    | Lprim(prim, ll) -> Lprim(prim, List.map aux  ll)
    | Lfunction(kind, params, l) -> Lfunction (kind, params , aux  l)
    | Lswitch(l, {sw_failaction; 
                  sw_consts; 
                  sw_blocks;
                  sw_numblocks;
                  sw_numconsts;
                 }) ->
      Lswitch(aux  l,
              {sw_consts = 
                 List.map (fun (v, l) -> v, aux  l) sw_consts;
               sw_blocks = List.map (fun (v, l) -> v, aux  l) sw_blocks;
               sw_numconsts = sw_numconsts;
               sw_numblocks = sw_numblocks;
               sw_failaction = 
                 begin 
                   match sw_failaction with 
                   | None -> None
                   | Some x -> Some (aux x)
                 end})
    | Lstringswitch(l, sw, d) ->
      Lstringswitch(aux  l ,
                    List.map (fun (i, l) -> i,aux  l) sw,
                    begin 
                      match d with
                      | Some d -> Some (aux d )
                      | None -> None
                    end)
    | Lstaticraise (i,ls) -> Lstaticraise(i, List.map (aux ) ls)
    | Lstaticcatch(l1, (i,x), l2) -> Lstaticcatch(aux  l1, (i,x), aux  l2)
    | Ltrywith(l1, v, l2) -> Ltrywith(aux  l1,v, aux  l2)
    | Lifthenelse(l1, l2, l3) -> Lifthenelse(aux  l1, aux  l2, aux  l3)
    | Lwhile(l1, l2) -> Lwhile(aux  l1, aux l2)
    | Lfor(flag, l1, l2, dir, l3) -> Lfor(flag,aux  l1, aux  l2, dir, aux  l3)
    | Lassign(v, l) ->
      (* Lalias-bound variables are never assigned, so don't increase
         v's refaux *)
      Lassign(v,aux  l)
    | Lsend(u, m, o, ll, v) -> Lsend(u, aux m, aux o, List.map aux ll,v)

    (* Levent(aux  l, event) *)
    | Lifused(v, l) -> Lifused(v,aux  l)
  in aux lam

(* TODO: check that if label belongs to a different 
    namesape
*)
let count = ref 0 

let generate_label ?(name="") ()  = 
  incr count; 
  Printf.sprintf "%s_tailcall_%04d" name !count

let log_counter = ref 0

let dump  env filename   pred lam = 
  incr log_counter ; 
  if pred 
  then 
    Printlambda.seriaize env 
      (Filename.chop_extension filename ^ 
       (Printf.sprintf ".%02d.lam" !log_counter)
      ) lam;
  lam
