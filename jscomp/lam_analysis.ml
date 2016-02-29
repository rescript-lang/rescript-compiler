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
            | "js_from_nullable"
            | "js_from_def"
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

      | Pignore 
      | Prevapply _
      | Pdirapply _

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
    | Lconst c -> size_constant c
    | Llet(_, _, l1, l2) -> 1 + size l1 + size l2 
    | Lletrec _ -> really_big ()
    | Lprim(Pfield _, [Lprim(Pgetglobal _, [  ])])
      -> 1
    | Lprim (Praise _, [l ]) 
      -> size l
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
    (* | Lfunction(_, params, l) -> really_big () *)
    | Lfunction(_,_params,body) -> size body 
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
and size_constant x = 
  match x with 
  | Const_base _
  | Const_immstring _
  | Const_pointer _ 
    -> 1 
  | Const_block (_, _, str) 
    ->  List.fold_left (fun acc x -> acc + size_constant x ) 0 str
  | Const_float_array xs  -> List.length xs

and size_lams acc (lams : Lambda.lambda list) = 
  List.fold_left (fun acc l -> acc  + size l ) acc lams

let exit_inline_size = 7 
let small_inline_size = 5
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
  | Pccall {prim_name = n0 ; 
            prim_attributes = [];
           },  
    Pccall {prim_name = n1; 
            prim_attributes = [] ;

           } -> 
    n0 = n1 (* No attributes, should be class api, comparison by name is good *)
  | _ , _ -> 
    (* FIXME: relies on structure equality *) 
    try p = p1 with _ -> false



type stats = 
  { 
    mutable top : bool ; 
    (* all appearances are in the top,  substitution is fine 
       whether it is pure or not
       {[
         (fun x y          
           ->  x + y + (f x )) (32) (console.log('hi'), 33)
       ]}       
       since in ocaml, the application order is intentionally undefined, 
       note if [times] is not one, this field does not make sense       
    *)    
    mutable times : int ; 
  }
type env = 
  { top  : bool ; 
    loop : bool 
  }

let no_substitute = { top = false; loop = true }
let fresh_env = {top = true; loop = false }
let fresh_stats () = { top = true; times = 0 }

let param_map_of_list lst = 
  List.fold_left  (fun acc l -> Ident_map.add l (fresh_stats ()) acc) Ident_map.empty  lst 

(** Sanity check, remove all varaibles in [local_set] in the last pass *)  

let free_variables (export_idents : Ident_set.t ) (params : stats Ident_map.t ) lam = 
  let fv = ref params in
  let local_set = ref export_idents in

  let local_add k =
    local_set := Ident_set.add k !local_set in
  let local_add_list ks = 
    local_set :=  
      List.fold_left (fun acc k -> Ident_set.add k acc) !local_set ks 
  in    
  let loop_use = 100 in
  let map_use {top; loop} v = 
    (* relies on [identifier] uniquely bound *)    
    let times = if loop then loop_use else 1 in
    if Ident_set.mem v !local_set then ()    
    else begin match Ident_map.find v !fv with 
      | exception Not_found
        -> fv := Ident_map.add v { top ; times } !fv
      | v ->
        v.times <- v.times + times ; 
        v.top <- v.top && top          
    end
  in
  let new_env lam (env : env) = 
    if env.top then 
      if no_side_effects lam 
      then env 
      else { env with top = false}
    else env      
  in    
  let rec iter (top : env) (lam : Lambda.lambda) =
    match lam with 
    | Lvar v -> map_use top v 
    | Lconst _ -> ()
    | Lapply(fn, args, _) ->
      iter top  fn; 
      let top = new_env fn top in
      List.iter (iter top ) args  
    | Lprim(_p, args) -> 
      (* Check: can top be propoaged for all primitives *)
      List.iter (iter top) args
    | Lfunction(_kind, params, body) ->
      local_add_list params;
      iter no_substitute body 
    | Llet(_let_kind, id, arg, body) ->
      local_add id ;  
      iter top  arg; iter no_substitute body
    | Lletrec(decl, body) ->
      local_set := List.fold_left (fun acc (id, _) -> 
          Ident_set.add id acc) !local_set decl;        
      List.iter (fun (_, exp) -> iter no_substitute exp) decl;
      iter no_substitute body
    | Lswitch(arg, sw) ->
      iter top arg; 
      let top = new_env arg top  in       
      List.iter (fun (key, case) -> iter top case) sw.sw_consts;
      List.iter (fun (key, case) -> iter top  case) sw.sw_blocks;
  
      begin match sw.sw_failaction with 
        | None -> ()
        | Some x ->
          let nconsts = List.length sw.sw_consts in
          let nblocks = List.length sw.sw_blocks in

          if nconsts < sw.sw_numconsts  && nblocks < sw.sw_numblocks then
            iter no_substitute x
          else
            iter top x
      end

    | Lstringswitch (arg,cases,default) ->
      iter top arg ;
      let top = new_env arg top  in       
      List.iter (fun (_,act) -> iter top  act) cases ;
      begin match default with 
      | None -> ()
      | Some x -> iter top x 
      end
    | Lstaticraise (_,args) ->
      List.iter (iter no_substitute ) args
    | Lstaticcatch(e1, (_,vars), e2) ->
      iter no_substitute  e1; 
      local_add_list vars;       
      iter no_substitute e2
    | Ltrywith(e1, exn, e2) ->
      iter top  e1; iter no_substitute  e2
    | Lifthenelse(e1, e2, e3) ->
      iter top e1; 
      let top = new_env e1 top  in
      iter top e2; iter top e3
    | Lsequence(e1, e2) ->
      iter top e1; iter no_substitute e2
    | Lwhile(e1, e2) ->
      iter no_substitute e1; iter no_substitute e2 (* in the loop, no substitution any way *)
    | Lfor(v, e1, e2, dir, e3) ->
      local_add v ; 
      iter no_substitute e1; iter no_substitute e2; iter no_substitute e3
    | Lassign(id, e) ->
      map_use top  id ; 
      iter top e
    | Lsend (_k, met, obj, args, _) ->
      iter no_substitute met ; 
      iter no_substitute obj;
      List.iter (iter no_substitute) args
    | Levent (lam, evt) ->
      iter top  lam
    | Lifused (v, e) ->
      iter no_substitute e in
  iter fresh_env  lam ; !fv 


let is_closed_by set lam = 
  Ident_map.is_empty (free_variables set (Ident_map.empty ) lam   )


(** A bit consverative , it should be empty *)
let is_closed  lam = 
  Ident_map.for_all (fun k _ -> Ident.global k)
    (free_variables Ident_set.empty Ident_map.empty lam)  


let is_closed_with_map exports params body = 
  let param_map = free_variables exports (param_map_of_list params) body in
  let old_count = List.length params in
  let new_count = Ident_map.cardinal param_map in
  (old_count  = new_count, param_map)


  
(* TODO:  We can relax this a bit later,
    but decide whether to inline it later in the call site
 *)
let safe_to_inline (lam : Lambda.lambda) = 
  match lam with 
  | Lfunction _ ->  true
  | Lconst (Const_pointer _  | Const_immstring _ ) -> true
  | _ -> false
