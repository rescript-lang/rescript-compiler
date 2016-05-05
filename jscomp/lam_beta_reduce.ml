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








(*
   Given an [map], rewrite all let bound variables into new variables, 
   note that the [map] is changed
   example    
   {[
     let a/112 = 3 in a/112      
   ]}
   would be converted into 
   {[
     let a/113 = 3 in a/113     
   ]}   

   ATTENTION: [let] bound idents have to be renamed, 
   Note we rely on an invariant that parameter could not be rebound 
 *)

(* 
  Small function inline heuristics:
  Even if a function is small, it does not mean it is good for inlining,
  for example, in list.ml
   {[
     let rec length_aux len = function
         [] -> len
       | a::l -> length_aux (len + 1) l

     let length l = length_aux 0 l
   ]}   
   if we inline [length], it will expose [length_aux] to the user, first, it make 
   the code not very friendly, also since [length_aux] is used everywhere now, it 
   may affect that we will not do the inlining of [length_aux] in [length]

   Criteior for sure to inline   
   1. small size, does not introduce extra symbols, non-exported and non-recursive   
      non-recursive is required if we re-apply the strategy

   Other Factors:   
   2. number of invoked times
   3. arguments are const or not   
*)
let rewrite (map :   (Ident.t, _) Hashtbl.t) 
    (lam : Lambda.lambda) : Lambda.lambda = 

  let rebind i = 
    let i' = Ident.rename i in 
    Hashtbl.add map i (Lambda.Lvar i');
    i' in
  (* order matters, especially for let bindings *)
  let rec 
    option_map op = 
    match op with 
    | None -> None 
    | Some x -> Some (aux x)
  and aux (lam : Lambda.lambda) : Lambda.lambda = 
    match lam with 
    | Lvar v -> 
      begin 
        try (* Lvar *) (Hashtbl.find map v) 
        with Not_found -> lam 
      end
    | Llet(str, v, l1, l2) ->
      let v = rebind v in
      let l1 = aux l1 in      
      let l2 = aux l2 in
      Llet(str, v,  l1,  l2 )
    | Lletrec(bindings, body) ->
      let bindings = 
        bindings |> List.map (fun (k,l) ->  
            let k = rebind k in
            (k, aux l)
          )  in 
      let body = aux body in       
      Lletrec(bindings, body) 
    | Lfunction(kind, params, body) -> 
      let params =  List.map rebind params in
      let body = aux body in      
      Lfunction (kind, params, body)
    | Lstaticcatch(l1, (i,xs), l2) -> 
      let l1 = aux l1 in
      let xs = List.map rebind xs in
      let l2 = aux l2 in
      Lam_comb.staticcatch l1 (i,xs) l2
    | Lfor(ident, l1, l2, dir, l3) ->
      let ident = rebind ident in 
      let l1 = aux l1 in
      let l2 = aux l2 in
      let l3 = aux l3 in
      Lam_comb.for_ ident (aux  l1)  l2 dir  l3
    | Lconst _ -> lam
    | Lprim(prim, ll) ->
      (* here it makes sure that global vars are not rebound *)      
      Lam_comb.prim prim (List.map aux  ll)
    | Lapply(fn, args, info) ->
      let fn = aux fn in       
      let args = List.map aux  args in 
      Lapply(fn, args, info)
    | Lswitch(l, {sw_failaction; 
                  sw_consts; 
                  sw_blocks;
                  sw_numblocks;
                  sw_numconsts;
                 }) ->
      let l = aux l in
      Lam_comb.switch l
              {sw_consts = 
                 List.map (fun (v, l) -> v, aux  l) sw_consts;
               sw_blocks = List.map (fun (v, l) -> v, aux  l) sw_blocks;
               sw_numconsts = sw_numconsts;
               sw_numblocks = sw_numblocks;
               sw_failaction =  option_map sw_failaction
              }
    | Lstringswitch(l, sw, d) ->
      let l = aux  l in
      Lam_comb.stringswitch l 
                     (List.map (fun (i, l) -> i,aux  l) sw)
                     (option_map d)
    | Lstaticraise (i,ls) 
      -> Lam_comb.staticraise i (List.map aux  ls)
    | Ltrywith(l1, v, l2) -> 
      let l1 = aux l1 in
      let v = rebind v in
      let l2 = aux l2 in
      Lam_comb.try_ l1 v l2
    | Lifthenelse(l1, l2, l3) -> 
      let l1 = aux l1 in
      let l2 = aux l2 in
      let l3 = aux l3 in
      Lam_comb.if_ l1  l2   l3
    | Lsequence(l1, l2) -> 
      let l1 = aux l1 in
      let l2 = aux l2 in
      Lam_comb.seq l1 l2
    | Lwhile(l1, l2) -> 
      let l1 = aux l1 in
      let l2 = aux l2 in
      Lam_comb.while_  l1  l2
    | Lassign(v, l) 
      -> Lam_comb.assign v (aux  l)
    | Lsend(u, m, o, ll, v) ->
      let m = aux m in 
      let o = aux o in 
      let ll = List.map aux ll in
      Lam_comb.send u  m  o  ll v
    | Levent(l, event) ->
      let l = aux l in
      Lam_comb.event  l event
    | Lifused(v, l) -> 
      let l = aux l in 
      Lam_comb.ifused v  l
  in 
  aux lam


let refresh lam = rewrite (Hashtbl.create 17 ) lam




(* 
    A naive beta reduce would break the invariants of the optmization.


    The sane but slowest  way:
      when we do a beta reduction, we need rename all variables inlcuding 
      let-bound ones

    A conservative one:
      - for internal one 
        rename params and let bound variables
      - for external one (seriaized)
        if it's enclosed environment should be good enough
        so far, we only inline enclosed lambdas
    TODO: rename 

   Optimizations:   
   {[
     (fun x y -> ...     ) 100 3 
   ]}   
   we can bound [x] to [100] in a single step     
 *)
let propogate_beta_reduce 
    (meta : Lam_stats.meta) params body args =
  match Lam_beta_reduce_util.simple_beta_reduce params body  args with 
  | Some x -> x 
  | None -> 
  let rest_bindings, rev_new_params  = 
    List.fold_left2 
      (fun (rest_bindings, acc) old_param (arg : Lambda.lambda) -> 
         match arg with          
         | Lconst _
         | Lvar _  -> rest_bindings , arg :: acc 
         | _ -> 
           let p = Ident.rename old_param in 
           (p,arg) :: rest_bindings , (Lambda.Lvar p) :: acc 
      )  ([],[]) params args in
  let new_body = rewrite (Ext_hashtbl.of_list2 (List.rev params) (rev_new_params)) body in
  List.fold_right
    (fun (param, (arg : Lambda.lambda)) l -> 
       let arg = 
         match arg with 
         | Lvar v -> 
           begin 
             match Hashtbl.find meta.ident_tbl v with 
             | exception Not_found -> ()
             | ident_info -> 
               Hashtbl.add meta.ident_tbl param ident_info 
           end;
           arg 
         | Lprim (Pgetglobal ident, []) -> 
           (* It's not completeness, its to make it sound.. *)
           Lam_compile_global.query_lambda ident meta.env 
         (* alias meta param ident (Module (Global ident)) Strict *)
         | Lprim (Pmakeblock (_, _, Immutable ) , ls) -> 
           Hashtbl.replace meta.ident_tbl param 
             (Lam_util.kind_of_lambda_block Normal ls ); (** *)
           arg
         | _ -> arg in
       Lam_util.refine_let param arg l) 
     rest_bindings new_body

let propogate_beta_reduce_with_map  
    (meta : Lam_stats.meta) (map : Lam_analysis.stats Ident_map.t ) params body args =
  match Lam_beta_reduce_util.simple_beta_reduce params body args with
  | Some x -> x
  | None ->
  let rest_bindings, rev_new_params  = 
    List.fold_left2 
      (fun (rest_bindings, acc) old_param (arg : Lambda.lambda) -> 
         match arg with          
         | Lconst _
         | Lvar _  -> rest_bindings , arg :: acc 
         | Lprim (Pgetglobal ident, [])
           (* TODO: we can pass Global, but you also need keep track of it*)
           ->
           let p = Ident.rename old_param in 
           (p,arg) :: rest_bindings , (Lambda.Lvar p) :: acc 

         | _ -> 
           if  Lam_analysis.no_side_effects arg then
             begin match Ident_map.find old_param map with 
               | exception Not_found -> assert false 
               | {top = true ; times = 0 }
               | {top = true ; times = 1 } 
                 -> 
                 rest_bindings, arg :: acc                
               | _  ->  
                 let p = Ident.rename old_param in 
                 (p,arg) :: rest_bindings , (Lambda.Lvar p) :: acc 
             end
           else
             let p = Ident.rename old_param in 
             (p,arg) :: rest_bindings , (Lambda.Lvar p) :: acc 
      )  ([],[]) params args in
  let new_body = rewrite (Ext_hashtbl.of_list2 (List.rev params) (rev_new_params)) body in
  List.fold_right
    (fun (param, (arg : Lambda.lambda)) l -> 
       let arg = 
         match arg with 
         | Lvar v -> 
           begin 
             match Hashtbl.find meta.ident_tbl v with 
             | exception Not_found -> ()
             | ident_info -> 
               Hashtbl.add meta.ident_tbl param ident_info 
           end;
           arg 
         | Lprim (Pgetglobal ident, []) -> 
           (* It's not completeness, its to make it sound.. *)
           Lam_compile_global.query_lambda ident meta.env 
         (* alias meta param ident (Module (Global ident)) Strict *)
         | Lprim (Pmakeblock (_, _, Immutable ) , ls) -> 
           Hashtbl.replace meta.ident_tbl param 
             (Lam_util.kind_of_lambda_block Normal ls ); (** *)
           arg
         | _ -> arg in
       Lam_util.refine_let param arg l) 
     rest_bindings new_body



let beta_reduce params body args =
  match Lam_beta_reduce_util.simple_beta_reduce params body args with 
  | Some x -> x 
  | None -> 
    List.fold_left2 
      (fun l param arg ->
         Lam_util.refine_let param arg l)
      body params args
