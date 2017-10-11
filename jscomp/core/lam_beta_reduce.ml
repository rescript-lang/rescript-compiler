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
    (meta : Lam_stats.t) params body args =
  match Lam_beta_reduce_util.simple_beta_reduce params body  args with 
  | Some x -> x 
  | None -> 
  let rest_bindings, rev_new_params  = 
    List.fold_left2 
      (fun (rest_bindings, acc) old_param (arg : Lam.t) -> 
         match arg with          
         | Lconst _
         | Lvar _  -> rest_bindings , arg :: acc 
         | _ -> 
           let p = Ident.rename old_param in 
           (p,arg) :: rest_bindings , (Lam.var p) :: acc 
      )  ([],[]) params args in
  let new_body = Lam_bounded_vars.rewrite (Ident_hashtbl.of_list2 (List.rev params) (rev_new_params)) body in
  Ext_list.fold_right
    (fun (param, (arg : Lam.t)) l -> 
       let arg = 
         match arg with 
         | Lvar v -> 
           begin 
             match Ident_hashtbl.find_opt meta.ident_tbl v with 
             | None -> ()
             | Some ident_info -> 
               Ident_hashtbl.add meta.ident_tbl param ident_info 
           end;
           arg 
         | Lglobal_module ident   
         -> 
           (* It's not completeness, its to make it sound.. 
              Pass global module as an argument
           *)
           Lam_compile_global.expand_global_module_as_lam ident meta.env 
         (* alias meta param ident (Module (Global ident)) Strict *)
         | Lprim {primitive = Pmakeblock (_, _, Immutable) ;args ; _} -> 
           Ident_hashtbl.replace meta.ident_tbl param 
             (Lam_util.kind_of_lambda_block Normal args ); (** *)
           arg
         | _ -> arg in
       Lam_util.refine_let ~kind:Strict param arg l) 
     rest_bindings new_body

let propogate_beta_reduce_with_map  
    (meta : Lam_stats.t) (map : Lam_closure.stats Ident_map.t ) params body args =
  match Lam_beta_reduce_util.simple_beta_reduce params body args with
  | Some x -> x
  | None ->
  let rest_bindings, rev_new_params  = 
    List.fold_left2 
      (fun (rest_bindings, acc) old_param (arg : Lam.t) -> 
         match arg with          
         | Lconst _
         | Lvar _  -> rest_bindings , arg :: acc 
         | Lglobal_module ident 
            (* We can pass Global, but you also need keep track of it*)
           ->
           let p = Ident.rename old_param in 
           (p,arg) :: rest_bindings , (Lam.var p) :: acc 

         | _ -> 
           if  Lam_analysis.no_side_effects arg then
             begin match Ident_map.find_exn old_param map with 
               | exception Not_found -> assert false 
               | {top = true ; times = 0 }
               | {top = true ; times = 1 } 
                 -> 
                 rest_bindings, arg :: acc                
               | _  ->  
                 let p = Ident.rename old_param in 
                 (p,arg) :: rest_bindings , (Lam.var p) :: acc 
             end
           else
             let p = Ident.rename old_param in 
             (p,arg) :: rest_bindings , (Lam.var p) :: acc 
      )  ([],[]) params args in
  let new_body = Lam_bounded_vars.rewrite (Ident_hashtbl.of_list2 (List.rev params) (rev_new_params)) body in
  Ext_list.fold_right
    (fun (param, (arg : Lam.t)) l -> 
       let arg = 
         match arg with 
         | Lvar v -> 
           begin 
             match Ident_hashtbl.find_opt meta.ident_tbl v with 
             | None -> ()
             | Some ident_info -> 
               Ident_hashtbl.add meta.ident_tbl param ident_info 
           end;
           arg 
          | Lglobal_module ident 
          -> 
           (* It's not completeness, its to make it sound.. *)
           Lam_compile_global.expand_global_module_as_lam ident meta.env 
         (* alias meta param ident (Module (Global ident)) Strict *)
         | Lprim {primitive = Pmakeblock (_, _, Immutable ) ; args} -> 
           Ident_hashtbl.replace meta.ident_tbl param 
             (Lam_util.kind_of_lambda_block Normal args ); (** *)
           arg
         | _ -> arg in
       Lam_util.refine_let ~kind:Strict param arg l) 
     rest_bindings new_body



let beta_reduce params body args =
  match Lam_beta_reduce_util.simple_beta_reduce params body args with 
  | Some x -> x 
  | None -> 
    List.fold_left2 
      (fun l param arg ->
         Lam_util.refine_let ~kind:Strict param arg l)
      body params args
