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








let alpha_conversion (meta : Lam_stats.t) (lam : Lam.t) : Lam.t = 
  let rec 
    populateApplyInfo (args_arity : int list) (len : int) (fn : Lam.t) (args : Lam.t list) loc status : Lam.t = 
    match args_arity with 
    | 0 :: _ 
    | [] -> Lam.apply (simpl fn) (Ext_list.map args simpl)  loc status
    | x :: xs -> 
      if x = len 
      then 
        Lam.apply (simpl fn) (Ext_list.map args simpl) loc App_ml_full
      else if x > len  
      then 
        let fn = simpl fn in
        let args = Ext_list.map args simpl in
        Lam_eta_conversion.transform_under_supply (x - len) loc App_ml_full
          fn args 
      else 
        let first,rest = Ext_list.split_at args x in 
        Lam.apply (
          Lam.apply (simpl fn) 
            (Ext_list.map first simpl ) 
            loc App_ml_full
        )
          (Ext_list.map rest simpl ) loc status (* TODO refien *)
    
  and simpl  (lam : Lam.t) = 
    match lam with 
    | Lconst _ -> lam
    | Lvar _ -> lam 
    | Lapply {fn = l1; args =  ll;  loc ; status} 
      -> (* detect functor application *)
      let args_arity =  Lam_arity.extract_arity (Lam_arity_analysis.get_arity meta l1) in
      let len = List.length ll in         
      populateApplyInfo args_arity len l1 ll loc status
    | Llet (str, v, l1, l2) ->
      Lam.let_ str v (simpl l1) (simpl l2 )
    | Lletrec (bindings, body) ->
      let bindings = Ext_list.map_snd  bindings simpl in 
      Lam.letrec bindings (simpl body) 
    | Lglobal_module _ -> lam 
    | Lprim {primitive = (Pjs_fn_make len) as primitive ; args = [arg] 
      ; loc } -> 
      
      begin match 
      Lam_arity.get_first_arity 
      (Lam_arity_analysis.get_arity meta arg) with       
      | Some x 
        -> 
        let arg = simpl arg in
          Lam_eta_conversion.unsafe_adjust_to_arity loc 
            ~to_:len 
            ~from:x
            arg 
      | None ->  Lam.prim ~primitive ~args:[simpl arg] loc
      end
    | Lprim {primitive; args ; loc} -> 
      Lam.prim ~primitive ~args:(Ext_list.map args simpl) loc
    | Lfunction {arity; params; body = l} ->
      (* Lam_mk.lfunction kind params (simpl l) *)
      Lam.function_ ~arity  ~params  ~body:(simpl  l)
    | Lswitch (l, {sw_failaction; 
                  sw_consts; 
                  sw_blocks;
                  sw_numblocks;
                  sw_numconsts;
                 }) ->
      Lam.switch (simpl  l)
              {sw_consts = 
                 Ext_list.map_snd  sw_consts simpl;
               sw_blocks = Ext_list.map_snd  sw_blocks simpl;
               sw_numconsts;
               sw_numblocks;
               sw_failaction = Ext_option.map sw_failaction simpl;
              }
    | Lstringswitch (l, sw, d) ->
      Lam.stringswitch (simpl  l)
                    (Ext_list.map_snd sw simpl)
                    (Ext_option.map d simpl)
    | Lstaticraise (i,ls) ->
      Lam.staticraise i (Ext_list.map ls simpl)
    | Lstaticcatch (l1, ids, l2) 
      -> 
      Lam.staticcatch (simpl  l1) ids (simpl  l2)
    | Ltrywith (l1, v, l2) 
      -> 
      Lam.try_ (simpl  l1) v (simpl  l2)
    | Lifthenelse (l1, l2, l3) -> 
      Lam.if_ (simpl  l1) (simpl  l2) (simpl  l3)
    | Lsequence (l1, l2) 
      -> Lam.seq (simpl  l1) (simpl  l2)
    | Lwhile (l1, l2)
      -> Lam.while_ (simpl  l1) (simpl l2)
    | Lfor (flag, l1, l2, dir, l3)
      -> Lam.for_ flag (simpl  l1) (simpl  l2) dir (simpl  l3)
    | Lassign (v, l) ->
      (* Lalias-bound variables are never assigned, so don't increase
         v's refsimpl *)
      Lam.assign v (simpl  l)
    | Lsend (u, m, o, ll, v) -> 
      Lam.send u (simpl m) (simpl o) (Ext_list.map ll simpl) v

  in 

  simpl lam
