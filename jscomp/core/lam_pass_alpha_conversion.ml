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
  let rec simpl  (lam : Lam.t) = 
    match lam with 
    | Lconst _ -> lam
    | Lvar _ -> lam 
    | Lapply {fn = l1; args =  ll;  loc ; status} 
      -> (* detect functor application *)
      begin 
        match Lam_arity_analysis.get_arity meta l1 with 
        | Arity_na -> 
          Lam.apply (simpl  l1) (Ext_list.map simpl  ll) loc status
        | Arity_info (_, args, _) -> 
          let len = List.length ll in 
          let rec take args = 
            match args with 
            | x :: xs -> 
              if x = len 
              then 
                Lam.apply (simpl l1)
                  (Ext_list.map simpl ll) loc App_ml_full
              else if x > len  
              then 
                let fn = simpl l1 in
                let args = Ext_list.map simpl ll in
                Lam_eta_conversion.transform_under_supply (x - len) loc App_ml_full
                  fn args 
              else 
                let first,rest = Ext_list.split_at x ll in 
                Lam.apply (
                  Lam.apply (simpl l1) 
                         (Ext_list.map simpl first) 
                         loc App_ml_full
                )
                  (Ext_list.map simpl rest) loc status (* TODO refien *)
            | _ -> Lam.apply (simpl l1) (Ext_list.map simpl ll)  loc status
          in take args
      end

    | Llet (str, v, l1, l2) ->
      Lam.let_ str v (simpl l1) (simpl l2 )
    | Lletrec (bindings, body) ->
      let bindings = Ext_list.map (fun (k,l) -> (k, simpl l)) bindings in 
      Lam.letrec bindings (simpl body) 
    | Lglobal_module _ -> lam 
    | Lprim {primitive = (Lam.Pjs_fn_make len) as primitive ; args = [arg] 
      ; loc } -> 
      
      begin match Lam_arity_analysis.get_arity meta arg with       
      | Arity_info (_, x::_, _)
        -> 
        let arg = simpl arg in
          Lam_eta_conversion.unsafe_adjust_to_arity loc 
            ~to_:len 
            ~from:x
            arg 
      | Arity_info(_,[],_)
      | Arity_na   -> Lam.prim ~primitive ~args:[simpl arg] loc
      end
    | Lprim {primitive; args ; loc} -> 
      Lam.prim ~primitive ~args:(Ext_list.map simpl  args) loc
    | Lfunction {arity; function_kind; params; body = l} ->
      (* Lam_mk.lfunction kind params (simpl l) *)
      Lam.function_ ~arity ~function_kind ~params  ~body:(simpl  l)
    | Lswitch (l, {sw_failaction; 
                  sw_consts; 
                  sw_blocks;
                  sw_numblocks;
                  sw_numconsts;
                 }) ->
      Lam.switch (simpl  l)
              {sw_consts = 
                 Ext_list.map (fun (v, l) -> v, simpl  l) sw_consts;
               sw_blocks = Ext_list.map (fun (v, l) -> v, simpl  l) sw_blocks;
               sw_numconsts = sw_numconsts;
               sw_numblocks = sw_numblocks;
               sw_failaction = 
                 begin 
                   match sw_failaction with 
                   | None -> None
                   | Some x -> Some (simpl x)
                 end}
    | Lstringswitch (l, sw, d) ->
      Lam.stringswitch (simpl  l)
                    (Ext_list.map (fun (i, l) -> i,simpl  l) sw)
                    (match d with
                      | Some d -> Some (simpl d )
                      | None -> None)
                    
    | Lstaticraise (i,ls) ->
      Lam.staticraise i (Ext_list.map simpl  ls)
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
      Lam.send u (simpl m) (simpl o) (Ext_list.map simpl ll) v
    | Lifused (v, l) -> Lam.ifused v (simpl  l)
  in 

  simpl lam
