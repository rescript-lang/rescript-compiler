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








let alpha_conversion (meta : Lam_stats.meta) (lam : Lambda.lambda) : Lambda.lambda = 
  let rec simpl  (lam : Lambda.lambda) = 
    match lam with 
    | Lconst _ -> lam
    | Lvar _ -> lam 
    | Lapply (l1, ll, info) -> (* detect functor application *)
      begin 
        match Lam_stats_util.get_arity meta l1 with 
        | NA -> 
          Lapply(simpl  l1, List.map simpl  ll,info)
        | Determin (b, args, tail) -> 
          let len = List.length ll in 
          let rec take args = 
            match args with 
            | (x,_) :: xs -> 
              if x = len 
              then 
                Lambda.Lapply(simpl l1,
                              List.map simpl ll,
                              {info with apply_status = App_ml_full} )
              else if x > len  
              then 
                let fn = simpl l1 in
                let args = List.map simpl ll in
                Lam_util.eta_conversion (x - len) 
                  {info with  apply_status = App_ml_full}
                  fn args 
              else 
                let first,rest = Ext_list.take x ll in 
                Lapply (
                  Lapply(simpl l1, 
                         List.map simpl first, 
                         {
                           info with apply_status = App_ml_full
                         }),
                  (List.map simpl rest), info) (* TODO refien *)
            | _ -> Lapply(simpl l1, List.map simpl ll,  info )
          in take args
      end

    | Llet (str, v, l1, l2) ->
      Llet (str, v, simpl l1, simpl l2 )
    | Lletrec (bindings, body) ->
      let bindings = List.map (fun (k,l) -> (k, simpl l)) bindings in 
      Lletrec (bindings, simpl body) 
    | Lprim (prim, ll) -> Lam_comb.prim prim (List.map simpl  ll)
    | Lfunction (kind, params, l) ->
      (* Lam_mk.lfunction kind params (simpl l) *)
      Lfunction (kind, params , simpl  l)
    | Lswitch (l, {sw_failaction; 
                  sw_consts; 
                  sw_blocks;
                  sw_numblocks;
                  sw_numconsts;
                 }) ->
      Lam_comb.switch (simpl  l)
              {sw_consts = 
                 List.map (fun (v, l) -> v, simpl  l) sw_consts;
               sw_blocks = List.map (fun (v, l) -> v, simpl  l) sw_blocks;
               sw_numconsts = sw_numconsts;
               sw_numblocks = sw_numblocks;
               sw_failaction = 
                 begin 
                   match sw_failaction with 
                   | None -> None
                   | Some x -> Some (simpl x)
                 end}
    | Lstringswitch (l, sw, d) ->
      Lam_comb.stringswitch (simpl  l)
                    (List.map (fun (i, l) -> i,simpl  l) sw)
                    (match d with
                      | Some d -> Some (simpl d )
                      | None -> None)
                    
    | Lstaticraise (i,ls) ->
      Lam_comb.staticraise i (List.map simpl  ls)
    | Lstaticcatch (l1, ids, l2) 
      -> 
      Lam_comb.staticcatch (simpl  l1) ids (simpl  l2)
    | Ltrywith (l1, v, l2) 
      -> 
      Lam_comb.try_ (simpl  l1) v (simpl  l2)
    | Lifthenelse (l1, l2, l3) -> 
      Lam_comb.if_ (simpl  l1) (simpl  l2) (simpl  l3)
    | Lsequence (l1, l2) 
      -> Lam_comb.seq (simpl  l1) (simpl  l2)
    | Lwhile (l1, l2)
      -> Lam_comb.while_ (simpl  l1) (simpl l2)
    | Lfor (flag, l1, l2, dir, l3)
      -> Lam_comb.for_ flag (simpl  l1) (simpl  l2) dir (simpl  l3)
    | Lassign (v, l) ->
      (* Lalias-bound variables are never assigned, so don't increase
         v's refsimpl *)
      Lam_comb.assign v (simpl  l)
    | Lsend (u, m, o, ll, v) -> 
      Lam_comb.send u (simpl m) (simpl o) (List.map simpl ll) v
    | Levent (l, event) -> Lam_comb.event (simpl  l) event
    | Lifused (v, l) -> Lifused (v,simpl  l)
  in 

  simpl lam
