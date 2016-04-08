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
                              List.map simpl ll, {info with apply_status = Full} )
              else if x > len  
              then 
                (* Lapply(simpl l1, List.map simpl ll,  info ) *)
                let extra_args = Ext_list.init (x - len)
                    (fun _ ->   (Ident.create "param")) in
                let fn = simpl l1 in
                let args = List.map simpl ll in
                let extra_lambdas = List.map (fun x -> Lambda.Lvar x) extra_args in
                let args, bindings =
                  List.fold_right (fun lam (acc, bind) ->
                      match lam with
                      | Lambda.Lvar _
                      | Lconst (Const_base _ | Const_pointer _ | Const_immstring _ ) 
                      | Lprim (Lambda.Pfield (_), [Lprim (Lambda.Pgetglobal _, _)] )
                      | Lfunction _ 
                        ->
                        (lam :: acc, bind)
                      | _ ->
                        let v = Ident.create Literals.partial_arg in
                        (Lambda.Lvar v :: acc),  ((v, lam) :: bind)
                    ) args ([],[])   in 
                (* let args, bindings = args, [] in *)
                let rest : Lambda.lambda = 
                  Lfunction(Curried, extra_args,
                            Lapply(fn,
                                   args @ extra_lambdas ,
                                   {info with  apply_status = Full}
                                  )) in
                List.fold_left (fun lam (id,x) ->
                      Lambda.Llet (Strict, id, x,lam)
                    ) rest bindings
                                  (*
                                    let f x y =  x + y 
                                    Invariant: there is no currying 
                                    here since f's arity is 2, no side effect 
                                    f 3 --> function(y) -> f 3 y 
                                   *)
              else 
                let first,rest = Ext_list.take x ll in 
                Lapply (
                  Lapply(simpl l1, List.map simpl first, {info with apply_status = Full}),
                  (List.map simpl rest), info) (* TODO refien *)
            | _ -> Lapply(simpl l1, List.map simpl ll,  info )
          in take args
      end

    | Llet (str, v, l1, l2) ->
      Llet (str, v, simpl l1, simpl l2 )
    | Lletrec (bindings, body) ->
      let bindings = List.map (fun (k,l) -> (k, simpl l)) bindings in 
      Lletrec (bindings, simpl body) 
    | Lprim (prim, ll) -> Lprim(prim, List.map simpl  ll)
    | Lfunction (kind, params, l) ->
      (* Lam_mk.lfunction kind params (simpl l) *)
      Lfunction (kind, params , simpl  l)
    | Lswitch (l, {sw_failaction; 
                  sw_consts; 
                  sw_blocks;
                  sw_numblocks;
                  sw_numconsts;
                 }) ->
      Lswitch (simpl  l,
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
                 end})
    | Lstringswitch (l, sw, d) ->
      Lstringswitch (simpl  l ,
                    List.map (fun (i, l) -> i,simpl  l) sw,
                    begin 
                      match d with
                      | Some d -> Some (simpl d )
                      | None -> None
                    end)
    | Lstaticraise (i,ls) -> Lstaticraise(i, List.map (simpl ) ls)
    | Lstaticcatch (l1, (i,x), l2) -> Lstaticcatch(simpl  l1, (i,x), simpl  l2)
    | Ltrywith (l1, v, l2) -> Ltrywith(simpl  l1,v, simpl  l2)
    | Lifthenelse (l1, l2, l3) -> Lifthenelse(simpl  l1, simpl  l2, simpl  l3)
    | Lsequence (l1, l2) -> Lsequence(simpl  l1, simpl  l2)
    | Lwhile (l1, l2) -> Lwhile(simpl  l1, simpl l2)
    | Lfor (flag, l1, l2, dir, l3) -> Lfor(flag,simpl  l1, simpl  l2, dir, simpl  l3)
    | Lassign (v, l) ->
      (* Lalias-bound variables are never assigned, so don't increase
         v's refsimpl *)
      Lassign (v,simpl  l)
    | Lsend (u, m, o, ll, v) -> Lsend(u, simpl m, simpl o, List.map simpl ll,v)
    | Levent (l, event) -> Levent (simpl  l, event)
    | Lifused (v, l) -> Lifused (v,simpl  l)
  in 

  simpl lam
