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

(* [groups] are in reverse order *)

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
      (*            | Single ((Alias | Strict | StrictOpt), id, ( Lfunction _ )) -> 
              (** FIXME: 
                   It should be alias and alias will be optimized away
                   in later optmizations, however, 
                   this means if we don't optimize 
                  {[ let u/a = v in ..]}
                   the output would be wrong, we should *optimize 
                   this away right now* instead of delaying it to the 
                   later passes
              *)
              (acc, set, g :: wrap, stop)
      *)
       (* could also be from nested [let rec] 
                 like 
                 {[
                   let rec x = 
                     let rec y = 1 :: y in
                     2:: List.hd y:: x 
                 ]}
                 TODO: seems like we should update depenency graph, 

              *)
      (** TODO: more flattening, 
          - also for function compilation, flattening should be done first
          - [compile_group] and [compile] become mutually recursive function
      *)
      (* Printlambda.lambda Format.err_formatter lam ; assert false  *)              
let lambda_of_groups ~rev_bindings result  = 
  List.fold_left (fun acc x -> 
      match (x : Lam_group.t) with 
      | Nop l -> Lam.seq l acc
      | Single(kind,ident,lam) -> Lam_util.refine_let ~kind ident lam acc
      | Recursive bindings -> Lam.letrec bindings acc) 
    result rev_bindings


(* TODO: 
    refine effectful [ket_kind] to be pure or not
    Be careful of how [Lifused(v,l)] work 
    since its semantics depend on whether v is used or not
    return value are in reverse order, but handled by [lambda_of_groups]
*)
let deep_flatten
    (lam : Lam.t) :  Lam.t  = 
  let rec
    flatten 
      (acc :  Lam_group.t list ) 
      (lam : Lam.t) :  Lam.t *  Lam_group.t list = 
    match lam with 
    | Llet (str, id, 
            (Lprim {primitive = (
                          Pnull_to_opt
                         | Pundefined_to_opt
                         | Pnull_undefined_to_opt)
                   ; args  =  [Lvar _]} as arg), body)
      -> 
      flatten (Single(str, id, (aux arg) ) :: acc) body
    | Llet (str, id, 
            Lprim {primitive = ( 
                          Pnull_to_opt | Pundefined_to_opt | Pnull_undefined_to_opt as primitive );
                   args = [arg]}, body)
      -> 
      let id' = Ident.rename id in 
      flatten acc 
        (Lam.let_ str id' arg 
               (Lam.let_ Alias id 
                  (Lam.prim 
                     ~primitive
                     ~args: [Lam.var id'] Location.none (* FIXME*))
                  body)
              )
    | Llet (str,id,arg,body) -> 
      let (res,l) = flatten acc arg  in
      flatten (Single(str, id, res ) :: l) body
    | Lletrec (bind_args, body) -> 

      flatten
        (
          Recursive
            (Ext_list.map (fun (id, arg ) -> (id, aux arg)) bind_args)
          :: acc
        )
        body
    | Lsequence (l,r) -> 
      let (res, l)  = flatten acc l in
      flatten (Nop res :: l)  r
    | x ->  
      aux x, acc      

  and aux  (lam : Lam.t) : Lam.t= 
    match lam with 
    | Llet _ -> 
      let res, groups = flatten [] lam  
      in lambda_of_groups res ~rev_bindings:groups
    | Lletrec (bind_args, body) ->  
      (* Attention: don't mess up with internal {let rec} *)
      let rec iter bind_args groups set  =
        match bind_args with
        | [] ->   (List.rev groups, set)
        | (id,arg) :: rest ->
          iter rest ((id, aux arg) :: groups) (Ident_set.add id set)
      in
      let groups, collections = iter bind_args [] Ident_set.empty in
      (* Try to extract some value definitions from recursive values as [wrap],
         it will stop whenever it find it could not move forward
        {[
           let rec x = 
              let y = 1 in
              let z = 2 in 
              ...  
        ]}
      *)
      let (rev_bindings, rev_wrap, _) = 
        List.fold_left (fun  (inner_recursive_bindings,  wrap,stop)  ((id,lam) )  ->           
          if stop || Lam.hit_any_variables collections lam  then 
              (id, lam) :: inner_recursive_bindings, wrap, true
          else 
              (inner_recursive_bindings,  (Lam_group.Single (Strict, id, lam)) :: wrap, false)
          ) ([],  [], false ) groups in
      lambda_of_groups 
      ~rev_bindings:rev_wrap (* These bindings are extracted from [letrec] *)
        (Lam.letrec  (List.rev rev_bindings)  (aux body)) 
    | Lsequence (l,r) -> Lam.seq (aux l) (aux r)
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

    | Lapply{fn = l1; args  = ll; loc; status} -> 
      Lam.apply (aux l1) (Ext_list.map aux ll) loc status

    (* This kind of simple optimizations should be done each time
       and as early as possible *) 

    | Lprim {primitive = Pccall{prim_name = "caml_int64_float_of_bits"; _};
            args = [ Lconst (  (Const_int64 i))]; _} 
      ->  
      Lam.const 
        (  (Const_float (Js_number.to_string (Int64.float_of_bits i) )))
    | Lprim {primitive = Pccall{prim_name = "caml_int64_to_float"; _}; 
             args = [ Lconst (  (Const_int64 i))]; _} 
      -> 
      (* TODO: note when int is too big, [caml_int64_to_float] is unsafe *)
      Lam.const 
        (  (Const_float (Js_number.to_string (Int64.to_float i) )))
    | Lglobal_module _ -> lam 
    | Lprim {primitive ; args; loc }
      -> 
      let args = Ext_list.map aux args in
      Lam.prim ~primitive ~args loc

    | Lfunction{arity; function_kind; params;  body = l} -> 
      Lam.function_ ~arity ~function_kind ~params  ~body:(aux  l)
    | Lswitch(l, {sw_failaction; 
                  sw_consts; 
                  sw_blocks;
                  sw_numblocks;
                  sw_numconsts;
                 }) ->
      Lam.switch (aux  l)
              {sw_consts = 
                 Ext_list.map (fun (v, l) -> v, aux  l) sw_consts;
               sw_blocks = Ext_list.map (fun (v, l) -> v, aux  l) sw_blocks;
               sw_numconsts = sw_numconsts;
               sw_numblocks = sw_numblocks;
               sw_failaction = 
                 begin 
                   match sw_failaction with 
                   | None -> None
                   | Some x -> Some (aux x)
                 end}
    | Lstringswitch(l, sw, d) ->
      Lam.stringswitch (aux  l) 
                    (Ext_list.map (fun (i, l) -> i,aux  l) sw)
                    (match d with
                     | Some d -> Some (aux d )
                     | None -> None)

    | Lstaticraise (i,ls) 
      -> Lam.staticraise i (Ext_list.map aux  ls)
    | Lstaticcatch(l1, ids, l2) 
      -> 
      Lam.staticcatch (aux  l1) ids (aux  l2)
    | Ltrywith(l1, v, l2) ->
      Lam.try_ (aux  l1) v (aux  l2)
    | Lifthenelse(l1, l2, l3) 
      -> 
      Lam.if_ (aux  l1) (aux l2) (aux l3)
    | Lwhile(l1, l2) 
      -> 
      Lam.while_ (aux  l1) (aux l2)
    | Lfor(flag, l1, l2, dir, l3) 
      -> 
      Lam.for_ flag (aux  l1) (aux  l2) dir (aux  l3)
    | Lassign(v, l) ->
      (* Lalias-bound variables are never assigned, so don't increase
         v's refaux *)
      Lam.assign v (aux  l)
    | Lsend(u, m, o, ll, v) -> 
      Lam.send u (aux m) (aux o) (Ext_list.map aux ll) v

    | Lifused(v, l) -> Lam.ifused v (aux  l)
  in aux lam
