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






type t = 
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

let pp_group env fmt ( x : t) =
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


let rec flatten 
    (acc :  t list ) 
    (lam : Lambda.lambda) :  Lambda.lambda *  t list = 
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
      | Nop l -> Lam_comb.seq l acc
      | Single(kind,ident,lam) -> Lam_util.refine_let ~kind ident lam acc
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
      (acc :  t list ) 
      (lam : Lambda.lambda) :  Lambda.lambda *  t list = 
    match lam with 
    | Levent (e,_) -> flatten acc e (* TODO: We stripped event in the beginning*)
    | Llet (str, id, 
            (Lprim (Pccall 
                      {prim_name = 
                         ("js_from_nullable" 
                         | "js_from_def"
                         |"js_from_nullable_def"); _ }
                   , [Lvar _]) as arg), body)
      -> 
      flatten (Single(str, id, (aux arg) ) :: acc) body
    | Llet (str, id, 
            Lprim (Pccall 
                     ({prim_name = 
                         ("js_from_nullable"
                         | "js_from_def"
                         | "js_from_nullable_def"); _ } as p ),
                            [arg]), body)
      -> 
      let id' = Ident.rename id in 
      flatten acc (Llet (str, id', arg, 
                         Llet(Alias, id, Lam_comb.prim (Pccall p)  [Lvar id'], body)
                        ))
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
    | Lsequence (l,r) -> Lam_comb.seq (aux l) (aux r)
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
    | Lprim(Pccall{prim_name = "caml_int64_float_of_bits"; _},
            [ Lconst (Const_base (Const_int64 i))]) 
      ->  
      Lconst (Const_base (Const_float (Js_number.to_string (Int64.float_of_bits i) )))
    | Lprim(Pccall{prim_name = "caml_int64_to_float"; _},
            [ Lconst (Const_base (Const_int64 i))]) 
      -> 
      (* TODO: note when int is too big, [caml_int64_to_float] is unsafe *)
      Lconst (Const_base (Const_float (Js_number.to_string (Int64.to_float i) )))
    | Lprim(p, ll)
      -> 
      begin
        let ll = List.map aux ll in
        match p, ll with
        (* Simplify %revapply, for n-ary functions with n > 1 *)
        | Prevapply loc, [x; Lapply (f, args, _)]
        | Prevapply loc, [x; Levent (Lapply (f, args, _),_)] ->
          Lapply (f, args@[x], Lambda.default_apply_info ~loc ())
        | Prevapply loc, [x; f] -> Lapply (f, [x], Lambda.default_apply_info ~loc ())
        (* Simplify %apply, for n-ary functions with n > 1 *)
        | Pdirapply loc, [Lapply(f, args, _); x]
        | Pdirapply loc, [Levent (Lapply (f, args, _),_); x] ->
          Lapply (f, args@[x], Lambda.default_apply_info ~loc ())
        | Pdirapply loc, [f; x] -> Lapply (f, [x], Lambda.default_apply_info ~loc ())
        | _ -> Lam_comb.prim p ll
      end
    | Lfunction(kind, params, l) -> Lfunction (kind, params , aux  l)
    | Lswitch(l, {sw_failaction; 
                  sw_consts; 
                  sw_blocks;
                  sw_numblocks;
                  sw_numconsts;
                 }) ->
      Lam_comb.switch (aux  l)
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
                 end}
    | Lstringswitch(l, sw, d) ->
      Lam_comb.stringswitch (aux  l) 
                    (List.map (fun (i, l) -> i,aux  l) sw)
                    (match d with
                     | Some d -> Some (aux d )
                     | None -> None)

    | Lstaticraise (i,ls) 
      -> Lam_comb.staticraise i (List.map aux  ls)
    | Lstaticcatch(l1, ids, l2) 
      -> 
      Lam_comb.staticcatch (aux  l1) ids (aux  l2)
    | Ltrywith(l1, v, l2) ->
      Lam_comb.try_ (aux  l1) v (aux  l2)
    | Lifthenelse(l1, l2, l3) 
      -> 
      Lam_comb.if_ (aux  l1) (aux l2) (aux l3)
    | Lwhile(l1, l2) 
      -> 
      Lam_comb.while_ (aux  l1) (aux l2)
    | Lfor(flag, l1, l2, dir, l3) 
      -> 
      Lam_comb.for_ flag (aux  l1) (aux  l2) dir (aux  l3)
    | Lassign(v, l) ->
      (* Lalias-bound variables are never assigned, so don't increase
         v's refaux *)
      Lam_comb.assign v (aux  l)
    | Lsend(u, m, o, ll, v) -> 
      Lam_comb.send u (aux m) (aux o) (List.map aux ll) v

    (* Levent(aux  l, event) *)
    | Lifused(v, l) -> Lifused(v,aux  l)
  in aux lam
