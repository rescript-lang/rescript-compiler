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


let arity_of_var (meta : Lam_stats.t) (v : Ident.t)  =
    (** for functional parameter, if it is a high order function,
        if it's not from function parameter, we should warn
    *)
      match Ident_hashtbl.find_opt meta.ident_tbl v with 
      | Some (FunctionId {arity;_}) -> arity
      | Some _
      | None ->
        Lam_arity.na 


(* we need record all aliases -- since not all aliases are eliminated, 
   mostly are toplevel bindings
   We will keep iterating such environment
   If not found, we will return [NA]
*)
let rec get_arity 
    (meta : Lam_stats.t) 
    (lam : Lam.t) : 
  Lam_arity.t = 
  match lam with 
  | Lconst _ -> Lam_arity.non_function_arity_info
  | Lvar v -> arity_of_var meta v 
  | Llet(_,_,_, l ) -> get_arity meta l 
  | Lprim {primitive = Pfield (n,_); 
           args =  [ Lglobal_module id  ]; _} ->
    begin match (Lam_compile_env.cached_find_ml_id_pos id n meta.env).arity with 
    | Single x -> x 
    | Submodule _ -> Lam_arity.na
    end
  | Lprim {primitive = Pfield (m,_); 
           args =  [ Lprim{primitive = Pfield(n,_); 
            args = [ Lglobal_module id]}  ]
           ; _} ->
    begin match (Lam_compile_env.cached_find_ml_id_pos id n meta.env).arity with 
      | Submodule subs -> subs.(m)
      | Single _ -> Lam_arity.na
  end
  (* TODO: all information except Pccall is complete, we could 
    get more arity information
  *)
  | Lprim {primitive = Praise ;  _} -> Lam_arity.raise_arity_info
  | Lglobal_module _ (* TODO: fix me never going to happen assert false  *)
  | Lprim _  -> Lam_arity.na (* CHECK*)
  (* shall we handle primitive in a direct way, 
      since we know all the information
      Invariant: all primitive application is fully applied, 
      since this information  is already available

      -- Check external c functions ?
      -- it's not true for primitives 
      like caml_set_oo_id  or  Lprim (Pmakeblock , [])

      it seems true that primitive is always fully applied, however,
      it can return a function
  *)
  | Lletrec(_, body) ->  get_arity meta body

  | Lapply{fn = app;  args; _ } -> (* detect functor application *)
    let fn = get_arity meta app in 
    begin match fn with 
      | Arity_na -> Lam_arity.na
      | Arity_info (b, xs, tail ) -> 
        let rec take (xs : _ list) arg_length = 
          match xs with 
          | x :: yys ->
            if arg_length = x then Lam_arity.info b yys tail
            else if arg_length > x then
              take yys (arg_length - x)
            else Lam_arity.info b 
                           ((x -  arg_length ) :: yys)
                           tail
          | [] -> 
            if tail then Lam_arity.info b [] tail            
            else Lam_arity.na
            (* Actually, you can not have truly deministic arities
               for example [fun x -> x ]
            *)
        in
        take xs (List.length args) 
    end
  | Lfunction {arity;  body} -> 
    Lam_arity.merge arity  (get_arity meta body)
  | Lswitch(l, {sw_failaction; 
                sw_consts; 
                sw_blocks;
                sw_numblocks = _;
                sw_numconsts = _;
               }) -> 
    all_lambdas meta (
      let rest = 
        Ext_list.map_append snd sw_consts
        (Ext_list.map snd sw_blocks) in
      match sw_failaction with None -> rest | Some x -> x::rest )
  | Lstringswitch(l, sw, d) -> 
    begin match d with 
      | None -> all_lambdas meta (Ext_list.map snd  sw )
      | Some v -> all_lambdas meta (v:: Ext_list.map snd  sw)
    end
  | Lstaticcatch(_, _, handler) -> get_arity meta handler
  | Ltrywith(l1, _, l2) -> 
    all_lambdas meta [l1;l2]
  | Lifthenelse(l1, l2, l3) ->
    all_lambdas meta [l2;l3]
  | Lsequence(_, l2) -> get_arity meta l2 
  | Lstaticraise _ (* since it will not be in tail position *)
  | Lsend _
  | Lifused _ -> Lam_arity.na 
  | Lwhile _ 
  | Lfor _  
  | Lassign _ -> Lam_arity.non_function_arity_info

and all_lambdas meta (xs : Lam.t list) = 
  match xs with 
  | y :: ys -> 
    let arity =  get_arity meta y in 
    List.fold_left (fun exist (v : Lam.t) -> 
        match (exist : Lam_arity.t) with 
        | Arity_na -> Lam_arity.na
        | Arity_info (bbb, xxxs, tail) -> 
          begin 
            match get_arity meta v with 
            | Arity_na -> Lam_arity.na 
            | Arity_info (u,ys,tail2) -> 
              let rec aux (b,acc) xs ys = 
                match xs,ys with
                | [], [] -> (b, List.rev acc, tail && tail2) 
                | [], y::ys when tail  -> 
                  aux (b,y::acc) [] ys 
                | x::xs, [] when tail2 -> 
                  aux (b,x::acc) [] xs
                | x::xs, y::ys when x = y -> aux (b, (y :: acc)) xs ys 
                | _, _  -> (false, List.rev acc, false) in 
              let (b,acc, tail3)  = aux ( u &&bbb, []) xxxs ys in 
              Lam_arity.info b acc tail3
          end
      ) arity ys 
  | _ -> assert false 

(*
let dump_exports_arities (meta : Lam_stats.t ) = 
  let fmt = 
    if meta.filename <> "" then 
      let cmj_file = Ext_path.chop_extension meta.filename ^ Literals.suffix_cmj in
      let out = open_out cmj_file in   
      Format.formatter_of_out_channel out
    else 
      Format.err_formatter in 
  begin 
    List.iter (fun (i : Ident.t) ->  
      pp fmt "@[%s: %s -> %a@]@." meta.filename i.name 
        pp_arities  (get_arity meta (Lvar i))
              ) meta.exports
  end
*)

