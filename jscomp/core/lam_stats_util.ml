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








let pp = Format.fprintf

let pp_arities (fmt : Format.formatter) (x : Lam.function_arities) = 
  match x with 
  | NA -> pp fmt "?"
  | Determin (b,ls,tail) -> 
    begin 
      pp fmt "@[";
      (if not b 
       then 
         pp fmt "~");
      pp fmt "[";
      Format.pp_print_list ~pp_sep:(fun fmt () -> pp fmt ",")
        (fun fmt  (x,_) -> Format.pp_print_int fmt x)
        fmt ls ;
      if tail 
      then pp fmt "@ *";
      pp fmt "]@]";
    end

let pp_arities_tbl 
    (fmt : Format.formatter) 
    (arities_tbl : (Ident.t, Lam.function_arities ref) Hashtbl.t) = 
  Hashtbl.fold (fun (i:Ident.t) (v : Lam.function_arities ref) _ -> 
      pp Format.err_formatter "@[%s -> %a@]@."i.name pp_arities !v ) arities_tbl ()

let pp_alias_tbl fmt (tbl : Lam_stats.alias_tbl) = 
  Ident_hashtbl.iter (fun k v -> pp fmt "@[%a -> %a@]@." Ident.print k Ident.print v)
    tbl


let pp_kind fmt (kind : Lam_stats.kind) = 
  match kind with 
  | ImmutableBlock (arr,_) -> 
    pp fmt "Imm(%d)" (Array.length arr)
  | MutableBlock (arr) ->     
    pp fmt "Mutable(%d)" (Array.length arr)
  | Constant _  ->
    pp fmt "Constant"
  | Module id -> 
    pp fmt "%s/%d" id.name id.stamp 
  | Function _ -> 
    pp fmt "function"
  | Exception ->
    pp fmt "Exception" 
  | Parameter -> 
    pp fmt "Parameter"  
  | NA -> 
    pp fmt "NA"

let pp_ident_tbl fmt (ident_tbl : Lam_stats.ident_tbl) = 
  Ident_hashtbl.iter (fun k v -> pp fmt "@[%a -> %a@]@." 
    Ident.print k pp_kind v)
    ident_tbl
      
let merge 
    ((n : int ), params as y)
    (x : Lam.function_arities) : Lam.function_arities = 
  match x with 
  | NA -> Determin(false, [y], false)
  | Determin (b,xs,tail) -> Determin (b, y :: xs, tail)

(* we need record all aliases -- since not all aliases are eliminated, 
   mostly are toplevel bindings
   We will keep iterating such environment
   If not found, we will return [NA]
*)
let rec get_arity 
    (meta : Lam_stats.meta) 
    (lam : Lam.t) : 
  Lam.function_arities = 
  match lam with 
  | Lconst _ -> Determin (true,[], false)
  | Lvar v -> 
    (** for functional parameter, if it is a high order function,
        if it's not from function parameter, we should warn
    *)
    begin 
      match Ident_hashtbl.find_opt meta.ident_tbl v with 
      | Some (Function {arity;_}) -> arity
      | Some _
      | None ->
        (* Format.fprintf Format.err_formatter *)
        (*   "@[%s %a is not function/functor@]@." meta.filename Ident.print v ; *)
        (NA : Lam.function_arities)

    end
  | Llet(_,_,_, l ) -> get_arity meta l 

  (*   begin match Parsetree_util.has_arity prim_attributes with *)
  (*     | Some arity ->  *)
  (*       (\* Ext_log.dwarn __LOC__ "arity %d" arity; *\) *)
  (*       Determin(false, [arity, None], false) *)
  (*     | None -> NA *)
  (*   end *)
  | Lprim {primitive = Pfield (n,_); 
           args =  [ Lglobal_module id  ]; _} ->
    Lam_compile_env.find_and_add_if_not_exist (id, n) meta.env
      ~not_found:(fun _ -> assert false)
      ~found:(fun x -> x.arity )
  | Lprim {primitive = Pfield _; _} -> NA (** TODO *)
  | Lprim {primitive = Praise ;  _} -> Determin(true,[], true)
  | Lprim {primitive = Pccall _; _} -> Determin(false, [], false)
  | Lglobal_module _ (* TODO: fix me never going to happen assert false  *)
  | Lprim _  -> Determin(true,[] ,false)
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
  | Lletrec(_, body) ->
    get_arity meta body
  (* | Lapply(Lprim( p, _), _args, _info) -> *)
  (*     Determin(true, [], false) (\** Invariant : primtive application is always complete.. *\) *)

  | Lapply{fn = app;  args; _ } -> (* detect functor application *)
    let fn = get_arity meta app in 
    begin match fn with 
      | NA -> NA 
      | Determin (b, xs, tail ) -> 
        let rec take (xs : _ list) arg_length = 
          match xs with 
          | (x,y) :: xs ->
            if arg_length = x then Lam.Determin (b, xs, tail) 
            else if arg_length > x then
              take xs (arg_length - x)
            else Determin (b, 
                           ((x -  arg_length ), 
                            (match y with
                            | Some y -> Some (Ext_list.drop arg_length y) 
                            | None -> None)) :: xs ,
                           tail)
          | [] -> 
            if tail then Determin(b, [], tail)
            else if not b then 
              NA
            else NA
            (* Actually, you can not have truly deministic arities
               for example [fun x -> x ]
            *)
              (* Ext_pervasives.failwithf ~loc:__LOC__ "%s %s" *)
              (*   (Format.asprintf "%a" pp_arities fn)  *)
              (*   (Lam_util.string_of_lambda lam) *)
        in
        take xs (List.length args) 
    end
  | Lfunction {arity; function_kind; params; body = l} -> 
    merge (arity, Some params)  (get_arity meta l)
  | Lswitch(l, {sw_failaction; 
                sw_consts; 
                sw_blocks;
                sw_numblocks = _;
                sw_numconsts = _;
               }) -> 
    all_lambdas meta (
      let rest = (sw_consts |> List.map snd) @ (sw_blocks |> List.map snd ) in
      match sw_failaction with None -> rest | Some x -> x::rest )
  | Lstringswitch(l, sw, d) -> 
    begin match d with 
      | None -> all_lambdas meta (List.map snd  sw )
      | Some v -> all_lambdas meta (v:: List.map snd  sw)
    end
  | Lstaticraise _ -> NA (* since it will not be in tail position *)
  | Lstaticcatch(_, _, handler) -> get_arity meta handler
  | Ltrywith(l1, _, l2) -> 
    all_lambdas meta [l1;l2]
  | Lifthenelse(l1, l2, l3) ->
    all_lambdas meta [l2;l3]
  | Lsequence(_, l2) -> get_arity meta l2 
  | Lsend(u, m, o, ll, v) -> NA
  | Lifused(v, l) -> NA 
  | Lwhile _ 
  | Lfor _  
  | Lassign _ -> Determin(true,[], false)

and all_lambdas meta (xs : Lam.t list) = 
  match xs with 
  | y :: ys -> 
    let arity =  get_arity meta y in 
    List.fold_left (fun exist (v : Lam.t) -> 
        match (exist : Lam.function_arities) with 
        | NA -> NA 
        | Determin (b, xs, tail) -> 
          begin 
            match get_arity meta v with 
            | NA -> NA 
            | Determin (u,ys,tail2) -> 
              let rec aux (b,acc) xs ys = 
                match xs,ys with
                | [], [] -> (b, List.rev acc, tail && tail2) 
                | [], y::ys when tail  -> 
                  aux (b,y::acc) [] ys 
                | x::xs, [] when tail2 -> 
                  aux (b,x::acc) [] xs
                | x::xs, y::ys when x = y -> aux (b, (y :: acc)) xs ys 
                | _, _  -> (false, List.rev acc, false) in 
              let (b,acc, tail3)  = aux ( u &&b, []) xs ys in 
              Determin (b,acc, tail3)
          end
      ) arity ys 
  | _ -> assert false 

(*
let dump_exports_arities (meta : Lam_stats.meta ) = 
  let fmt = 
    if meta.filename != "" then 
      let cmj_file = Ext_filename.chop_extension meta.filename ^ Js_config.cmj_ext in
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

