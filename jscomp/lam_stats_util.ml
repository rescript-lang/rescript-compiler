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

let pp_arities (fmt : Format.formatter) (x : Lam_stats.function_arities) = 
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
    (arities_tbl : (Ident.t, Lam_stats.function_arities ref) Hashtbl.t) = 
  Hashtbl.fold (fun (i:Ident.t) (v : Lam_stats.function_arities ref) _ -> 
      pp Format.err_formatter "@[%s -> %a@]@."i.name pp_arities !v ) arities_tbl ()

let pp_alias_tbl fmt (tbl : Lam_stats.alias_tbl) = 
  Hashtbl.iter (fun k v -> pp fmt "@[%a -> %a@]@." Ident.print k Ident.print v)
    tbl

let merge 
    ((n : int ), params as y)
    (x : Lam_stats.function_arities) : Lam_stats.function_arities = 
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
    (lam : Lambda.lambda) : 
  Lam_stats.function_arities = 
  match lam with 
  | Lconst _ -> Determin (true,[], false)
  | Lvar v -> 
    (** for functional parameter, if it is a high order function,
        if it's not from function parameter, we should warn
    *)
    begin 
      match Hashtbl.find meta.ident_tbl v with 
      | exception Not_found -> (NA : Lam_stats.function_arities) 
      | Function {arity;_} -> arity
      | _ ->
        (* Format.fprintf Format.err_formatter *)
        (*   "@[%s %a is not function/functor@]@." meta.filename Ident.print v ; *)
        (NA : Lam_stats.function_arities)

    end
  | Llet(_,_,_, l ) -> get_arity meta l 
  (* | Lprim (Pccall {prim_name = "js_pure_expr"; prim_attributes},  *)
  (*          [Lconst (Const_base (Const_string (_str,_)))]) *)
  (*   -> *)
  (*   (\* Ext_log.dwarn __LOC__ "called %s %d" str (List.length prim_attributes ); *\) *)
  (*   begin match Parsetree_util.has_arity prim_attributes with *)
  (*     | Some arity ->  *)
  (*       (\* Ext_log.dwarn __LOC__ "arity %d" arity; *\) *)
  (*       Determin(false, [arity, None], false) *)
  (*     | None -> NA *)
  (*   end *)
  | Lprim (Pfield (n,_), [Lprim(Pgetglobal id,[])]) ->
    Lam_compile_env.find_and_add_if_not_exist (id, n) meta.env
      ~not_found:(fun _ -> assert false)
      ~found:(fun x -> x.arity )
  | Lprim (Pfield _, _ ) -> NA (** TODO *)
  | Lprim (Praise _, _ ) -> Determin(true,[], true)
  | Lprim (Pccall _, _) -> Determin(false, [], false)
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

  | Lapply(app, args, _info) -> (* detect functor application *)
    let fn = get_arity meta app in 
    begin match fn with 
      | NA -> NA 
      | Determin (b, xs, tail ) -> 
        let rec take (xs : _ list) arg_length = 
          match xs with 
          | (x,y) :: xs ->
            if arg_length = x then Lam_stats.Determin (b, xs, tail) 
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
  | Lfunction(kind, params, l) -> 
    let n = List.length params in 
    merge (n, Some params)  (get_arity meta l)
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
  | Levent(l, event) -> NA
  | Lifused(v, l) -> NA 
  | Lwhile _ 
  | Lfor _  
  | Lassign _ -> Determin(true,[], false)

and all_lambdas meta (xs : Lambda.lambda list) = 
  match xs with 
  | y :: ys -> 
    let arity =  get_arity meta y in 
    List.fold_left (fun exist (v : Lambda.lambda) -> 
        match (exist : Lam_stats.function_arities) with 
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

