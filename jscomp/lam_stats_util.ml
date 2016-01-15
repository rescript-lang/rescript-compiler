(* OCamlScript compiler
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
  | Lprim (Pfield n, [Lprim(Pgetglobal id,[])]) ->
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
                           ((x -  arg_length ), Ext_list.drop arg_length y) :: xs ,
                           tail)
          | [] -> 
            if tail then Determin(b, [], tail)
            else if not b then 
              NA
            else
              failwith (Lam_util.string_of_lambda lam)
        in
        take xs (List.length args) 
    end
  | Lfunction(kind, params, l) -> 
    let n = List.length params in 
    merge (n,params)  (get_arity meta l)
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
      let cmj_file = Ext_filename.chop_extension meta.filename ^ ".cmj" in
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

let pp = Format.fprintf 
(* we should exclude meaninglist names and do the convert as well *)

let meaningless_names  = ["*opt*"; "param";]

let rec dump_ident fmt (id : Ident.t) (arity : Lam_stats.function_arities)  = 
  pp fmt  "@[<2>export var %s:@ %a@ ;@]" (Ext_ident.convert id.name ) dump_arity arity

and dump_arity fmt (arity : Lam_stats.function_arities) = 
  match arity with 
  | NA -> pp fmt "any"
  | Determin (_, [], _) -> pp fmt "any"
  | Determin (_, (_,args)::xs, _) -> 
    pp fmt "@[(%a)@ =>@ any@]" 
      (Format.pp_print_list  
         ~pp_sep:(fun fmt _ -> 
             Format.pp_print_string fmt ",";
             Format.pp_print_space fmt ();
           )
         (fun fmt ident -> pp fmt "@[%s@ :@ any@]" 
             (Ext_ident.convert @@ Ident.name ident))
      ) args 

(* Note that 
   [lambda_exports] is 
   lambda expression to be exported
   for the js backend, we compile to js 
   for the inliner, we try to seriaize it -- 
   relies on other optimizations to make this happen
   {[
     exports.Make = function () {.....}
   ]}
*)
let export_to_cmj 
    (meta : Lam_stats.meta ) 
    maybe_pure
    external_ids 
    lambda_exports

  : Js_cmj_format.cmj_table = 
  let values = 
    List.fold_left2 
      (fun acc (x : Ident.t)  (lambda : Lambda.lambda) ->
         let arity =  get_arity meta (Lvar x) in
         let closed_lambda = 
           if Lam_inline_util.maybe_functor x.name 
           then
             if Lambda.IdentSet.is_empty @@ Lambda.free_variables lambda 
             then Some lambda
             else None
           else None in 
         String_map.add x.name  Js_cmj_format.{arity ; closed_lambda } acc 
      )
      String_map.empty
      meta.exports lambda_exports 
  in

  let rec dump fmt ids = 
    (* TODO: also use {[Ext_pp]} module instead *)
    match ids with 
    | [] -> ()
    | x::xs -> 
      dump_ident fmt x (get_arity meta (Lvar x)) ; 
      Format.pp_print_space fmt ();
      dump fmt xs in

  let () =
    if not @@ Ext_string.is_empty meta.filename then
      Ext_pervasives.with_file_as_pp 
        (Ext_filename.chop_extension ~loc:__LOC__ meta.filename ^ ".d.ts")
      @@ fun fmt -> 
        pp fmt "@[<v>%a@]@." dump meta.exports
  in
  let pure = 
    match maybe_pure with
    | None ->  
      Ext_option.bind ( Ext_list.for_all_ret 
                          (fun (id : Lam_module_ident.t) -> 
                             Lam_compile_env.query_and_add_if_not_exist id meta.env 
                               ~not_found:(fun _ -> false ) ~found:(fun i -> 
                                   i.pure)
                          ) external_ids) (fun x -> Lam_module_ident.name x)
    | Some _ -> maybe_pure

  in
  {values; pure }

let find_unused_exit_code  hash_set = 
  let rec aux i = 
    if not @@ Hash_set.mem hash_set i 
    then i 
    else 
    if not @@ Hash_set.mem hash_set (- i) 
    then (- i)
    else aux (i + 1)in
  aux 0 
