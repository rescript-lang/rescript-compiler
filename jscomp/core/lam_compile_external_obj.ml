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




module E = Js_exp_make


(* Note: can potentially be inconsistent, sometimes 
   {[
     { x : 3 , y : undefined}
   ]}
   and 
   {[
     {x : 3 }
   ]}
   But the default to be undefined  seems reasonable 
*)
  
let assemble_args_obj (labels : Ast_arg.kind list) (args : J.expression list) = 
  let rec aux (labels : Ast_arg.kind list) args = 
    match labels, args with 
    | [] , [] as empty_pair -> empty_pair
    | {arg_label = Label (label, Some cst )} :: labels  , args -> 
      let accs, eff = aux labels args in 
      (Js_op.Key label, Lam_compile_const.translate_arg_cst cst )::accs, eff 
    | {arg_label = Empty (Some _) } :: rest  , args -> assert false 
    | {arg_label = Empty None }::labels, arg::args 
      ->  
      let (accs, eff) as r  = aux labels args in 
      if Js_analyzer.no_side_effect_expression arg then r 
      else (accs, arg::eff)
    | ({arg_label = Label (label,None)  } as arg_kind)::labels, arg::args 
      -> 
      let accs, eff = aux labels args in 
      let acc, new_eff = Lam_compile_external_call.ocaml_to_js_eff arg_kind arg in 
      begin match acc with 
        | [ ] -> assert false
        | x::xs -> 
          (Js_op.Key label, E.fuse_to_seq x xs ) :: accs , new_eff @ eff 
      end (* evaluation order is undefined *)

    | ({arg_label = Optional label } as arg_kind)::labels, arg::args 
      -> 
      let (accs, eff) as r = aux labels args  in 
      begin match arg.expression_desc with 
        | Number _ -> (*Invariant: None encoding*)
          r
        | _ ->                 
          let acc, new_eff = Lam_compile_external_call.ocaml_to_js_eff arg_kind arg in 
          begin match acc with 
            | [] -> assert false 
            | x::xs -> 
              (Js_op.Key label, E.fuse_to_seq x xs)::accs , 
              new_eff @ eff 
          end 
      end

    | {arg_label = Empty None | Label (_,None) | Optional _  } :: _ , [] -> assert false 
    | [],  _ :: _  -> assert false 
  in 
  let map, eff = aux labels args in 

  match eff with
  | [] -> 
    E.obj map 
  | x::xs -> E.seq (E.fuse_to_seq x xs) (E.obj map)

