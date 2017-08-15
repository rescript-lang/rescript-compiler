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
module S = Js_stmt_make

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

(* TODO: check stackoverflow *)
let assemble_args_obj (labels : Ast_arg.kind list)  (args : J.expression list) 
  : J.block * J.expression = 
   let rec aux (labels : Ast_arg.kind list) args 
    : (Js_op.property_name * E.t ) list  * J.expression list * _ = 
    match labels, args with 
    | [] , []  ->  [], [], []
    | {arg_label = Label (label, Some cst )} :: labels  , args -> 
      let accs, eff, assign = aux labels args in 
      (Key label, Lam_compile_const.translate_arg_cst cst )::accs, eff, assign 
    | {arg_label = Empty (Some _) } :: rest  , args -> assert false 
    | {arg_label = Empty None }::labels, arg::args 
      ->  (* unit type*)
      let (accs, eff, assign) as r  = aux labels args in 
      if Js_analyzer.no_side_effect_expression arg then r 
      else (accs, arg::eff, assign)
    | ({arg_label = Label (label,None)  } as arg_kind)::labels, arg::args 
      -> 
      let accs, eff, assign = aux labels args in 
      let acc, new_eff = Lam_compile_external_call.ocaml_to_js_eff arg_kind arg in 
      begin match acc with 
        | [ ] -> assert false
        | x::xs -> 
          (Key label, E.fuse_to_seq x xs ) :: accs , new_eff @ eff , assign
      end (* evaluation order is undefined *)

    | ({arg_label = Optional label; arg_type } as arg_kind)::labels, arg::args 
      -> 
      let (accs, eff, assign) as r = aux labels args  in 
      begin match arg.expression_desc with 
        | Number _ -> (*Invariant: None encoding*)
          r
        | Array ([x],_)
        | Caml_block ([x],_,_,_) ->
          let acc, new_eff = Lam_compile_external_call.ocaml_to_js_eff 
            ({Ast_arg.arg_label = Ast_arg.label label None; arg_type}) x in 
          begin match acc with 
          | [] -> assert false 
          | x::xs -> 
            (Key label, E.fuse_to_seq x xs ) :: accs , new_eff @ eff , assign
          end   
        | _ ->                 
          accs, eff , (arg_kind,arg)::assign 
      end
    | {arg_label = Empty None | Label (_,None) | Optional _  } :: _ , [] -> assert false 
    | [],  _ :: _  -> assert false 
  in 
  let map, eff, assignment = aux labels args in 
  match assignment with 
  | [] -> 
    [],  begin  match eff with
      | [] -> 
        E.obj map 
      | x::xs -> E.seq (E.fuse_to_seq x xs) (E.obj map)
    end
  | _ ->     
    let v  = Ext_ident.create_tmp () in 
    let var_v = E.var v in 
    S.define ~kind:Variable v 
    (begin match eff with
      | [] -> 
        E.obj map 
      | x::xs -> E.seq (E.fuse_to_seq x xs) (E.obj map)     
    end) :: 
      (Ext_list.flat_map (fun 
        ((label : Ast_arg.kind), (arg  : J.expression )) -> 
      match label with 
      | {arg_label = Optional label } -> 
        (* Need make sure whether assignment is effectful or not
          to avoid code duplication
        *)
        begin match Js_ast_util.named_expression arg with 
        | None ->
          [S.if_ arg [S.exp (E.assign (E.dot var_v label) 
            (E.index arg 0l) ) ] ] 
        | Some (st,id) ->
          let var_id = E.var id in         
          st ::  
            [S.if_ var_id [S.exp (E.assign (E.dot var_v label) 
              (E.index var_id 0l)) ]]
        end 
      |  _ -> assert false    
      )
      assignment)
    , var_v                    
