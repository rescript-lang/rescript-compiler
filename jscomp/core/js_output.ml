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

type finished = 
  | True 
  | False 
  | Dummy (* Have no idea, so that when [++] is applied, always use the other *)

type t  =  { 
  block : J.block ;
  value : J.expression option;
  finished : finished ; 

}

type continuation = Lam_compile_context.continuation

let make ?value ?(finished=False) block =
    { block ; value ; finished }


let dummy = 
    {value = None; block = []; finished = Dummy }

let output_of_expression 
    (continuation : continuation)
    (should_return : Lam_compile_context.return_type)
    (lam : Lam.t) (exp : J.expression) : t =
  begin match continuation, should_return with 
  | EffectCall, ReturnFalse -> 
      if Lam_analysis.no_side_effects lam 
      then dummy
      else {block = []; value  = Some exp ; finished = False}
  | Declare (kind, n), ReturnFalse -> 
      make [ S.define_variable ~kind n  exp]
  | Assign n ,ReturnFalse -> 
      make [S.assign n exp ]
  | EffectCall, ReturnTrue _ ->
      make [S.return_stmt  exp] ~finished:True    
  | (Declare _ | Assign _ ), ReturnTrue _ -> 
      make [S.unknown_lambda lam] ~finished:True
  | NeedValue, _ -> 
    {block = []; value = Some exp; finished = False }
  end

let output_of_block_and_expression 
    (continuation : continuation) 
    (should_return : Lam_compile_context.return_type)
    (lam : Lam.t) (block : J.block) exp : t = 
  match continuation, should_return with 
  | EffectCall, ReturnFalse -> make block ~value:exp
  | Declare (kind,n), ReturnFalse -> 
    make (block @ [ S.define_variable ~kind  n exp])
  | Assign n, ReturnFalse -> make (block @ [S.assign n exp])  
  | EffectCall, ReturnTrue _ -> make (block @ [S.return_stmt exp]) ~finished:True
  | (Declare _ | Assign _), ReturnTrue _ ->
    make [S.unknown_lambda lam] ~finished:True
  | NeedValue, (ReturnTrue _ | ReturnFalse) -> 
    make block ~value:exp



let block_with_opt_expr block (x : J.expression option) : J.block =   
    match x with 
    | None  -> block
    | Some x when Js_analyzer.no_side_effect_expression x -> block
    | Some x -> block @ [S.exp x ]

let opt_expr_with_block (x : J.expression option) block : J.block =     
    match x with 
    | None  -> block
    | Some x when Js_analyzer.no_side_effect_expression x -> block
    | Some x -> (S.exp x) :: block
    

let rec unnest_block (block : J.block) : J.block = 
  match block with 
  | [{statement_desc = Block block}] -> unnest_block block 
  |  _ -> block 

let output_as_block ( x : t)  : J.block = 
  match x with 
  | {block; value = opt; finished} ->
      let block = unnest_block block in
      if finished = True  then block
      else 
        block_with_opt_expr block opt
   

let to_break_block (x : t) : J.block * bool = 
    let block = unnest_block x.block in 
    match x with 
    | {finished = True;  _ } -> 
        block, false
       (* value does not matter when [finished] is true
           TODO: check if it has side efects
        *)
    | { value =  None; finished } -> 
        block, 
        (match finished with | True -> false | (False | Dummy)  -> true  )

    | {value = Some _ as opt; _} -> 
        block_with_opt_expr block opt, true


(** TODO: make everything expression make inlining hard, and code not readable?
           1. readability dpends on how we print the expression 
           2. inlining needs generate symbols, which are statements, type mismatch
              we need capture [Exp e]

           can we call them all [statement]? statement has no value 
        *)
(* | {block = [{statement_desc = Exp e }]; value = None ; _}, _ *)
(*   -> *)
(*     append { x with block = []; value = Some e} y *)
(* |  _ , {block = [{statement_desc = Exp e }]; value = None ; _} *)
(*   -> *)
(*     append x { y with block = []; value = Some e} *)
        
let rec append_output  (x : t ) (y : t ) : t =  
    match x , y with (* ATTTENTION: should not optimize [opt_e2], it has to conform to [NeedValue]*)
    | {finished = True; _ }, _ -> x  
    | _, {block = []; value= None; finished = Dummy } -> x 
          (* finished = true --> value = E.undefined otherwise would throw*)
    | {block = []; value= None; _ }, y  -> y 
    | {block = []; value= Some _; _}, {block = []; value= None; _ } -> x 
    | {block = []; value =  Some e1; _}, ({block = []; value = Some e2; finished } as z) -> 
        if Js_analyzer.no_side_effect_expression e1 
        then z
            (* It would optimize cases like [module aliases]
                Bigarray, List 
             *)
        else
          {block = []; value = Some (E.seq e1 e2); finished}
          (* {block = [S.exp e1]; value =  Some e2(\* (E.seq e1 e2) *\); finished} *)

    | {block = block1; value = opt_e1; _},  {block = block2; value = opt_e2; finished} -> 
        let block1 = unnest_block block1 in
        make (block1 @ (opt_expr_with_block opt_e1  @@ unnest_block block2))
          ?value:opt_e2 ~finished




(* Fold right is more efficient *)
let concat (xs : t list) : t = 
  Ext_list.fold_right (fun x acc -> append_output x  acc) xs dummy

let to_string x   = 
  Js_dump.string_of_block (output_as_block x)
