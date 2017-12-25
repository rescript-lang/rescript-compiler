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

type option_unwrap_time =
  | Static_unwrapped
  | Runtime_maybe_unwrapped

(**
  Invrariant: 
  - optional encoding
  -  None encoding

  when no argumet is supplied, [undefined] 
  if we detect that all rest arguments are [null], 
  we can remove them


  - avoid duplicate evlauation of [arg] when it
   is not a variable
  {!Js_ast_util.named_expression} does not help 
   since we need an expression here, it might be a statement
*)
let get_default_undefined
    ?(map=((fun _ x -> x) : option_unwrap_time -> J.expression -> J.expression))
    (arg : J.expression)
    : J.expression =
  match arg.expression_desc with
  | Number _ -> E.undefined
  | Array ([x],_)
  | Caml_block([x],_,_,_) -> (map Static_unwrapped x) (* invariant: option encoding *)
  | _ ->
    if Js_analyzer.is_okay_to_duplicate arg then
      E.econd arg (map Static_unwrapped (E.index arg 0l)) E.undefined
    else
      map Runtime_maybe_unwrapped (E.runtime_call Js_runtime_modules.js_primitive "option_get" [arg])

(** Another way: 
    {[
      | Var _  ->
        can only bd detected at runtime thing
          (E.triple_equal (E.typeof arg)
             (E.str "number"))
    ]}
*)
let none : J.expression = 
  {expression_desc = Number (Int {i = 0l; c  = None}); comment = Some "None" }

let some x : J.expression = 
  {expression_desc = Caml_block ( [x], Immutable, E.zero_int_literal , Blk_constructor ("Some",1) );
   comment = None}







