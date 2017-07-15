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

(* we need destruct [undefined] when input is optional *)
let eval (arg : J.expression) (dispatches : (int * string) list ) : E.t = 
  if arg == E.undefined then E.undefined else
  match arg.expression_desc with
  | Number (Int {i} | Uint i) -> 
    E.str (Ext_list.assoc_by_int None (Int32.to_int i) dispatches) 
  | _ ->  
    E.of_block
      [(S.int_switch arg
      (List.map (fun (i,r) -> 
              {J.case = i ; 
               body = [S.return (E.str r)],
                      false (* FIXME: if true, still print break*)
              }) dispatches))]

(** invariant: optional is not allowed in this case *)
let eval_as_event (arg : J.expression) (dispatches : (int * string) list ) : E.t list  = 
  match arg.expression_desc with
  | Array ([{expression_desc = Number (Int {i} | Uint i)}; cb], _)
  | Caml_block([{expression_desc = Number (Int {i} | Uint i)}; cb], _, _, _)
    -> 
    let v = Ext_list.assoc_by_int None (Int32.to_int i) dispatches in [E.str v ; cb ]   
  | _ ->  
    let event = Ext_ident.create "action" in
    [
      E.ocaml_fun [event]
      [(S.int_switch arg
      (List.map (fun (i,r) -> 
              {J.case = i ; 
               body = [S.return (E.index (E.var event) 0l)],
                      false (* FIXME: if true, still print break*)
              }) dispatches))]
      ; (* TODO: improve, one dispatch later, 
           the problem is that we can not create bindings 
           due to the 
        *)
      E.ocaml_fun [event]
      [(S.int_switch arg
      (List.map (fun (i,r) -> 
              {J.case = i ; 
               body = [S.return (E.index (E.var event) 1l)],
                      false (* FIXME: if true, still print break*)
              }) dispatches))]
    ]

(* we need destruct [undefined] when input is optional *)
let eval_as_int (arg : J.expression) (dispatches : (int * int) list ) : E.t  = 
  if arg == E.undefined then E.undefined else 
  match arg.expression_desc with
  | Number (Int {i} | Uint i) ->
    E.int (Int32.of_int (Ext_list.assoc_by_int None (Int32.to_int i) dispatches))    
  | _ ->  
    E.of_block
      [(S.int_switch arg
      (List.map (fun (i,r) -> 
              {J.case = i ; 
               body = [S.return (E.int (Int32.of_int  r))],
                      false (* FIXME: if true, still print break*)
              }) dispatches))]

let eval_as_unwrap (arg : J.expression) : E.t =
  if arg == E.undefined then
    E.undefined
  else
    match arg.expression_desc with
    | Caml_block ([{expression_desc = Number _}; cb], _, _, _) ->
      cb
    | _ ->
      E.index (arg) 1l
