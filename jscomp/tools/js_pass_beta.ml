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






(* When we inline a function call, if we don't do a beta-reduction immediately, there is 
   a chance that it is ignored, (we can not assume that each pass is robust enough)

   After we do inlining, it makes sense to do another constant folding and propogation 
 *)

(* Check: shall we inline functions with while loop? if it is used only once, 
   it makes sense to inline it
*)

module S = Js_stmt_make
module E = Js_exp_make


(** Update ident info use cases, it is a non pure function, 
    it will annotate [program] with some meta data
    TODO: Ident Hashtbl could be improved, 
    since in this case it can not be global?  

 *)

type inline_state = 
  | False
  | Inline_ignore of bool (*indicate whether use [break] to replace [return] or not *)
  | Inline_ret of J.expression * bool 
  | Inline_return 

let pass_beta = 
  object (self)
    inherit Js_map.map as super
    val inline_state = False 
    method with_inline_state x = 
      {<inline_state = x>}
    method! block bs = 
      match bs with 
      | {statement_desc = Block bs ; _} :: rest 
        -> 
          self#block (bs @  rest )
      | {statement_desc = Exp (
         {expression_desc = Call (
          {expression_desc = Fun (false, params, body, env) },
          args, _info) ; _ }) ; _ } :: rest (* TODO: don't inline tailcall for method yet*)
           when Ext_list.same_length args  params ->
             let body = self#block body in
             (Ext_list.fold_right2
              (fun p a acc ->
                S.define ~kind:Variable p a :: acc) params args
                 ((self#with_inline_state (Inline_ignore (Js_fun_env.is_tailcalled env))) #block  body) ) @
              (self#block rest)
      | {statement_desc = Exp (
         {expression_desc = Bin (Eq,
                                 e,
                                  {expression_desc = Call (
                                  {expression_desc = Fun (false, params, body, env) },
                                  args, _info) ; _ }); _
        }) ; _ } :: rest when Ext_list.same_length args params ->

          let body = self#block body  in
              (Ext_list.fold_right2
              (fun p a acc ->
                S.define ~kind:Variable p a :: acc) params args
                 ((self#with_inline_state (Inline_ret (e, Js_fun_env.is_tailcalled env))) #block body) ) @
              (self#block rest)

      | {statement_desc = Return (
         {return_value = {expression_desc = Call (
          {expression_desc = Fun (false,params, body, _) },
          args, _info) ; _ }}) ; _ } :: rest 
        when Ext_list.same_length args  params -> 
          let body = self#block body in
          (Ext_list.fold_right2 
                 (fun p a acc -> 
                   S.define ~kind:Variable p a :: acc) params args 
                 ((self#with_inline_state Inline_return) #block body))  @ 
              (self#block rest)

      | {statement_desc = Return {return_value = e} } as st  :: rest 
        -> 
          begin match inline_state with 
          | False 
            -> self#statement st :: self#block rest 
          | Inline_ignore b 
            ->
              S.exp (self#expression e) :: 
              (if b then S.break () :: self#block rest else self#block rest)
          | Inline_ret (v,b) ->
              S.exp (E.assign v (self#expression e)) :: 
              (if b then S.break () :: self#block rest else self#block rest)
          | Inline_return 
            -> 
              S.return (self#expression e ) :: self#block rest 
          end
      | x :: xs  -> self#statement x :: self#block xs
      | [] -> []
    method! expression e = 
      match e.expression_desc with 
      | Fun (params, body,env) ->  
         (** only  partial information of env is wrong,  needs  to be recomputed again,
             but we can not simply discard it 
          *)
          {e with expression_desc = Fun (params, ({< inline_state = False >}# block body), env)}

       (** so that we will not have inner [Return] *)
      | _ -> super#expression e 
  end
