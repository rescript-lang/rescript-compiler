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



module S = J_helper.Stmt
module E = J_helper.Exp


let count_collects () = 
  object (self)
    inherit Js_fold.fold as super
    val stats = Hashtbl.create 83
    method use id = 
      match Hashtbl.find stats id with
      | exception Not_found -> Hashtbl.add stats id (ref 1)
      | v -> incr v 
    method! variable_declaration vd=  
      match vd with 
      | {ident = _; value = None ; _} -> self
      | {ident = _; value = Some x; _} -> self#expression x 
    method! ident id = self#use id; self
    method get_stats = stats
  end

let subst export_set (stats : (Ident.t, int ref) Hashtbl.t) = 
  object (self)
    inherit Js_map.map as super
    val subst = Hashtbl.create 83 
    method! block bs = 
      match bs with
      | ({statement_desc = 
            Variable ({value = Some ({expression_desc = Fun _; _} as v )} as vd); _} as st) :: rest  -> 
        let is_export = Ident_set.mem vd.ident export_set in
        if is_export then 
          super#statement st :: self#block rest 
        else 
          begin 
            match Hashtbl.find stats vd.ident with
            | exception Not_found -> 
              if Js_analyzer.no_side_effect_expression v 
              then S.exp v  :: self#block rest 
              else self#block rest 
            | number when !number  = 1 && Js_analyzer.no_side_effect_expression v -> 
                (** 1. recursive value ? let rec x = 1 :: x 
                    non-terminating
                    2. duplicative identifiers ..
                    remove it at the same time is a bit unsafe, 
                    since we have to guarantee that the one use
                    case is substituted
                 *)
                let v' = self#expression v in
                Hashtbl.add subst vd.ident v';
                self#block rest
            | _ -> super#statement st  :: self#block rest 
            end
          
      | x :: xs ->
          self#statement x :: self#block xs
      | [] -> []
    method! expression e =
      match e.expression_desc with
      | Var (Id id) -> 
        begin match Hashtbl.find subst id with
          | exception Not_found -> e 
          | v -> 
              self#expression v 
        end
      | _ -> super#expression e 
  end

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
          {expression_desc = Fun (params, body, env) },
          args, _info) ; _ }) ; _ } :: rest 
           when Ext_list.same_length args  params ->
             let body = self#block body in
             (List.fold_right2
              (fun p a acc ->
                S.define ~kind:Variable p a :: acc) params args
                 ((self#with_inline_state (Inline_ignore (Js_fun_env.is_tailcalled env))) #block  body) ) @
              (self#block rest)
      | {statement_desc = Exp (
         {expression_desc = Bin (Eq,
                                 e,
                                  {expression_desc = Call (
                                  {expression_desc = Fun (params, body, env) },
                                  args, _info) ; _ }); _
        }) ; _ } :: rest when Ext_list.same_length args params ->

          let body = self#block body  in
              (List.fold_right2
              (fun p a acc ->
                S.define ~kind:Variable p a :: acc) params args
                 ((self#with_inline_state (Inline_ret (e, Js_fun_env.is_tailcalled env))) #block body) ) @
              (self#block rest)

      | {statement_desc = Return (
         {return_value = {expression_desc = Call (
          {expression_desc = Fun (params, body, _) },
          args, _info) ; _ }}) ; _ } :: rest 
        when Ext_list.same_length args  params -> 
          let body = self#block body in
          (List.fold_right2 
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

let inline_and_shake (program : J.program) = 
  let _stats = ((count_collects ()) #program program) #get_stats in
  let _export_set = program.export_set in
  program
  |> (subst _export_set _stats )# program
  |> pass_beta #program
    
