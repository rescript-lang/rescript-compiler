(* BuckleScript compiler
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

(* When we inline a function call, if we don't do a beta-reduction immediately, there is 
   a chance that it is ignored, (we can not assume that each pass is robust enough)

   After we do inlining, it makes sense to do another constant folding and propogation 
 *)

(* Check: shall we inline functions with while loop? if it is used only once, 
   it makes sense to inline it
*)

module S = Js_stmt_make
module E = Js_exp_make


let count_collects () = 
  object (self)
    inherit Js_fold.fold as super
    val stats : (Ident.t , int ref ) Hashtbl.t = Hashtbl.create 83
    val defined_idents = Hashtbl.create 83

    val mutable export_set  : Ident_set.t = Ident_set.empty
    val mutable name : string = ""

    method add_use id = 
      match Hashtbl.find stats id with
      | exception Not_found -> Hashtbl.add stats id (ref 1)
      | v -> incr v 
    method! program x = 
      export_set <- x.export_set ; 
      name <- x.name;
      super#program x
    method! variable_declaration 
        ({ident; value ; property  ; ident_info }  as v)
      =  
      begin
        Hashtbl.add defined_idents ident v; 
        match value with 
        | None
          -> 
          self
        | Some x
          -> self#expression x 
      end
    method! ident id = self#add_use id; self
    method get_stats = 
      Hashtbl.iter (fun ident (v : J.variable_declaration) -> 
          if Ident_set.mem ident export_set then 
            Js_op_util.update_used_stats v.ident_info Exported
          else 
          begin match Hashtbl.find stats ident with 
            | exception Not_found -> 
              let pure = 
                match v.value  with 
                | None -> false  (* can not happen *)
                | Some x -> Js_analyzer.no_side_effect_expression x  in
              Js_op_util.update_used_stats v.ident_info (if pure then Dead_pure else Dead_non_pure)
            | num -> 
              if !num = 1 then 
                let pure = 
                  match v.value  with 
                  | None -> false  (* can not happen *)
                  | Some x -> Js_analyzer.no_side_effect_expression x  in
                Js_op_util.update_used_stats v.ident_info (if pure then Once_pure else Used) 
          end
        ) defined_idents; defined_idents
  end


let get_stats program
  =  ((count_collects ()) #program program) #get_stats


(* 1. recursive value ? let rec x = 1 :: x
    non-terminating
    2. duplicative identifiers ..
    remove it at the same time is a bit unsafe,
    since we have to guarantee that the one use
    case is substituted
    we already have this? in [defined_idents]
*)
(* There is a side effect when traversing dead code, since 
   we assume that substitue a node would mark a node as dead node,
   so if we traverse a dead node, this would get a wrong result.
   it does happen in such scenario
   {[
     let generic_basename is_dir_sep current_dir_name name =
       let rec find_end n =
         if n < 0 then String.sub name 0 1
         else if is_dir_sep name n then find_end (n - 1)
         else find_beg n (n + 1)
       and find_beg n p =
         if n < 0 then String.sub name 0 p
         else if is_dir_sep name n then String.sub name (n + 1) (p - n - 1)
         else find_beg (n - 1) p
       in
       if name = ""
       then current_dir_name
       else find_end (String.length name - 1)
   ]}
   [find_beg] can potentially be expanded in [find_end] and in [find_end]'s expansion, 
   if the order is not correct, or even worse, only the wrong one [find_beg] in [find_end] get expanded 
   (when we forget to recursive apply), then some code non-dead [find_beg] will be marked as dead, 
   while it is still called 
*)
let subst name export_set stats  = 
  object (self)
    inherit Js_map.map as super
    method! statement st = 
      match st with 
      | {statement_desc =
           Variable ({value = _ ;
                      ident_info = {used_stats = Dead_pure}
                     }) ; comment = _}
        ->
        S.block []
      | {statement_desc = Variable { ident_info = {used_stats = Dead_non_pure} ; value = Some v  ; _ } 
        ; _}
        -> S.exp v
      | _ -> super#statement st 
    method! block bs = 
      match bs with
      | ({statement_desc = 
            Variable ({value =
                         Some ({expression_desc = Fun _; _ } as v )
                      } as vd) ; comment = _} as st) :: rest  -> 
        let is_export = Ident_set.mem vd.ident export_set in
        if is_export then 
          self#statement st :: self#block rest 
        else 
          begin 
            match (Hashtbl.find stats vd.ident : J.variable_declaration) with
            | exception Not_found -> 
              if Js_analyzer.no_side_effect_expression v 
              then S.exp v  :: self#block rest 
              else self#block rest 

            | _ -> self#statement st  :: self#block rest 
          end

      | {statement_desc = 
           Return {return_value = {expression_desc = Call({expression_desc = Var (Id id)},args,_info)}} } as st 
        :: rest 
        -> 
        begin match Hashtbl.find stats id with 
          | exception Not_found 
            ->  self#statement st :: self#block rest 

          | { value = Some {expression_desc = Fun (params, block, _env) ; comment = _}; 
              property = (Alias | StrictOpt | Strict);
              ident_info = {used_stats = Once_pure };
              ident = _
            } as v
            when Ext_list.same_length params args 
            -> 
            begin
              (* Ext_log.iwarn false __LOC__ "%s is dead ----- \n" id.name ; *)
              Js_op_util.update_used_stats v.ident_info Dead_pure;
              let block  = 
                List.fold_right2 (fun param arg acc ->  S.define ~kind:Variable param arg :: acc)
                  params args  ( self#block block) in
              (* Mark a function as dead means it will never be scanned, 
                 here we inline the function
              *)
              block @ self#block rest
            end
          | _ ->
            self#statement st :: self#block rest
        end
      | x :: xs 
        ->
        self#statement x :: self#block xs
      | [] 
        -> []

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
  let _stats = get_stats program in
  let _export_set = program.export_set in
  program
  |> (subst program.name _export_set _stats )# program
  (* |> pass_beta #program *)
    
