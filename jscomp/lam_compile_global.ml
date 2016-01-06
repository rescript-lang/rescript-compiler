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



module E = J_helper.Exp  
module S = J_helper.Stmt

open Js_output.Ops

(* TODO: used in functor inlining, so that it can not be an exception
   Make(S), S can not be an exception
 *)

let query_lambda id env = 
  Lam_compile_env.query_and_add_if_not_exist (Lam_module_ident.of_ml id) env
    ~not_found:(fun id -> assert false)
    ~found:(fun {signature = sigs; _} -> 
        (* TODO: add module into taginfo*)
        (* let len = List.length sigs in  *)
        (* TODO: could be optimized *)
        Lambda.Lprim (Pmakeblock(0,NA, Immutable) , 
                      (List.mapi (fun i _ -> 
                           Lambda.Lprim(Pfield i, [Lprim(Pgetglobal id,[])])))
                        sigs))

(* Given an module name and position, find its corresponding name  *)  
let get_exp (key : Lam_compile_env.key) : J.expression = 
  match key with 
  | GetGlobal ((id : Ident.t), (pos : int),env) -> 
    Lam_compile_env.find_and_add_if_not_exist (id,pos) env 
      ~not_found:(fun id -> 
          E.str ~pure:false (Printf.sprintf "Err %s %d %d" id.name id.flags pos)
          (* E.index m (pos + 1) *) (** shift by one *)
          (** This can not happen since this id should be already consulted by type checker *)
        )
      ~found:(fun {id; name;_} ->
          match id, name with 
          | {name = "Sys"; _}, "os_type" 
            (** We drop the ability of cross-compiling
                the compiler has to be the same running 
            *)
            ->  E.str Sys.os_type
          | _ -> 
            E.ml_var_dot id name
        ) 

  | QueryGlobal (id, env, expand) -> 
    if Ident.is_predef_exn id 
    then 
      begin 
        E.runtime_ref J_helper.exceptions id.name
      end
    else 
      Lam_compile_env.query_and_add_if_not_exist (Lam_module_ident.of_ml id) env
        ~not_found:(fun id -> assert false)
        ~found:(fun   {signature = sigs; _} -> 
            if expand 
            then 
              (** TODO: add module into taginfo*)
              let len = List.length sigs in (** TODO: could be optimized *) 
              E.arr Immutable (E.int ~comment:id.name 0 ::
                               Ext_list.init len (fun i -> E.ml_var_dot id
                                                     (Type_util.get_name sigs i ))
                              )
            else 
              E.ml_var id)

  | CamlRuntimePrimitive (prim, args)    -> 
    Lam_dispatch_primitive.query prim args


(* TODO: how nested module call would behave,
   In the future, we should keep in track  of if 
   it is fully applied from [Lapply]
   Seems that the module dependency is tricky..
   should we depend on [Pervasives] or not?

   we can not do this correctly for the return value, 
   however we can inline the definition in Pervasives
   TODO:
   [Pervasives.print_endline]
   [Pervasives.prerr_endline]
 *)

let get_exp_with_args (id : Ident.t) (pos : int) env (args : J.expression list) : J.expression = 

  Lam_compile_env.find_and_add_if_not_exist (id,pos) env ~not_found:(fun id -> 
      (** This can not happen since this id should be already consulted by type checker 
          Worst case 
          {[
            E.index m (pos + 1)
          ]}
          shift by one (due to module encoding)
      *)
      E.str ~pure:false  (Printf.sprintf "Err %s %d %d"
                            id.name
                            id.flags
                            pos
                         ))
    ~found:(fun {id; name;arity; _} -> 
        match id, name,  args with 
        | {name = "Pervasives"; _}, "^", [ e0 ; e1] ->  
          E.string_append e0 e1 
        | {name = "Pervasives"; _}, "print_endline", ([ _ ] as args) ->  
          E.seq (E.dump Log args) (E.unit ())
        | {name = "Pervasives"; _}, "prerr_endline", ([ _ ] as args) ->  
          E.seq (E.dump Error args) (E.unit ())
        | _ -> 


          let rec aux (acc : J.expression)
              (arity : Lam_stats.function_arities) args (len : int)  =
            match arity, len with
            | _, 0 -> 
              acc (** All arguments consumed so far *)
            | Determin (a, (x,_) :: rest, b), len   ->
              let x = 
                if x = 0 
                then 1 
                else x in (* Relax when x = 0 *)
              if  len >= x 
              then
                let first_part, continue =  (Ext_list.take x args) in
                aux
                  (E.call ~info:{arity=Full} acc first_part)
                  (Determin (a, rest, b))
                  continue (len - x)
              else  acc 
            (* alpha conversion now? --
               Since we did an alpha conversion before so it is not here
            *)
            | Determin (a, [], b ), _ ->
              (* can not happen, unless it's an exception ? *)
              E.call acc args
            | NA, _ ->
              E.call acc args
          in
          aux (E.ml_var_dot id name) arity args (List.length args )
      )
