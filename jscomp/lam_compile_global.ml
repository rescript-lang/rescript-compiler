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



module E = Js_helper.Exp  
module S = Js_helper.Stmt

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
   (id, env, expand) -> 
    if Ident.is_predef_exn id 
    then 
      begin 
        E.runtime_ref Js_config.builtin_exceptions id.name
      end
    else 
      Lam_compile_env.query_and_add_if_not_exist (Lam_module_ident.of_ml id) env
        ~not_found:(fun id -> assert false)
        ~found:(fun   {signature = sigs; _} -> 
            if expand 
            then 
              (** TODO: add module into taginfo*)
              let len = List.length sigs in (** TODO: could be optimized *) 
              Js_of_lam_module.make ~comment:id.name 
                (Ext_list.init len (fun i -> 
                     E.ml_var_dot id
                       (Type_util.get_name sigs i )))
                               

            else 
              E.ml_var id)

  

