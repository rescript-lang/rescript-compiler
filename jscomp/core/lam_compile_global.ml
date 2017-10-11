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

open Js_output.Ops

(* TODO: used in functor inlining, so that it can not be an exception
   Make(S), S can not be an exception
*)



let expand_global_module_as_lam id env = 
  Lam_compile_env.query_and_add_if_not_exist 
    (Lam_module_ident.of_ml id) 
    (Has_env env)
    ~not_found:(fun id -> assert false)
    ~found:(fun {signature ; _} 
             -> 
               Lam.prim
                 ~primitive:(Pmakeblock(0, Blk_module None, Immutable))  
                 ~args:(
                   let len = Ocaml_types.length signature in 
                   Ext_list.init len (fun i  -> 
                       Lam.prim
                         ~primitive:(Pfield (i, Lambda.Fld_na)) 
                         ~args:[ Lam.global_module id  ] Location.none)
                 )
                 Location.none (* FIXME*))


(* Given an module name,  find its expanded structure  *)  
let expand_global_module  id env  : J.expression = 
  Lam_compile_env.query_and_add_if_not_exist 
    (Lam_module_ident.of_ml id) 
    (Has_env env)
    ~not_found:(fun _ -> assert false)
    ~found:(fun   {signature; _} -> 
        Js_of_lam_module.make ~comment:id.name 
          (Ocaml_types.map (fun name -> E.ml_var_dot id name) signature )
      )



