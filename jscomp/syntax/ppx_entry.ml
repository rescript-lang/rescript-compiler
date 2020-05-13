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

let unsafe_mapper = Bs_builtin_ppx.mapper
  
let rewrite_signature (ast : Parsetree.signature) : Parsetree.signature =  
  Bs_ast_invariant.iter_warnings_on_sigi ast;  
  Ast_config.iter_on_bs_config_sigi ast; 
  let ast = 
    match !Js_config.jsx_version with
    | 2 -> Reactjs_jsx_ppx_v2.rewrite_signature ast 
    | 3 -> Reactjs_jsx_ppx_v3.rewrite_signature ast 
    | _ -> ast 
    (* react-jsx ppx relies on built-in ones like `##` *)
  in 
  if !Js_config.no_builtin_ppx then ast else 
    let result =  
      unsafe_mapper.signature  unsafe_mapper ast in
    (* Keep this check, since the check is not inexpensive*)
    Bs_ast_invariant.emit_external_warnings_on_signature result;
    result


  

let rewrite_implementation (ast : Parsetree.structure) : Parsetree.structure =   
  Bs_ast_invariant.iter_warnings_on_stru ast ;   
  Ast_config.iter_on_bs_config_stru ast ;   
  let ast = 
    match !Js_config.jsx_version with 
    | 2 -> Reactjs_jsx_ppx_v2.rewrite_implementation ast 
    | 3 -> Reactjs_jsx_ppx_v3.rewrite_implementation ast 
    | _ -> ast 
  in 
  if !Js_config.no_builtin_ppx then ast else
    begin
      let result =
        unsafe_mapper.structure  unsafe_mapper ast  in
      (* Keep this check since it is not inexpensive*)
      Bs_ast_invariant.emit_external_warnings_on_structure result;
      result
    end
