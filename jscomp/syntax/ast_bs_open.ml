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


let isCamlExceptionOrOpenVariant : Longident.t = 
  Ldot (Ldot (Lident "Js","Exn"), "isCamlExceptionOrOpenVariant")

  
let obj_magic : Longident.t = 
  Ldot (Lident "Obj", "magic")


let rec checkCases (cases : Parsetree.case list) = 
  List.iter check_case cases 
and check_case case = 
  check_pat case.pc_lhs 
and check_pat (pat : Parsetree.pattern) = 
  match pat.ppat_desc with 
  | Ppat_construct _ -> ()
  | Ppat_or (l,r) -> 
    check_pat l; check_pat r 
  | _ ->  Location.raise_errorf ~loc:pat.ppat_loc "Unsupported pattern in `bs.open`" 

let convertBsErrorFunction loc  
  (self : Bs_ast_mapper.mapper) 
  attrs 
  (cases : Parsetree.case list ) =
  let open Ast_helper in
  let txt  = "match" in 
  let txt_expr = Exp.ident ~loc {txt = Lident txt; loc} in 
  let none = Exp.construct ~loc {txt = Ast_literal.predef_none ; loc} None in
  let () = checkCases cases in  
  let cases = self.cases self cases in 
  Ast_compatible.fun_ ~attrs ~loc ( Pat.var ~loc  {txt; loc })
    (Exp.ifthenelse
    ~loc 
    (Ast_compatible.app1 ~loc (Exp.ident ~loc {txt = isCamlExceptionOrOpenVariant ; loc}) txt_expr )
    (Exp.match_ ~loc 
       (Exp.constraint_ ~loc 
          (Ast_compatible.app1  ~loc (Exp.ident ~loc {txt =  obj_magic; loc})  txt_expr)
          (Ast_literal.type_exn ~loc ())
       )
      (Ext_list.map_append cases 
        [ Exp.case  (Pat.any ~loc ()) none] 
        (fun x ->
           let pc_rhs = x.pc_rhs in 
           let  loc  = pc_rhs.pexp_loc in
           {
             x with pc_rhs = Exp.construct ~loc {txt = Ast_literal.predef_some;loc} (Some pc_rhs)
                        
           })))
    (Some none))
    
                       
