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


let is_bs_attribute txt = 
  let len = String.length txt  in
  len >= 2 &&
  (*TODO: check the stringing padding rule, this preciate may not be needed *)
  String.unsafe_get txt 0 = 'b'&& 
  String.unsafe_get txt 1 = 's' &&
  (len = 2 ||
   String.unsafe_get txt 2 = '.'
  )


let emit_external_warnings : Bs_ast_iterator .iterator=
  {
    Bs_ast_iterator.default_iterator with
    attribute = (fun _ a ->
        match a with
        | {txt ; loc}, _ ->
          if is_bs_attribute txt  then
            Location.prerr_warning loc (Bs_unused_attribute txt)
      );
    expr = (fun self a -> 
      match a.Parsetree.pexp_desc with 
      | Pexp_constant (Const_string (_, Some s)) 
        when Ext_string.equal s Literals.unescaped_j_delimiter 
        || Ext_string.equal s Literals.unescaped_js_delimiter -> 
        Bs_warnings.error_unescaped_delimiter a.pexp_loc s 
      | _ -> Bs_ast_iterator.default_iterator.expr self a 
    );
    value_description =
    (fun self v -> 
       match v with 
       | ( {
            pval_loc;
            pval_prim =
               "%identity"::_;
            pval_type
        } : Parsetree.value_description)
         when not
             (Ast_core_type.is_arity_one pval_type)
         -> 
           Location.raise_errorf
             ~loc:pval_loc
             "%%identity expect its type to be of form 'a -> 'b (arity 1)"
       | _ ->
         Bs_ast_iterator.default_iterator.value_description self v 
         
      )
  }
