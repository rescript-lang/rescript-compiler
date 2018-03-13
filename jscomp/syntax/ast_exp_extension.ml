(* Copyright (C) 2018 Authors of BuckleScript
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
open Ast_helper

let handle_extension record_as_js_object e (self : Bs_ast_mapper.mapper)
    (({txt ; loc} as lid , payload) : Parsetree.extension) = 
  begin match txt with
    | "bs.raw" | "raw" -> 
      Ast_util.handle_raw loc payload
    | "bs.re" | "re" ->
      Exp.constraint_ ~loc
        (Ast_util.handle_raw ~check_js_regex:true loc payload)
        (Ast_comb.to_js_re_type loc)
    | "bs.external" | "external" ->
      begin match Ast_payload.as_ident payload with 
        | Some {txt = Lident x}
          -> Ast_util.handle_external loc x
        (* do we need support [%external gg.xx ] 

           {[ Js.Undefined.to_opt (if Js.typeof x == "undefined" then x else Js.Undefined.empty ) ]}
        *)

        | None | Some _ -> 
          Location.raise_errorf ~loc 
            "external expects a single identifier"
      end
    | "bs.time"| "time" ->
      (
        match payload with 
        | PStr [{pstr_desc = Pstr_eval (e,_)}] -> 
          let locString = 
            if loc.loc_ghost then 
              "GHOST LOC"
            else 
              let loc_start = loc.loc_start in 
              let (file, lnum, __) = Location.get_pos_info loc_start in                  
              Printf.sprintf "%s %d"
                file lnum in   
          let e = self.expr self e in 
          Exp.sequence ~loc
            (Exp.apply ~loc     
               (Exp.ident ~loc {loc; 
                                txt = 
                                  Ldot (Ldot (Lident "Js", "Console"), "timeStart")   
                               })
               ["", Exp.constant ~loc (Const_string (locString,None))]
            )     
            ( Exp.let_ ~loc Nonrecursive
                [Vb.mk ~loc (Pat.var ~loc {loc; txt = "timed"}) e ;
                ]
                (Exp.sequence ~loc
                   (Exp.apply ~loc     
                      (Exp.ident ~loc {loc; 
                                       txt = 
                                         Ldot (Ldot (Lident "Js", "Console"), "timeEnd")   
                                      })
                      ["", Exp.constant ~loc (Const_string (locString,None))]
                   )    
                   (Exp.ident ~loc {loc; txt = Lident "timed"})
                )
            )
        | _ -> 
          Location.raise_errorf 
            ~loc "expect a boolean expression in the payload"
      )
    | "bs.assert" | "assert" ->
      (
        match payload with 
        | PStr [ {pstr_desc = Pstr_eval( e,_)}] -> 

          let locString = 
            if loc.loc_ghost then 
              "ASSERT FAILURE"
            else 
              let loc_start = loc.loc_start in 
              let (file, lnum, cnum) = Location.get_pos_info loc_start in
              let enum = 
                loc.Location.loc_end.Lexing.pos_cnum -
                loc_start.Lexing.pos_cnum + cnum in
              Printf.sprintf "File %S, line %d, characters %d-%d"
                file lnum cnum enum in   
          let raiseWithString  locString =      
            (Exp.apply ~loc 
               (Exp.ident ~loc {loc; txt = 
                                       Ldot(Ldot (Lident "Js","Exn"),"raiseError")})
               ["",

                Exp.constant (Const_string (locString,None))    
               ])
          in 
          (match e.pexp_desc with
           | Pexp_construct({txt = Lident "false"},None) -> 
             (* The backend will convert [assert false] into a nop later *)
             if !Clflags.no_assert_false  then 
               Exp.assert_ ~loc 
                 (Exp.construct ~loc {txt = Lident "false";loc} None)
             else 
               (raiseWithString locString)
           | Pexp_constant (Const_string (r, _)) -> 
             if !Clflags.noassert then 
               Exp.assert_ ~loc (Exp.construct ~loc {txt = Lident "true"; loc} None)
               (* Need special handling to make it type check*)
             else   
               raiseWithString r
           | _ ->    
             let e = self.expr self  e in 
             if !Clflags.noassert then 
               (* pass down so that it still type check, but the backend will
                  make it a nop
               *)
               Exp.assert_ ~loc e
             else 
               Exp.ifthenelse ~loc
                 (Exp.apply ~loc
                    (Exp.ident {loc ; txt = Ldot(Lident "Pervasives","not")})
                    ["", e]
                 )
                 (raiseWithString locString)
                 None
          )
        | _ -> 
          Location.raise_errorf 
            ~loc "expect a boolean expression in the payload"
      )
    | "bs.node" | "node" ->
      let strip s =
        match s with 
        | "_module" -> "module" 
        | x -> x  in 
      begin match Ast_payload.as_ident payload with
        | Some {txt = Lident
                    ( "__filename"
                    | "__dirname"
                    | "_module"
                    | "require" as name); loc}
          ->
          let exp =
            Ast_util.handle_external loc (strip name)  in
          let typ =
            Ast_core_type.lift_option_type  
            @@                 
            if name = "_module" then
              Typ.constr ~loc
                { txt = Ldot (Lident "Node", "node_module") ;
                  loc} []   
            else if name = "require" then
              (Typ.constr ~loc
                 { txt = Ldot (Lident "Node", "node_require") ;
                   loc} [] )  
            else
              Ast_literal.type_string ~loc () in                  
          Exp.constraint_ ~loc exp typ                
        | Some _ | None ->
          begin match payload with 
            | PTyp _ -> 
              Location.raise_errorf 
                ~loc "Illegal payload, expect an expression payload instead of type payload"              
            | PPat _ ->
              Location.raise_errorf 
                ~loc "Illegal payload, expect an expression payload instead of pattern  payload"        
            | _ -> 
              Location.raise_errorf 
                ~loc "Illegal payload"
          end

      end             
    | "bs.debugger"|"debugger" ->
      {e with pexp_desc = Ast_util.handle_debugger loc payload}
    | "bs.obj" | "obj" ->
      begin match payload with 
        | PStr [{pstr_desc = Pstr_eval (e,_)}]
          -> 
          Ext_ref.non_exn_protect record_as_js_object true
            (fun () -> self.expr self e ) 
        | _ -> Location.raise_errorf ~loc "Expect an expression here"
      end
    | _ ->
      match payload with
      | PTyp typ when Ext_string.starts_with txt Literals.bs_deriving_dot ->
        self.expr self (Ast_derive.gen_expression lid typ)
      | _ ->  
        e (* For an unknown extension, we don't really need to process further*)
        (* Exp.extension ~loc ~attrs:e.pexp_attributes (
            self.extension self extension) *)
        (* Bs_ast_mapper.default_mapper.expr self e   *)
  end 
