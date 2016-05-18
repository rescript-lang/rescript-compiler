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






(* When we design a ppx, we should keep it simple, and also think about 
   how it would work with other tools like merlin and ocamldep  *)

(**
1. extension point 
   {[ 
     [%unsafe{| blabla |}]
   ]}
   will be desugared into 
   {[ 
     let module Js = 
     struct unsafe_js : string -> 'a end 
     in Js.unsafe_js {| blabla |}
   ]}
   The major benefit is to better error reporting (with locations).
   Otherwise

   {[

     let f u = Js.unsafe_js u 
     let _ = f (1 + 2)
   ]}
   And if it is inlined some where   
*)


let tmp_module_name = "J"
let tmp_fn = "unsafe_expr"
let predef_string_type = 
  Ast_helper.Typ.var "string" 
let predef_any_type = 
  Ast_helper.Typ.any ()
let predef_unit_type = 
  Ast_helper.Typ.var "unit"
let predef_val_unit  = 
  Ast_helper.Exp.construct {txt = Lident "()"; loc = Location.none }  None
let prim = "js_pure_expr"
let prim_stmt = "js_pure_stmt"
let prim_debugger = "js_debugger"

(* note we first declare its type is [unit], 
   then [ignore] it, [ignore] is necessary since 
   the js value  maybe not be of type [unit] and 
   we can use [unit] value (though very little chance) 
   sometimes
*)
let discard_js_value loc e  : Parsetree.expression = 
  {pexp_desc = 
     Pexp_apply
       ({pexp_desc = 
           Pexp_ident {txt = Ldot (Lident "Pervasives", "ignore") ; loc};
         pexp_attributes = [];
         pexp_loc = loc},
        [("",
          {pexp_desc =
             Pexp_constraint (e,
                              {ptyp_desc = Ptyp_constr ({txt = Lident "unit"; loc}, []);
                               ptyp_loc = loc;
                               ptyp_attributes = []});
           pexp_loc = loc;
           pexp_attributes = []
          })]
       );
   pexp_loc = loc;
   pexp_attributes = [] 
  }


let handle_raw ?ty loc e attrs  = 
  let attrs = 
    match ty with 
    | Some ty -> 
      Parsetree_util.attr_attribute_from_type ty :: attrs  
    | None -> attrs in 
  Ast_helper.Exp.letmodule 
    {txt = tmp_module_name; loc }
    (Ast_helper.Mod.structure [ 
        Ast_helper.Str.primitive 
          (Ast_helper.Val.mk ~attrs {loc ; txt = tmp_fn} 
             ~prim:[prim]
             (Ast_helper.Typ.arrow "" predef_string_type predef_any_type))]
    )    
  (Ast_helper.Exp.constraint_ ~loc  
    (Ast_helper.Exp.apply 
       (Ast_helper.Exp.ident {txt= Ldot(Lident tmp_module_name, tmp_fn) ; loc})
       [("",e)])
    (match ty with 
    | Some ty -> ty
    | None -> predef_any_type))
    

let find_uncurry_attrs_and_remove (attrs : Parsetree.attributes ) = 
  let rec aux (attrs : Parsetree.attributes) acc = 
    match attrs with 
    | [({txt = "uncurry"}, _) as v ]  -> Some (List.rev acc, v)
    | ({txt = "uncurry"}, _) as v :: rest -> 
      Some ((List.rev acc @ rest)  , v)
    | non_uncurry :: rest -> aux rest  (non_uncurry :: acc) 
    | [] -> None 
  in 
  aux attrs []

let uncurry_attr loc  : Parsetree.attribute = 
  {txt = "uncurry"; loc}, PStr []

let handle_typ (super : Ast_mapper.mapper) 
    (self : Ast_mapper.mapper)
    (ty : Parsetree.core_type) = 
  match ty with
  | {ptyp_attributes ;
     ptyp_desc = Ptyp_arrow ("", args, body);
     ptyp_loc = loc
    } ->
    begin match  find_uncurry_attrs_and_remove ptyp_attributes with 
      | Some (ptyp_attributes, _) ->
        let args = self.typ self args in
        let body = self.typ self body in
        let fn_type : Parsetree.core_type =
          match args with
          | {ptyp_desc = Ptyp_tuple [arg ; {ptyp_desc = Ptyp_constr ({txt = Lident "__"}, [])} ]; _} 
            ->
            { ptyp_loc = loc; 
              ptyp_desc = Ptyp_tuple [ arg ; body];
              ptyp_attributes}
          | {ptyp_desc = Ptyp_tuple args; _} ->
            {ptyp_desc = Ptyp_tuple (List.rev (body :: List.rev args));
             ptyp_loc = loc;
             ptyp_attributes 
            }
          | {ptyp_desc = Ptyp_constr ({txt = Lident "unit"}, []); _} -> body
          | v -> {ptyp_desc = Ptyp_tuple [v ; body];
                  ptyp_loc = loc ; 
                  ptyp_attributes }
        in
        { ty with ptyp_desc =
                    Ptyp_constr ({txt = Ldot (Lident "Fn", "t") ; loc},
                                 [ fn_type]);
                  ptyp_attributes = []
        }

      | None -> super.typ self ty
    end
  | {ptyp_desc =  Ptyp_object ( methods, closed_flag) } -> 
    let methods = List.map (fun (label, ptyp_attrs, core_type ) -> 
        match find_uncurry_attrs_and_remove ptyp_attrs with 
        | None -> label, ptyp_attrs , self.typ self core_type
        | Some (ptyp_attrs, v) -> 
          label , ptyp_attrs, self.typ self 
            { core_type with ptyp_attributes = v :: core_type.ptyp_attributes}
      ) methods in           
    {ty with ptyp_desc = Ptyp_object (methods, closed_flag)}
      
  | _ -> super.typ self ty

let rec unsafe_mapper : Ast_mapper.mapper =   
  { Ast_mapper.default_mapper with 
    expr = (fun mapper e -> 
        match e.pexp_desc with 
        | Pexp_extension (
            {txt = "bs.raw"; loc} ,
            PStr 
              ( [{ pstr_desc = Pstr_eval ({ 
                   pexp_desc = Pexp_constant (Const_string (_, _)) ;
                   pexp_attributes = attrs } as e ,
                                                _); pstr_loc = _ }]))
          -> 

              handle_raw loc e attrs
        | Pexp_extension( {txt = "bs.raw"; loc}, PStr 
                ( [{ pstr_desc = Parsetree.Pstr_eval ({ 
                      pexp_desc = 
                        Pexp_constraint (
                          {pexp_desc = Pexp_constant (Const_string (_, _)) ; _}
                          as e,
                             ty)
                      ; pexp_attributes = attrs} , _);  }]))
        | Pexp_constraint({pexp_desc = Pexp_extension( {txt = "bs.raw"; loc}, PStr 
                ( [{ pstr_desc = Pstr_eval ({ 
                      pexp_desc = 
                        Pexp_constant (Const_string (_, _)) 
                      ; pexp_attributes = attrs} as e , _);  }]))}, ty)            
              -> handle_raw ~ty loc e attrs
        | Pexp_extension({txt = "bs.raw"; loc}, (PTyp _ | PPat _ | PStr _))
              -> 
              Location.raise_errorf ~loc "bs.raw can only be applied to a string"

        | Pexp_extension ({txt = "bs.debugger"; loc} , payload)
          ->
          begin
            match payload with
            | Parsetree.PStr ( [])
              ->
              Ast_helper.Exp.letmodule
                {txt = tmp_module_name; loc }
                (Ast_helper.Mod.structure [
                    Ast_helper.Str.primitive
                      (Ast_helper.Val.mk {loc ; txt = tmp_fn}
                         ~prim:[prim_debugger]
                         (Ast_helper.Typ.arrow "" predef_unit_type predef_unit_type)
                      )])
                (Ast_helper.Exp.apply
                   (Ast_helper.Exp.ident 
                      {txt= Ldot(Lident tmp_module_name, tmp_fn) ; loc})
                   [("",  predef_val_unit)])
            | Parsetree.PTyp _
            | Parsetree.PPat (_,_)
            | Parsetree.PStr _
              ->
              Location.raise_errorf ~loc "bs.raw can only be applied to a string"
          end
        |   (** Future 
                                 {| fun%bs this (a,b,c) -> |}
                                 function can only take one argument
                             *)
               Pexp_extension
                 ({txt = "uncurry";loc},
                  PStr
                    [{pstr_desc =
                        Pstr_eval
                          ({pexp_desc =
                              Pexp_fun ("", None, pat ,
                                        body)},
                           _)}])
          -> 
          let args = 
            match pat with 
            | {ppat_desc = Ppat_tuple [arg ; {ppat_desc = Ppat_var{txt = "__"}} ]; _} -> 
              [arg]
            | {ppat_desc = Ppat_tuple args; _} -> args
            | {ppat_desc = Ppat_construct ({txt = Lident "()"}, None); _} -> []
            | v -> [v]
          in
          let len = List.length args in 
          let mk = "mk" ^ string_of_int len in 
          let body = mapper.expr mapper body in 
          begin match args with 
            | [] -> 
              {e with pexp_desc =
                 Pexp_apply (
                   {pexp_desc = Pexp_ident {txt = Ldot (Lident "Fn", mk); loc};
                    pexp_loc = loc; 
                    pexp_attributes = []
                   },
                   [("",
                     {pexp_desc =
                        Pexp_fun ("", None,
                                  {ppat_desc = 
                                     Ppat_construct ({txt = Lident "()"; loc}, None);
                                   ppat_loc = loc ; 
                                   ppat_attributes = []},
                                  body);
                      pexp_loc = loc ;
                      pexp_attributes = []})])}
            | _ -> 
              let fun_ = 
                List.fold_right (fun arg body -> 
                    let arg = mapper.pat mapper arg in 
                    {Parsetree.
                      pexp_loc = loc ; 
                      pexp_desc = Pexp_fun ("", None, arg, body);
                      pexp_attributes = []}) args body in
              { e  with 
                pexp_desc = 
                  Pexp_apply ({pexp_desc = Pexp_ident {txt = Ldot (Lident "Fn", mk); loc};
                               pexp_loc = loc ; 
                               pexp_attributes = []},
                              [("",
                                fun_)])
              }
          end
        | Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "#@"; loc}},
                      [("", fn);
                       ("", pat)])
          -> 
          let args = 
            match pat with 
            | {pexp_desc = Pexp_tuple [arg ; {pexp_desc = Pexp_ident{txt = Lident "__"; _}} ]; _} -> 
              [arg]
            | {pexp_desc = Pexp_tuple args; _} -> args
            | {pexp_desc = Pexp_construct ({txt = Lident "()"}, None); _} -> []
            | v -> [v]
          in

          let fn = mapper.expr mapper fn in 
          let args = List.map (mapper.expr mapper) args in 
          let len = List.length args in 
          let run = "run" ^ string_of_int len in 
          { e with
            pexp_desc =
              Pexp_apply (
                {pexp_desc = 
                   Pexp_ident {txt = Ldot (Lident "Fn", run) ;
                               loc ; };
                 pexp_loc = loc ;
                 pexp_attributes = []
                },
                (("", fn) :: List.map (fun x -> "", x) args))
          }

        (** object 
            for setter : we can push more into [Lsend] and enclose it with a unit type

            for getter :

            (* Invariant: we expect the typechecker & lambda emitter  
               will not do agressive inlining
               Worst things could happen
               {[
                 let x = y## case 3  in 
                 x 2
               ]}
               in normal case, it should be compiled into Lambda
               {[
                 let x = Lsend(y,case, [3]) in 
                 Lapp(x,2)
               ]}

               worst:
               {[ Lsend(y, case, [3,2])
               ]}               
               for setter(include case setter), this could 
               be prevented by type system, for getter.

               solution: we can prevent this by rewrite into 
               {[
                 Fn.run1  (!x# case) v 
               ]}
            *)

        *)
        | Pexp_apply ({pexp_desc = 
                         Pexp_ident  {txt = Lident "#." ; loc} ; _},
                      [("", obj) ;
                       ("", 
                        ({pexp_desc = Pexp_ident {txt = Lident name;_ } ; _}
                        |{pexp_desc = Pexp_construct ({txt = Lident name;_ }, None) ; _}
                        ) ) (* TODO: design: shall we allow 
                               {[ x #.Capital ]}
                            *)
                      ])
          ->
          (* ./dumpast -e ' (Js.Unsafe.(!) obj) # property ' *)
          let obj = mapper.expr mapper obj in 
          {pexp_desc =
             Pexp_send
               ({pexp_desc =
                   Pexp_apply
                     ({pexp_desc =
                         Pexp_ident {txt = Ldot (Ldot (Lident "Js", "Unsafe"), "!");
                                     loc};
                       pexp_loc = loc;
                       pexp_attributes = []},
                      [("", obj)]);
                 pexp_loc = loc;
                 pexp_attributes = []},
                name);
           pexp_loc = loc ; 
           pexp_attributes = []}
        (** TODO: 
            More syntax sanity check for [case__set] 
            case__set: arity 2
            __set : arity 1            
            case:
        *)

        | Pexp_apply
            ({pexp_desc = 
               Pexp_apply ({pexp_desc = 
                         Pexp_ident  {txt = Lident "##" ; loc} ; _},
                      [("", obj) ;
                       ("", {pexp_desc = Pexp_ident {txt = Lident name;_ } ; _} )
                      ]);
             _
            }, [ "", value]  )
          -> (** f ## xx a b -->  (f ## x a ) b -- we just pick the first one *)

          let is_setter = 
            name <> Literals.case_set && 
            Ext_string.ends_with name Literals.setter_suffix in
          let args = 
            if  is_setter then 
              [value]
            else 
              match value with 
              | {pexp_desc = 
                   Pexp_tuple 
                     [arg ; {pexp_desc = Pexp_ident{txt = Lident "__"; _}} ];
                 _} -> 
                [arg]
              | {pexp_desc = Pexp_tuple args; _} -> args
              | {pexp_desc = 
                   Pexp_construct ({txt = Lident "()"}, None);
                 _} -> []
              | v -> [v]
          in
          let len = List.length args in 
          let obj = mapper.expr mapper obj in 
          let args = List.map (mapper.expr mapper ) args in 
          (* TODO: in the future, dynamically create the c externs, 
             so it can handle arbitrary large number
          *)
          let run = "run" ^ string_of_int len in 
          { e with
            pexp_desc =
              Pexp_apply (
                {pexp_desc = 
                   Pexp_ident {txt = Ldot (Lident "Fn", run) ;
                               loc ; };
                 pexp_loc = loc ;
                 pexp_attributes = []
                },
                (("",
                  {pexp_desc =
                     Pexp_send
                       ({pexp_desc =
                           Pexp_apply
                             ({pexp_desc =
                                 Pexp_ident {
                                   txt = Ldot (Ldot (Lident "Js", "Unsafe"), "!");
                                   loc };
                               pexp_loc = loc ; 
                               pexp_attributes = []},
                              [("", obj)]);
                         pexp_loc = loc ;
                         pexp_attributes = []},
                        name);
                   pexp_loc = loc ; 
                   pexp_attributes = [] }) :: 
                 List.map (fun x -> "", x) args
                ))
          }

        | _ ->  Ast_mapper.default_mapper.expr  mapper e
      );
    typ = (fun self typ -> handle_typ Ast_mapper.default_mapper self typ);
    structure_item = (fun mapper (str : Parsetree.structure_item) -> 
        begin match str.pstr_desc with 
        | Pstr_extension ( ({txt = "bs.raw"; loc}, payload), _attrs) 
          -> 
            begin match payload with 
              | Parsetree.PStr 
                  ( [{ pstr_desc = Parsetree.Pstr_eval ({ 
                        pexp_desc = Pexp_constant (Const_string (cont, opt_label)) ;
                        pexp_loc; pexp_attributes } as e ,_); pstr_loc }])
                -> 
                Ast_helper.Str.eval @@ 
                Ast_helper.Exp.letmodule 
                  {txt = tmp_module_name; loc }
                  (Ast_helper.Mod.structure [ 
                      Ast_helper.Str.primitive 
                        (Ast_helper.Val.mk {loc ; txt = tmp_fn} 
                           ~prim:[prim_stmt]
                           (Ast_helper.Typ.arrow ""
                              predef_string_type predef_any_type))])    
                  (Ast_helper.Exp.apply 
                     (Ast_helper.Exp.ident 
                        {txt= Ldot(Lident tmp_module_name, tmp_fn) ; loc})
                     [("",e)])
              | Parsetree.PTyp _ 
              | Parsetree.PPat (_,_) 
              | Parsetree.PStr _ 
                -> 
                Location.raise_errorf ~loc "bs.raw can only be applied to a string"
            end
        | _ -> Ast_mapper.default_mapper.structure_item mapper str 
        end
      )
  }
let rewrite_signature : (Parsetree.signature -> Parsetree.signature) ref = 
  ref (fun  x -> 
      unsafe_mapper.signature  unsafe_mapper x
       )

let rewrite_implementation : (Parsetree.structure -> Parsetree.structure) ref = 
  ref (fun x -> unsafe_mapper.structure  unsafe_mapper x )

