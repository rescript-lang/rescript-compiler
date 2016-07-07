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

type attr =  Parsetree.attribute
type t =  attr list 

type ('a,'b) st = 
  { get : 'a option ; 
    set : 'b option }


let process_method_attributes_rev (attrs : t) = 
  List.fold_left (fun (st,acc) (({txt ; loc}, payload) as attr : attr) -> 

      match txt  with 
      | "bs.get" (* [@@bs.get{null; undefined}]*)
        -> 
        let result = 
          List.fold_left 
          (fun 
            (null, undefined)
            (({txt ; loc}, opt_expr) : Ast_payload.action) -> 
            if txt = Lident "null" then 
              (match opt_expr with 
              | None -> true
              | Some e -> 
                Ast_payload.assert_bool_lit e), undefined

            else if txt = Lident "undefined" then 
              null, 
              (match opt_expr with
               | None ->  true
               | Some e -> 
                 Ast_payload.assert_bool_lit e)

            else Location.raise_errorf ~loc "unsupported predicates"
          ) (false, false) (Ast_payload.as_record_and_process loc payload)  in 

        ({st with get = Some result}, acc  )

      | "bs.set"
        -> 
        let result = 
          List.fold_left 
          (fun st (({txt ; loc}, opt_expr) : Ast_payload.action) -> 
            if txt = Lident "no_get" then 
              match opt_expr with 
              | None -> `No_get 
              | Some e -> 
                if Ast_payload.assert_bool_lit e then 
                  `No_get
                else `Get
            else Location.raise_errorf ~loc "unsupported predicates"
          ) `Get (Ast_payload.as_record_and_process loc payload)  in 
        (* properties -- void 
              [@@bs.set{only}]
           *)
        {st with set = Some result }, acc
      | _ -> 
        (st, attr::acc  )
    ) ( {get = None ; set = None}, []) attrs


let process_attributes_rev (attrs : t) = 
  List.fold_left (fun (st, acc) (({txt; loc}, _) as attr : attr) -> 
      match txt, st  with 
      | "bs", (`Nothing | `Uncurry) 
        -> 
        `Uncurry, acc
      | "bs.this", (`Nothing | `Meth)
        ->  `Meth, acc
      | "bs", `Meth 
      | "bs.this", `Uncurry
        -> Location.raise_errorf 
             ~loc
             "[@bs.this] and [@bs] can not be applied at the same time"
      | _ , _ -> 
        st, attr::acc 
    ) ( `Nothing, []) attrs


let bs_obj  : attr 
  = {txt = "bs.obj" ; loc = Location.none}, Ast_payload.empty

let bs : attr
  =  {txt = "bs" ; loc = Location.none}, Ast_payload.empty
let bs_this : attr
  =  {txt = "bs.this" ; loc = Location.none}, Ast_payload.empty
