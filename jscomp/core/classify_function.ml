(* Copyright (C) 2020- Authors of BuckleScript
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

let classify (prog : string) : Js_raw_info.exp = 
  match Parser_flow.parse_expression 
    (Parser_env.init_env None prog) false with 
  | (_, Function {
    id = _;
    params = (_, {params});
    async = false;
    generator = false;
    predicate = None
  }) , [] -> 
    Js_function {arity = List.length params; arrow = false}
  | (_, ArrowFunction {
    id = None;
    params = (_, {params});
    async = false;
    generator = false;
    predicate = None
  }) , [] -> 
    Js_function
      {arity = List.length params; arrow = true} 
 |(_, Literal {comments}), [] -> 
  let comment = 
    match comments with 
    | None -> None 
    | Some {leading = [_, Block comment]} -> Some ("/*" ^ comment ^ "*/")
    | Some {leading = [_, Line comment]} -> Some ("//" ^ comment)
    | Some _ -> None
  in   
  Js_literal {comment}   
 | _ -> 
  Js_exp_unknown
 | exception _ -> 
  Js_exp_unknown

let classify_stmt (prog : string) : Js_raw_info.stmt = 
  let result =  Parser_flow.parse_program false None prog in 
  match fst result with 
  | (_loc, [], _) -> 
    Js_stmt_comment 
  | _ -> Js_stmt_unknown
(* we can also analayze throw
  x.x pure access
 *)