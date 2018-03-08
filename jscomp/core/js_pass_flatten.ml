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

let flatten_map = 
  object(self)
    inherit Js_map.map as super
    method! statement x = 
      match x.statement_desc with 
      |  Exp ({expression_desc = Seq _; _} as v) ->
          (S.block ( List.rev_map (self#statement) (Js_analyzer.rev_flatten_seq v )))
      |  Exp ({expression_desc = Cond(a,b,c); comment} ) -> 
          (* Note that we need apply [self#statement] recursively *)
          { statement_desc = If (a, [ self#statement (S.exp b)],  
                                 Some [ self#statement (S.exp c)]); comment}
          (* CHECK? Trick semantics difference *)
          (* super#statement (S.if_ a ([ (\* self#statement *\) (S.exp b) ]) *)
          (*     ~else_:([self#statement (S.exp c)]) *)
          (*                 ) *)

      |  Exp ({expression_desc = Bin(Eq, a, ({expression_desc = Seq _; _ } as v)); _} )
        ->
          let block = Js_analyzer.rev_flatten_seq v in
          begin match block with
          | {statement_desc = Exp last_one ; _} :: rest_rev
            ->  
              S.block (Ext_list.rev_map_append (self#statement) rest_rev 
                [self#statement @@ S.exp (E.assign a  last_one)])
                (* TODO: here we introduce a block, should avoid it *)
              (* super#statement *)
              (*   (S.block (List.rev_append rest_rev [S.exp (E.assign a  last_one)])) *)
          | _ ->
              assert false
          end
      | Return ( {return_value = {expression_desc = Cond (a,b,c);  comment}}) 
        -> 
          { statement_desc = If (a, [self#statement (S.return_stmt b)],  
                                 Some [ self#statement (S.return_stmt c)]); comment}

      | Return ({return_value = {expression_desc = Seq _; _} as v}) ->
          let block = Js_analyzer.rev_flatten_seq v  in
          begin match block with
          | {statement_desc = Exp last_one ; _} :: rest_rev
            ->  
              super#statement 
                (S.block (Ext_list.rev_map_append (self#statement) rest_rev [S.return_stmt last_one]))
          | _ -> assert false
          end
      | Block [x]
          -> 
            self#statement x 
      | _ -> super#statement x 

    method! block b =
      match b with
      | {statement_desc = Block bs } :: rest ->
          self#block ( bs @  rest)
      | x::rest  
        -> 
          self#statement x :: self#block rest
      | [] -> []
  end

let program ( x : J.program) = flatten_map # program x 
