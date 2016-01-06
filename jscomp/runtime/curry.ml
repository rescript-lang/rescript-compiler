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






external function_length : 'a -> int = "js_function_length" 

external apply1 : 
    ('a -> 'b) -> 'a -> 'b
    = "js_apply1"
external apply2 :
        ('a -> 'b -> 'c) -> 'a -> 'b -> 'c  
    = "js_apply2"

external apply3 : 
            ('a -> 'b -> 'c -> 'd)
          -> 'a -> 'b -> 'c -> 'd  
    = "js_apply3"

external apply4 : 
              ('a -> 'b -> 'c -> 'd -> 'e)
            -> 'a -> 'b -> 'c -> 'd -> 'e
    = "js_apply4"

external apply5 : 
                ('a -> 'b -> 'c -> 'd -> 'e -> 'f )
              -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f = 
  "js_apply5"

external apply6 : 
                  ('a -> 'b -> 'c -> 'd -> 'e ->  'f -> 'g ) 
                -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g = 
                  "js_apply6"


external apply7 : 
                    ('a -> 'b -> 'c -> 'd -> 'e ->  'f -> 'g -> 'h ) 
                  -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h = 
      "js_apply7"


external apply8 : 
                      ('a -> 'b -> 'c -> 'd -> 'e ->  'f -> 'g -> 'h -> 'i) 
                    -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i = 
                      "js_apply8"

external apply_args : 
                        ('a -> 'b) -> _ array -> 'b = "js_apply"

external append : 'a array -> 'a array -> 'a array  = "caml_array_append"

external sub : 'a array -> int -> int -> 'a array = "caml_array_sub"

let rec curry f args = 
  let arity = function_length f in
  let len = Array.length args in
  let d = arity - len in 
  if d = 0 then 
    apply_args f  args (**f.apply (null,args) *)
  else if d < 0 then 
    (** TODO: could avoid copy by tracking the index *)
    curry (Obj.magic (apply_args f (sub args 0 arity)))
      (sub args arity (-d))
  else 
    Obj.magic (fun x -> curry f (append args [|x|] ))

(* Generated code 
   [if/else]
   Here it would be nice to just generate 
   [switch .. default]
 *)
let curry1 o x arity = 
  (match arity with 
  | 0 -> apply1 (Obj.magic o) x  
  | 1 -> apply1 (Obj.magic o) x 
  | 2 ->  apply2 (Obj.magic o) x 
  | 3 -> apply3 (Obj.magic o) x
  | 4 ->  apply4 (Obj.magic o) x 
  | 5 -> apply5 (Obj.magic o) x
  | 6 -> apply6 (Obj.magic o) x 
  | 7 -> apply7 (Obj.magic o) x 
  | _ -> (fun a -> curry o [|x; a |]))

let app1 o x = 
  let len = function_length o in
  if len = 1 || len = 0 then apply1 o x 
  else Obj.magic (curry1 o x len )

let app2 o x y = 
  let len = function_length o in 
  if len = 2 then apply2 o  x y
  else Obj.magic (curry o [|x; y|])

let app3 o a0 a1 a2 =  
  let len = function_length o in 
  if len = 3 then apply3 o a0 a1 a2 
  else 
    Obj.magic (curry o [|a0;a1;a2|])

let app4 o a0 a1 a2 a3 =  
  let len = function_length o in 
  if len = 4 then apply3 o a0 a1 a2 a3
  else 
    Obj.magic (curry o [|a0;a1;a2; a3 |])

let app5 o a0 a1 a2 a3 a4 =  
  let len = function_length o in 
  if len = 4 then apply3 o a0 a1 a2 a3 a4 
  else 
    Obj.magic (curry o [|a0;a1;a2; a3; a4 |])
