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

let rec app f args = 
  let arity = function_length f in
  let len = Array.length args in
  let d = arity - len in 
  if d = 0 then 
    apply_args f  args (**f.apply (null,args) *)
  else if d < 0 then 
    (** TODO: could avoid copy by tracking the index *)
    app (Obj.magic (apply_args f (sub args 0 arity)))
      (sub args arity (-d))
  else 
    Obj.magic (fun x -> app f (append args [|x|] ))

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
  | _ -> (fun a -> app o [|x; a |]))

let app1 o x = 
  let len = function_length o in
  if len = 1 || len = 0 then apply1 o x 
  else Obj.magic (curry1 o x len )

  
let app2 o x y = 
  let len = function_length o in 
  if len = 2 then apply2 o  x y
  else Obj.magic (app o [|x; y|])



let app3 o a0 a1 a2 =  
  let len = function_length o in 
  if len = 3 then apply3 o a0 a1 a2 
  else 
    Obj.magic (app o [|a0;a1;a2|])


let app4 o a0 a1 a2 a3 =  
  let len = function_length o in 
  if len = 4 then apply4 o a0 a1 a2 a3
  else 
    Obj.magic (app o [|a0;a1;a2; a3 |])

let app5 o a0 a1 a2 a3 a4 =  
  let len = function_length o in 
  if len = 5 then apply5 o a0 a1 a2 a3 a4
  else 
    Obj.magic (app o [|a0;a1;a2; a3; a4 |])


let app6 o a0 a1 a2 a3 a4 a5  =  
  let len = function_length o in 
  if len = 6 then apply6 o a0 a1 a2 a3 a4 a5
  else 
    Obj.magic (app o [|a0;a1;a2; a3; a4; a5 |])

let app7 o a0 a1 a2 a3 a4 a5 a6 =  
  let len = function_length o in 
  if len = 7 then apply7 o a0 a1 a2 a3 a4 a5 a6 
  else 
    Obj.magic (app o [|a0;a1;a2; a3; a4; a5; a6 |])

let app8 o a0 a1 a2 a3 a4 a5 a6 a7  =  
  let len = function_length o in 
  if len = 8 then apply8 o a0 a1 a2 a3 a4 a5 a6 a7 
  else 
    Obj.magic (app o [|a0;a1;a2; a3; a4; a5; a6; a7|])

(** For efficiency, [args.(0)] would contain obj as well  *)
let js label cacheid obj args = 
  let meth = 
    (Obj.magic Caml_oo.caml_get_public_method obj label cacheid) in
  app meth args


(* example like [x#hi] *)
let js1   label cacheid obj = 
  let meth = 
    (Obj.magic Caml_oo.caml_get_public_method obj label cacheid) in
  app1 meth obj 

let js2   label cacheid obj a1 = 
  let meth = 
    (Obj.magic Caml_oo.caml_get_public_method obj label cacheid) in
  app2 meth obj a1 

let js3  label cacheid obj  a1 a2 = 
  let meth = 
    (Obj.magic Caml_oo.caml_get_public_method obj label cacheid) in
  app3 meth obj a1 a2

let js4  label cacheid obj a1 a2 a3 = 
  let meth = 
    (Obj.magic Caml_oo.caml_get_public_method obj label cacheid) in
  app4 meth obj a1 a2 a3 


let js5  label cacheid obj a1 a2 a3 a4 = 
  let meth = 
    (Obj.magic Caml_oo.caml_get_public_method obj label cacheid) in
  app5 meth obj a1 a2 a3 a4

let js6  label cacheid obj a1 a2 a3 a4 a5 = 
  let meth = 
    (Obj.magic Caml_oo.caml_get_public_method obj label cacheid) in
  app6 meth obj a1 a2 a3 a4 a5

let js7  label cacheid obj a1 a2 a3 a4 a5 a6= 
  let meth = 
    (Obj.magic Caml_oo.caml_get_public_method obj label cacheid) in
  app7 meth obj a1 a2 a3 a4 a5 a6

let js8  label cacheid obj a1 a2 a3 a4 a5 a6 a7 = 
  let meth = 
    (Obj.magic Caml_oo.caml_get_public_method obj label cacheid) in
  app8 meth obj a1 a2 a3 a4 a5 a6 a7 






