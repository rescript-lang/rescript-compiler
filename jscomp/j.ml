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



(** Javascript IR
  
    It's a subset of Javascript AST specialized for OCaml lambda backend

    Note it's not exactly the same as Javascript, the AST itself follows lexical
    convention and [Block] is just a sequence of statements, which means it does 
    not introduce new scope
*)

type label = string

and binop = Js_op.binop

and int_op = Js_op.int_op
 
and kind = Js_op.kind

and property = Js_op.property

and number = Js_op.number 

and mutable_flag = Js_op.mutable_flag 

and ident_info = Js_op.ident_info

and exports = Js_op.exports

and required_modules = Js_op.required_modules

(** object literal, if key is ident, in this case, it might be renamed by 
    Google Closure  optimizer,
    currently we always use quote
 *)
and property_name =  string

and ident = Ident.t 

and vident = 
  | Id of ident
  | Qualified of ident * kind * string option
    (* Since camldot is only available for toplevel module accessors,
       we don't need print  `A.length$2`
       just print `A.length` - it's guarateed to be unique
       
       when the third one is None, it means the whole module 
     *)

and exception_ident = ident 

and for_ident = ident 

and for_direction = Asttypes.direction_flag

and property_map = 
    (property_name * expression) list

and expression_desc =
  | Math of string * expression list
  | Array_length of expression
  | String_length of expression
  | Bytes_length of expression
  | Function_length of expression 

  | Char_of_int of expression
  | Char_to_int of expression 
  | Array_of_size of expression 
    (* used in [js_create_array] primitive, note having
       uninitilized array is not as bad as in ocaml, 
       since GC does not rely on it
     *)
  | Array_append of expression * expression list (* For [caml_array_append]*)
  | Tag_ml_obj of expression
  | String_append of expression * expression 
  | Int_of_boolean of expression 
  (* https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence 
     [typeof] is an operator     
  *)
  | Typeof of expression
  | Not of expression (* !v *)
  | String_of_small_int_array of expression 
    (* String.fromCharCode.apply(null, args) *)
    (* Convert JS boolean into OCaml boolean 
       like [+true], note this ast talks using js
       terminnology unless explicity stated                       
     *)
  | Dump of Js_op.level * expression list
    (* to support 
       val log1 : 'a -> unit
       val log2 : 'a -> 'b -> unit 
       val log3 : 'a -> 'b -> 'c -> unit 
     *)

  (* TODO: Add some primitives so that [js inliner] can do a better job *)  
  | Seq of expression * expression
  | Cond of expression * expression * expression
  | Bin of binop * expression * expression

  (* [int_op] will guarantee return [int32] bits 
     https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Operators/Bitwise_Operators  *)
  (* | Int32_bin of int_op * expression * expression *)
  | FlatCall of expression * expression 
    (* f.apply(null,args) -- Fully applied guaranteed 
       TODO: once we know args's shape --
       if it's know at compile time, we can turn it into
       f(args[0], args[1], ... )
     *)
  | Call of expression * expression list * Js_call_info.t
    (* Analysze over J expression is hard since, 
        some primitive  call is translated 
        into a plain call, it's better to keep them
    *) 
  | String_access of expression * expression 
  | Access of expression * expression 
    (* Invariant: 
       The second argument has to be type of [int],
       This can be constructed either in a static way [E.index] or a dynamic way 
       [E.access]
     *)
  | Dot of expression * string * bool
    (* The third argument bool indicates whether we should 
       print it as 
       a["idd"] -- false
       or 
       a.idd  -- true
       There are several kinds of properties
       1. OCaml module dot (need to be escaped or not)
          All exported declarations have to be OCaml identifiers
       2. Javascript dot (need to be preserved/or using quote)
     *)
  | New of expression * expression list option (* TODO: option remove *)
  | Var of vident
  | Fun of ident list  * block * Js_fun_env.t
  | Str of bool * string 
    (* A string is UTF-8 encoded, the string may contain
       escape sequences.
       The first argument is used to mark it is non-pure, please
       don't optimize it, since it does have side effec, 
       examples like "use asm;" and our compiler may generate "error;..." 
       which is better to leave it alone
     *)
  | Array of expression list * mutable_flag
  | Number of number
  | Object of property_map

and for_ident_expression = expression (* pure*)

and finish_ident_expression = expression (* pure *)
(* https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/block
   block can be nested, specified in ES3 
 *)

(* Delay some units like [primitive] into JS layer ,
   benefit: better cross module inlining, and smaller IR size?
 *)

(* 
  [closure] captured loop mutable values in the outer loop

  check if it contains loop mutable values, happens in nested loop
  when closured, it's no longer loop mutable value. 
  which means the outer loop mutable value can not peek into the inner loop
  {[
  var i = f ();
  for(var finish = 32; i < finish; ++i){
  }
  ]}
  when [for_ident_expression] is [None], [var i] has to 
  be initialized outside, so 

  {[
  var i = f ()
  (function (xxx){
  for(var finish = 32; i < finish; ++i)
  }(..i))
  ]}
  This happens rare it's okay

  this is because [i] has to be initialized outside, if [j] 
  contains a block side effect
  TODO: create such example
*)

(* Since in OCaml, 
   
  [for i = 0 to k end do done ]
  k is only evaluated once , to encode this invariant in JS IR,
  make sure [ident] is defined in the first b

  TODO: currently we guarantee that [bound] was only 
  excecuted once, should encode this in AST level
*)

(* Can be simplified to keep the semantics of OCaml
   For (var i, e, ...){
     let  j = ... 
   }

   if [i] or [j] is captured inside closure

   for (var i , e, ...){
     (function (){
     })(i)
   }
*)

(* Single return is good for ininling..
   However, when you do tail-call optmization
   you loose the expression oriented semantics
   Block is useful for implementing goto
   {[
   xx:{
   break xx;
   }
   ]}
*)


and statement_desc =
  | Block of block
  | Variable of variable_declaration
        (* Function declaration and Variable declaration  *)
  | Exp of expression
  | If of expression * block * block option
  | While of label option *  expression * block 
        * Js_closure.t (* check if it contains loop mutable values, happens in nested loop *)
  | ForRange of for_ident_expression option * finish_ident_expression * 
        for_ident  *  for_direction * block
        * Js_closure.t  
  | Continue of label 
  | Break (* only used when inline a fucntion *)
  | Return of return_expression   (* Here we need track back a bit ?, move Return to Function ...
                              Then we can only have one Return, which is not good *)
  | Int_switch of expression * int case_clause list * block option 
  | String_switch of expression * string case_clause list * block option 
  | Throw of expression
  | Try of block * (exception_ident * block) option * block option

and return_expression = {
 (* since in ocaml, it's expression oriented langauge, [return] in
    general has no jumps, it only happens when we do 
    tailcall conversion, in that case there is a jump.
    However, currently  a single [break] is good to cover
    our compilation strategy 

    Attention: we should not insert [break] arbitrarily, otherwise 
    it would break the semantics
    A more robust signature would be 
    {[ goto : label option ; ]}
  *)
  return_value : expression
}   

and expression = {
  expression_desc : expression_desc; 
  comment : string option;
} 

and statement = { 
  statement_desc :  statement_desc; 
  comment : string option;
}

and variable_declaration = { 
  ident : ident ;
  value : expression  option;
  property : property;
  ident_info : ident_info;
}

and 'a case_clause = { 
  case : 'a ; 
  body : block * bool ;  (* true means break *)
}

(* TODO: For efficency: block should not be a list, it should be able to 
   be concatenated in both ways 
 *)
and block =  statement list

and program = {
  name :  string;
  modules : required_modules ;
  block : block ;
  exports : exports ;
  export_set : Ident_set.t ;
  side_effect : string option (* None: no, Some reason  *)
}
