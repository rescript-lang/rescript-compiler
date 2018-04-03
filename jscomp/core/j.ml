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

and tag_info = Js_op.tag_info 
 
and required_modules = Js_op.required_modules

and code_info = Js_op.code_info 
(** object literal, if key is ident, in this case, it might be renamed by 
    Google Closure  optimizer,
    currently we always use quote
 *)
and property_name =  Js_op.property_name
and jsint = Js_op.jsint
and ident = Ident.t 

and vident = 
  | Id of ident
  | Qualified of ident * kind * string option
    (* Since camldot is only available for toplevel module accessors,
       we don't need print  `A.length$2`
       just print `A.length` - it's guarateed to be unique
       
       when the third one is None, it means the whole module 

       TODO: 
       invariant, when [kind] is [Runtime], then we can ignore [ident], 
       since all [runtime] functions are unique, when do the 
       pattern match we can ignore the first one for simplicity
       for example       
       {[
         Qualified (_, Runtime, Some "caml_int_compare")         
       ]}       
     *)

and exception_ident = ident 

and for_ident = ident 

and for_direction = Asttypes.direction_flag

and property_map = 
    (property_name * expression) list
and length_object = Js_op.length_object
and expression_desc =
  | Math of string * expression list
  | Length of expression * length_object
  | Char_of_int of expression
  | Char_to_int of expression 
  | Is_null_undefined_to_boolean of expression 
    (** where we use a trick [== null ] *)
  | Array_of_size of expression 
    (* used in [#create_array] primitive, note having
       uninitilized array is not as bad as in ocaml, 
       since GC does not rely on it
     *)
  | Array_copy of expression (* shallow copy, like [x.slice] *)

  | String_append of expression * expression 
  | Bool of bool (* js true/false*)
  (* https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence 
     [typeof] is an operator     
  *)
  | Typeof of expression
  | Js_not of expression (* !v *)
  | String_of_small_int_array of expression 
    (* String.fromCharCode.apply(null, args) *)
    (* Convert JS boolean into OCaml boolean 
       like [+true], note this ast talks using js
       terminnology unless explicity stated                       
     *)

  (* TODO: 
     add 
     {[ Assert of bool * expression ]}     
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
  | Fun of bool * ident list  * block * Js_fun_env.t
  (* The first parameter by default is false, 
     it will be true when it's a method
  *)
  | Str of bool * string 
    (* A string is UTF-8 encoded, the string may contain
       escape sequences.
       The first argument is used to mark it is non-pure, please
       don't optimize it, since it does have side effec, 
       examples like "use asm;" and our compiler may generate "error;..." 
       which is better to leave it alone
       The last argument is passed from as `j` from `{j||j}`
     *)
  | Unicode of string 
    (* It is escaped string, print delimited by '"'*)   
  | Raw_js_code of string * code_info
  (* literally raw JS code 
  *)
  | Array of expression list * mutable_flag
  | Caml_block of expression list * mutable_flag * expression * tag_info 
  (* The third argument is [tag] , forth is [tag_info] *)
  (* | Caml_uninitialized_obj of expression * expression *)
  (* [tag] and [size] tailed  for [Obj.new_block] *)

  (* For setter, it still return the value of expression, 
     we can not use 
     {[
       type 'a access = Get | Set of 'a
     ]}
     in another module, since it will break our code generator
     [Caml_block_tag] can return [undefined], 
     you have to use [E.tag] in a safe way     
  *)
  | Caml_block_tag of expression
  | Caml_block_set_tag of expression * expression
  | Caml_block_set_length of expression * expression
  (* It will just fetch tag, to make it safe, when creating it, 
     we need apply "|0", we don't do it in the 
     last step since "|0" can potentially be optimized
  *)      
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
  | Debugger
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
  switch_case : 'a ; 
  switch_body : block * bool ;  (* true means break *)
}

(* TODO: For efficency: block should not be a list, it should be able to 
   be concatenated in both ways 
 *)
and block =  statement list

and program = {
  name :  string;

  block : block ;
  exports : exports ;
  export_set : Ident_set.t ;

}
and deps_program = 
  {
    program : program ; 
    modules : required_modules ;
    side_effect : string option (* None: no, Some reason  *)
  }
