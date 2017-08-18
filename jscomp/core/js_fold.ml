(* BuckleScript compiler
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
(** GENERATED CODE for fold visitor patten of JS IR  *)
open J
  
class virtual fold =
  object ((o : 'self_type))
    method string : string -> 'self_type = o#unknown
    method option :
      'a. ('self_type -> 'a -> 'self_type) -> 'a option -> 'self_type =
      fun _f_a -> function | None -> o | Some _x -> let o = _f_a o _x in o
    method list :
      'a. ('self_type -> 'a -> 'self_type) -> 'a list -> 'self_type =
      fun _f_a ->
        function
        | [] -> o
        | _x :: _x_i1 -> let o = _f_a o _x in let o = o#list _f_a _x_i1 in o
    method int : int -> 'self_type = o#unknown
    method bool : bool -> 'self_type = function | false -> o | true -> o
    method vident : vident -> 'self_type =
      function
      | Id _x -> let o = o#ident _x in o
      | Qualified (_x, _x_i1, _x_i2) ->
          let o = o#ident _x in
          let o = o#kind _x_i1 in
          let o = o#option (fun o -> o#string) _x_i2 in o
    method variable_declaration : variable_declaration -> 'self_type =
      fun { ident = _x; value = _x_i1; property = _x_i2; ident_info = _x_i3 }
        ->
        let o = o#ident _x in
        let o = o#option (fun o -> o#expression) _x_i1 in
        let o = o#property _x_i2 in let o = o#ident_info _x_i3 in o
    method tag_info : tag_info -> 'self_type = o#unknown
    method statement_desc : statement_desc -> 'self_type =
      function
      | Block _x -> let o = o#block _x in o
      | Variable _x -> let o = o#variable_declaration _x in o
      | Exp _x -> let o = o#expression _x in o
      | If (_x, _x_i1, _x_i2) ->
          let o = o#expression _x in
          let o = o#block _x_i1 in
          let o = o#option (fun o -> o#block) _x_i2 in o
      | While (_x, _x_i1, _x_i2, _x_i3) ->
          let o = o#option (fun o -> o#label) _x in
          let o = o#expression _x_i1 in
          let o = o#block _x_i2 in let o = o#unknown _x_i3 in o
      | ForRange (_x, _x_i1, _x_i2, _x_i3, _x_i4, _x_i5) ->
          let o = o#option (fun o -> o#for_ident_expression) _x in
          let o = o#finish_ident_expression _x_i1 in
          let o = o#for_ident _x_i2 in
          let o = o#for_direction _x_i3 in
          let o = o#block _x_i4 in let o = o#unknown _x_i5 in o
      | Continue _x -> let o = o#label _x in o
      | Break -> o
      | Return _x -> let o = o#return_expression _x in o
      | Int_switch (_x, _x_i1, _x_i2) ->
          let o = o#expression _x in
          let o =
            o#list
              (fun o ->
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
                 (** object literal, if key is ident, in this case, it might be renamed by 
    Google Closure  optimizer,
    currently we always use quote
 *)
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
                 (** where we use a trick [== null ] *)
                 (* used in [#create_array] primitive, note having
       uninitilized array is not as bad as in ocaml, 
       since GC does not rely on it
     *)
                 (* shallow copy, like [x.slice] *)
                 (* For [caml_array_append]*)
                 (* | Tag_ml_obj of expression *) (* js true/false*)
                 (* https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence 
     [typeof] is an operator     
  *)
                 (* 1 - v *) (* !v *)
                 (* String.fromCharCode.apply(null, args) *)
                 (* Convert JS boolean into OCaml boolean 
       like [+true], note this ast talks using js
       terminnology unless explicity stated                       
     *)
                 (* TODO: in the future, it might make sense to group primitivie by type,
     which makes optimizations easier
     {[ JSON.stringify(value, replacer[, space]) ]}
  *)
                 (* for debugging utitlites, 
     TODO:  [Dump] is not necessary with this primitive 
     Note that the semantics is slightly different from [JSON.stringify]     
     {[
       JSON.stringify("x")       
     ]}
     {[
       ""x""       
     ]}     
     {[
       JSON.stringify(undefined)       
     ]}     
     {[
       undefined       
     ]}
     {[ '' + undefined
     ]}     
     {[ 'undefined'
     ]}     
  *)
                 (* TODO: 
     add 
     {[ Assert of bool * expression ]}     
  *)
                 (* to support 
       val log1 : 'a -> unit
       val log2 : 'a -> 'b -> unit 
       val log3 : 'a -> 'b -> 'c -> unit 
     *)
                 (* TODO: Add some primitives so that [js inliner] can do a better job *)
                 (* [int_op] will guarantee return [int32] bits 
     https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Operators/Bitwise_Operators  *)
                 (* | Int32_bin of int_op * expression * expression *)
                 (* f.apply(null,args) -- Fully applied guaranteed 
       TODO: once we know args's shape --
       if it's know at compile time, we can turn it into
       f(args[0], args[1], ... )
     *)
                 (* {[ Bind (a,b) ]}
     is literally
     {[ a.bind(b) ]}
  *)
                 (* Analysze over J expression is hard since, 
        some primitive  call is translated 
        into a plain call, it's better to keep them
    *)
                 (* Invariant: 
       The second argument has to be type of [int],
       This can be constructed either in a static way [E.index] or a dynamic way 
       [E.access]
     *)
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
                 (* TODO: option remove *)
                 (* The first parameter by default is false, 
     it will be true when it's a method
  *)
                 (* A string is UTF-8 encoded, the string may contain
       escape sequences.
       The first argument is used to mark it is non-pure, please
       don't optimize it, since it does have side effec, 
       examples like "use asm;" and our compiler may generate "error;..." 
       which is better to leave it alone
       The last argument is passed from as `j` from `{j||j}`
     *)
                 (* It is escaped string, print delimited by '"'*)
                 (* literally raw JS code 
  *)
                 (* The third argument is [tag] , forth is [tag_info] *)
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
                 (* It will just fetch tag, to make it safe, when creating it, 
     we need apply "|0", we don't do it in the 
     last step since "|0" can potentially be optimized
  *)
                 (* pure*) (* pure *)
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
                 (* Function declaration and Variable declaration  *)
                 (* check if it contains loop mutable values, happens in nested loop *)
                 (* only used when inline a fucntion *)
                 (* Here we need track back a bit ?, move Return to Function ...
                              Then we can only have one Return, which is not good *)
                 o#case_clause (fun o -> o#int))
              _x_i1 in
          let o = o#option (fun o -> o#block) _x_i2 in o
      | String_switch (_x, _x_i1, _x_i2) ->
          let o = o#expression _x in
          let o =
            o#list (fun o -> o#case_clause (fun o -> o#string)) _x_i1 in
          let o = o#option (fun o -> o#block) _x_i2 in o
      | Throw _x -> let o = o#expression _x in o
      | Try (_x, _x_i1, _x_i2) ->
          let o = o#block _x in
          let o =
            o#option
              (fun o (_x, _x_i1) ->
                 let o = o#exception_ident _x in let o = o#block _x_i1 in o)
              _x_i1 in
          let o = o#option (fun o -> o#block) _x_i2 in o
      | Debugger -> o
    method statement : statement -> 'self_type =
      fun { statement_desc = _x; comment = _x_i1 } ->
        let o = o#statement_desc _x in
        let o = o#option (fun o -> o#string) _x_i1 in o
    method return_expression : return_expression -> 'self_type =
      fun { return_value = _x } -> let o = o#expression _x in o
    method required_modules : required_modules -> 'self_type = o#unknown
    method property_name : property_name -> 'self_type = o#unknown
    method property_map : property_map -> 'self_type =
      o#list
        (fun o (_x, _x_i1) ->
           let o = o#property_name _x in let o = o#expression _x_i1 in o)
    method property : property -> 'self_type = o#unknown
    method program : program -> 'self_type =
      fun { name = _x; block = _x_i1; exports = _x_i2; export_set = _x_i3 }
        ->
        let o = o#string _x in
        let o = o#block _x_i1 in
        let o = o#exports _x_i2 in let o = o#unknown _x_i3 in o
    method number : number -> 'self_type = o#unknown
    method mutable_flag : mutable_flag -> 'self_type = o#unknown
    method length_object : length_object -> 'self_type = o#unknown
    method label : label -> 'self_type = o#string
    method kind : kind -> 'self_type = o#unknown
    method jsint : jsint -> 'self_type = o#unknown
    method int_op : int_op -> 'self_type = o#unknown
    method ident_info : ident_info -> 'self_type = o#unknown
    method ident : ident -> 'self_type = o#unknown
    method for_ident_expression : for_ident_expression -> 'self_type =
      o#expression
    method for_ident : for_ident -> 'self_type = o#ident
    method for_direction : for_direction -> 'self_type = o#unknown
    method finish_ident_expression : finish_ident_expression -> 'self_type =
      o#expression
    method expression_desc : expression_desc -> 'self_type =
      function
      | Math (_x, _x_i1) ->
          let o = o#string _x in
          let o = o#list (fun o -> o#expression) _x_i1 in o
      | Length (_x, _x_i1) ->
          let o = o#expression _x in let o = o#length_object _x_i1 in o
      | Char_of_int _x -> let o = o#expression _x in o
      | Char_to_int _x -> let o = o#expression _x in o
      | Is_null_undefined _x -> let o = o#expression _x in o
      | Array_of_size _x -> let o = o#expression _x in o
      | Array_copy _x -> let o = o#expression _x in o
      | Array_append (_x, _x_i1) ->
          let o = o#expression _x in let o = o#expression _x_i1 in o
      | String_append (_x, _x_i1) ->
          let o = o#expression _x in let o = o#expression _x_i1 in o
      | Int_of_boolean _x -> let o = o#expression _x in o
      | Anything_to_number _x -> let o = o#expression _x in o
      | Bool _x -> let o = o#bool _x in o
      | Typeof _x -> let o = o#expression _x in o
      | Caml_not _x -> let o = o#expression _x in o
      | Js_not _x -> let o = o#expression _x in o
      | String_of_small_int_array _x -> let o = o#expression _x in o
      | Json_stringify _x -> let o = o#expression _x in o
      | Anything_to_string _x -> let o = o#expression _x in o
      | Dump (_x, _x_i1) ->
          let o = o#unknown _x in
          let o = o#list (fun o -> o#expression) _x_i1 in o
      | Seq (_x, _x_i1) ->
          let o = o#expression _x in let o = o#expression _x_i1 in o
      | Cond (_x, _x_i1, _x_i2) ->
          let o = o#expression _x in
          let o = o#expression _x_i1 in let o = o#expression _x_i2 in o
      | Bin (_x, _x_i1, _x_i2) ->
          let o = o#binop _x in
          let o = o#expression _x_i1 in let o = o#expression _x_i2 in o
      | FlatCall (_x, _x_i1) ->
          let o = o#expression _x in let o = o#expression _x_i1 in o
      | Bind (_x, _x_i1) ->
          let o = o#expression _x in let o = o#expression _x_i1 in o
      | Call (_x, _x_i1, _x_i2) ->
          let o = o#expression _x in
          let o = o#list (fun o -> o#expression) _x_i1 in
          let o = o#unknown _x_i2 in o
      | String_access (_x, _x_i1) ->
          let o = o#expression _x in let o = o#expression _x_i1 in o
      | Access (_x, _x_i1) ->
          let o = o#expression _x in let o = o#expression _x_i1 in o
      | Dot (_x, _x_i1, _x_i2) ->
          let o = o#expression _x in
          let o = o#string _x_i1 in let o = o#bool _x_i2 in o
      | New (_x, _x_i1) ->
          let o = o#expression _x in
          let o = o#option (fun o -> o#list (fun o -> o#expression)) _x_i1
          in o
      | Var _x -> let o = o#vident _x in o
      | Fun (_x, _x_i1, _x_i2, _x_i3) ->
          let o = o#bool _x in
          let o = o#list (fun o -> o#ident) _x_i1 in
          let o = o#block _x_i2 in let o = o#unknown _x_i3 in o
      | Str (_x, _x_i1) -> let o = o#bool _x in let o = o#string _x_i1 in o
      | Unicode _x -> let o = o#string _x in o
      | Raw_js_code (_x, _x_i1) ->
          let o = o#string _x in let o = o#code_info _x_i1 in o
      | Array (_x, _x_i1) ->
          let o = o#list (fun o -> o#expression) _x in
          let o = o#mutable_flag _x_i1 in o
      | Caml_block (_x, _x_i1, _x_i2, _x_i3) ->
          let o = o#list (fun o -> o#expression) _x in
          let o = o#mutable_flag _x_i1 in
          let o = o#expression _x_i2 in let o = o#tag_info _x_i3 in o
      | Caml_uninitialized_obj (_x, _x_i1) ->
          let o = o#expression _x in let o = o#expression _x_i1 in o
      | Caml_block_tag _x -> let o = o#expression _x in o
      | Caml_block_set_tag (_x, _x_i1) ->
          let o = o#expression _x in let o = o#expression _x_i1 in o
      | Caml_block_set_length (_x, _x_i1) ->
          let o = o#expression _x in let o = o#expression _x_i1 in o
      | Number _x -> let o = o#number _x in o
      | Object _x -> let o = o#property_map _x in o
    method expression : expression -> 'self_type =
      fun { expression_desc = _x; comment = _x_i1 } ->
        let o = o#expression_desc _x in
        let o = o#option (fun o -> o#string) _x_i1 in o
    method exports : exports -> 'self_type = o#unknown
    method exception_ident : exception_ident -> 'self_type = o#ident
    method deps_program : deps_program -> 'self_type =
      fun { program = _x; modules = _x_i1; side_effect = _x_i2 } ->
        let o = o#program _x in
        let o = o#required_modules _x_i1 in
        let o = o#option (fun o -> o#string) _x_i2 in o
    method code_info : code_info -> 'self_type = o#unknown
    method case_clause :
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
        'a. ('self_type -> 'a -> 'self_type) -> 'a case_clause -> 'self_type =
      fun _f_a { case = _x; body = _x_i1 } ->
        let o = _f_a o _x in
        let o =
          (fun (_x, _x_i1) -> let o = o#block _x in let o = o#bool _x_i1 in o)
            _x_i1
        in o
    method block : block -> 'self_type = (* true means break *)
      (* TODO: For efficency: block should not be a list, it should be able to 
   be concatenated in both ways 
 *)
      o#list (fun o -> o#statement)
    method binop : binop -> 'self_type = o#unknown
    method unknown : 'a. 'a -> 'self_type = fun _ -> o
  end
  

