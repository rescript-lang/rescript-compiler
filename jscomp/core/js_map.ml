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
(** GENERATED CODE, map visitor for JS IR  *)
open J
  
class virtual map =
  object ((o : 'self_type))
    method string : string -> string = o#unknown
    method option :
      'a 'a_out. ('self_type -> 'a -> 'a_out) -> 'a option -> 'a_out option =
      fun _f_a ->
        function | None -> None | Some _x -> let _x = _f_a o _x in Some _x
    method list :
      'a 'a_out. ('self_type -> 'a -> 'a_out) -> 'a list -> 'a_out list =
      fun _f_a ->
        function
        | [] -> []
        | _x :: _x_i1 ->
            let _x = _f_a o _x in
            let _x_i1 = o#list _f_a _x_i1 in _x :: _x_i1
    method int : int -> int = o#unknown
    method bool : bool -> bool = function | false -> false | true -> true
    method vident : vident -> vident =
      function
      | Id _x -> let _x = o#ident _x in Id _x
      | Qualified (_x, _x_i1, _x_i2) ->
          let _x = o#ident _x in
          let _x_i1 = o#kind _x_i1 in
          let _x_i2 = o#option (fun o -> o#string) _x_i2
          in Qualified (_x, _x_i1, _x_i2)
    method variable_declaration :
      variable_declaration -> variable_declaration =
      fun { ident = _x; value = _x_i1; property = _x_i2; ident_info = _x_i3 }
        ->
        let _x = o#ident _x in
        let _x_i1 = o#option (fun o -> o#expression) _x_i1 in
        let _x_i2 = o#property _x_i2 in
        let _x_i3 = o#ident_info _x_i3
        in
          { ident = _x; value = _x_i1; property = _x_i2; ident_info = _x_i3;
          }
    method tag_info : tag_info -> tag_info = o#unknown
    method statement_desc : statement_desc -> statement_desc =
      function
      | Block _x -> let _x = o#block _x in Block _x
      | Variable _x -> let _x = o#variable_declaration _x in Variable _x
      | Exp _x -> let _x = o#expression _x in Exp _x
      | If (_x, _x_i1, _x_i2) ->
          let _x = o#expression _x in
          let _x_i1 = o#block _x_i1 in
          let _x_i2 = o#option (fun o -> o#block) _x_i2
          in If (_x, _x_i1, _x_i2)
      | While (_x, _x_i1, _x_i2, _x_i3) ->
          let _x = o#option (fun o -> o#label) _x in
          let _x_i1 = o#expression _x_i1 in
          let _x_i2 = o#block _x_i2 in
          let _x_i3 = o#unknown _x_i3 in While (_x, _x_i1, _x_i2, _x_i3)
      | ForRange (_x, _x_i1, _x_i2, _x_i3, _x_i4, _x_i5) ->
          let _x = o#option (fun o -> o#for_ident_expression) _x in
          let _x_i1 = o#finish_ident_expression _x_i1 in
          let _x_i2 = o#for_ident _x_i2 in
          let _x_i3 = o#for_direction _x_i3 in
          let _x_i4 = o#block _x_i4 in
          let _x_i5 = o#unknown _x_i5
          in ForRange (_x, _x_i1, _x_i2, _x_i3, _x_i4, _x_i5)
      | Continue _x -> let _x = o#label _x in Continue _x
      | Break -> Break
      | Return _x -> let _x = o#return_expression _x in Return _x
      | Int_switch (_x, _x_i1, _x_i2) ->
          let _x = o#expression _x in
          let _x_i1 =
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
                 (* shallow copy, like [x.slice] *) (* js true/false*)
                 (* https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence 
     [typeof] is an operator     
  *)
                 (* !v *)
                 (* TODO: Add some primitives so that [js inliner] can do a better job *)
                 (* [int_op] will guarantee return [int32] bits 
     https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Operators/Bitwise_Operators  *)
                 (* | Int32_bin of int_op * expression * expression *)
                 (* f.apply(null,args) -- Fully applied guaranteed 
       TODO: once we know args's shape --
       if it's know at compile time, we can turn it into
       f(args[0], args[1], ... )
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
          let _x_i2 = o#option (fun o -> o#block) _x_i2
          in Int_switch (_x, _x_i1, _x_i2)
      | String_switch (_x, _x_i1, _x_i2) ->
          let _x = o#expression _x in
          let _x_i1 =
            o#list (fun o -> o#case_clause (fun o -> o#string)) _x_i1 in
          let _x_i2 = o#option (fun o -> o#block) _x_i2
          in String_switch (_x, _x_i1, _x_i2)
      | Throw _x -> let _x = o#expression _x in Throw _x
      | Try (_x, _x_i1, _x_i2) ->
          let _x = o#block _x in
          let _x_i1 =
            o#option
              (fun o (_x, _x_i1) ->
                 let _x = o#exception_ident _x in
                 let _x_i1 = o#block _x_i1 in (_x, _x_i1))
              _x_i1 in
          let _x_i2 = o#option (fun o -> o#block) _x_i2
          in Try (_x, _x_i1, _x_i2)
      | Debugger -> Debugger
    method statement : statement -> statement =
      fun { statement_desc = _x; comment = _x_i1 } ->
        let _x = o#statement_desc _x in
        let _x_i1 = o#option (fun o -> o#string) _x_i1
        in { statement_desc = _x; comment = _x_i1; }
    method return_expression : return_expression -> return_expression =
      fun { return_value = _x } ->
        let _x = o#expression _x in { return_value = _x; }
    method required_modules : required_modules -> required_modules =
      o#unknown
    method property_name : property_name -> property_name = o#unknown
    method property_map : property_map -> property_map =
      o#list
        (fun o (_x, _x_i1) ->
           let _x = o#property_name _x in
           let _x_i1 = o#expression _x_i1 in (_x, _x_i1))
    method property : property -> property = o#unknown
    method program : program -> program =
      fun { name = _x; block = _x_i1; exports = _x_i2; export_set = _x_i3 }
        ->
        let _x = o#string _x in
        let _x_i1 = o#block _x_i1 in
        let _x_i2 = o#exports _x_i2 in
        let _x_i3 = o#unknown _x_i3
        in { name = _x; block = _x_i1; exports = _x_i2; export_set = _x_i3; }
    method number : number -> number = o#unknown
    method mutable_flag : mutable_flag -> mutable_flag = o#unknown
    method length_object : length_object -> length_object = o#unknown
    method label : label -> label = o#string
    method kind : kind -> kind = o#unknown
    method jsint : jsint -> jsint = o#unknown
    method int_op : int_op -> int_op = o#unknown
    method ident_info : ident_info -> ident_info = o#unknown
    method ident : ident -> ident = o#unknown
    method for_ident_expression :
      for_ident_expression -> for_ident_expression = o#expression
    method for_ident : for_ident -> for_ident = o#ident
    method for_direction : for_direction -> for_direction = o#unknown
    method finish_ident_expression :
      finish_ident_expression -> finish_ident_expression = o#expression
    method expression_desc : expression_desc -> expression_desc =
      function
      | Math (_x, _x_i1) ->
          let _x = o#string _x in
          let _x_i1 = o#list (fun o -> o#expression) _x_i1
          in Math (_x, _x_i1)
      | Length (_x, _x_i1) ->
          let _x = o#expression _x in
          let _x_i1 = o#length_object _x_i1 in Length (_x, _x_i1)
      | Char_of_int _x -> let _x = o#expression _x in Char_of_int _x
      | Char_to_int _x -> let _x = o#expression _x in Char_to_int _x
      | Is_null_or_undefined _x ->
          let _x = o#expression _x in Is_null_or_undefined _x
      | Array_copy _x -> let _x = o#expression _x in Array_copy _x
      | String_append (_x, _x_i1) ->
          let _x = o#expression _x in
          let _x_i1 = o#expression _x_i1 in String_append (_x, _x_i1)
      | Bool _x -> let _x = o#bool _x in Bool _x
      | Typeof _x -> let _x = o#expression _x in Typeof _x
      | Js_not _x -> let _x = o#expression _x in Js_not _x
      | Seq (_x, _x_i1) ->
          let _x = o#expression _x in
          let _x_i1 = o#expression _x_i1 in Seq (_x, _x_i1)
      | Cond (_x, _x_i1, _x_i2) ->
          let _x = o#expression _x in
          let _x_i1 = o#expression _x_i1 in
          let _x_i2 = o#expression _x_i2 in Cond (_x, _x_i1, _x_i2)
      | Bin (_x, _x_i1, _x_i2) ->
          let _x = o#binop _x in
          let _x_i1 = o#expression _x_i1 in
          let _x_i2 = o#expression _x_i2 in Bin (_x, _x_i1, _x_i2)
      | FlatCall (_x, _x_i1) ->
          let _x = o#expression _x in
          let _x_i1 = o#expression _x_i1 in FlatCall (_x, _x_i1)
      | Call (_x, _x_i1, _x_i2) ->
          let _x = o#expression _x in
          let _x_i1 = o#list (fun o -> o#expression) _x_i1 in
          let _x_i2 = o#unknown _x_i2 in Call (_x, _x_i1, _x_i2)
      | String_access (_x, _x_i1) ->
          let _x = o#expression _x in
          let _x_i1 = o#expression _x_i1 in String_access (_x, _x_i1)
      | Access (_x, _x_i1) ->
          let _x = o#expression _x in
          let _x_i1 = o#expression _x_i1 in Access (_x, _x_i1)
      | Dot (_x, _x_i1, _x_i2) ->
          let _x = o#expression _x in
          let _x_i1 = o#string _x_i1 in
          let _x_i2 = o#bool _x_i2 in Dot (_x, _x_i1, _x_i2)
      | New (_x, _x_i1) ->
          let _x = o#expression _x in
          let _x_i1 =
            o#option (fun o -> o#list (fun o -> o#expression)) _x_i1
          in New (_x, _x_i1)
      | Var _x -> let _x = o#vident _x in Var _x
      | Fun (_x, _x_i1, _x_i2, _x_i3) ->
          let _x = o#bool _x in
          let _x_i1 = o#list (fun o -> o#ident) _x_i1 in
          let _x_i2 = o#block _x_i2 in
          let _x_i3 = o#unknown _x_i3 in Fun (_x, _x_i1, _x_i2, _x_i3)
      | Str (_x, _x_i1) ->
          let _x = o#bool _x in let _x_i1 = o#string _x_i1 in Str (_x, _x_i1)
      | Unicode _x -> let _x = o#string _x in Unicode _x
      | Raw_js_code (_x, _x_i1) ->
          let _x = o#string _x in
          let _x_i1 = o#code_info _x_i1 in Raw_js_code (_x, _x_i1)
      | Array (_x, _x_i1) ->
          let _x = o#list (fun o -> o#expression) _x in
          let _x_i1 = o#mutable_flag _x_i1 in Array (_x, _x_i1)
      | Caml_block (_x, _x_i1, _x_i2, _x_i3) ->
          let _x = o#list (fun o -> o#expression) _x in
          let _x_i1 = o#mutable_flag _x_i1 in
          let _x_i2 = o#expression _x_i2 in
          let _x_i3 = o#tag_info _x_i3
          in Caml_block (_x, _x_i1, _x_i2, _x_i3)
      | Caml_block_tag _x -> let _x = o#expression _x in Caml_block_tag _x
      | Caml_block_set_tag (_x, _x_i1) ->
          let _x = o#expression _x in
          let _x_i1 = o#expression _x_i1 in Caml_block_set_tag (_x, _x_i1)
      | Caml_block_set_length (_x, _x_i1) ->
          let _x = o#expression _x in
          let _x_i1 = o#expression _x_i1 in Caml_block_set_length (_x, _x_i1)
      | Number _x -> let _x = o#number _x in Number _x
      | Object _x -> let _x = o#property_map _x in Object _x
    method expression : expression -> expression =
      fun { expression_desc = _x; comment = _x_i1 } ->
        let _x = o#expression_desc _x in
        let _x_i1 = o#option (fun o -> o#string) _x_i1
        in { expression_desc = _x; comment = _x_i1; }
    method exports : exports -> exports = o#unknown
    method exception_ident : exception_ident -> exception_ident = o#ident
    method deps_program : deps_program -> deps_program =
      fun { program = _x; modules = _x_i1; side_effect = _x_i2 } ->
        let _x = o#program _x in
        let _x_i1 = o#required_modules _x_i1 in
        let _x_i2 = o#option (fun o -> o#string) _x_i2
        in { program = _x; modules = _x_i1; side_effect = _x_i2; }
    method code_info : code_info -> code_info = o#unknown
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
        'a 'a_out.
        ('self_type -> 'a -> 'a_out) -> 'a case_clause -> 'a_out case_clause =
      fun _f_a { switch_case = _x; switch_body = _x_i1 } ->
        let _x = _f_a o _x in
        let _x_i1 =
          (fun (_x, _x_i1) ->
             let _x = o#block _x in let _x_i1 = o#bool _x_i1 in (_x, _x_i1))
            _x_i1
        in { switch_case = _x; switch_body = _x_i1; }
    method block : block -> block = (* true means break *)
      (* TODO: For efficency: block should not be a list, it should be able to 
   be concatenated in both ways 
 *)
      o#list (fun o -> o#statement)
    method binop : binop -> binop = o#unknown
    method unknown : 'a. 'a -> 'a = fun x -> x
  end
  

