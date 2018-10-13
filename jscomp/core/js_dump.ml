(* BuckleScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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
(* Authors: Jérôme Vouillon, Hongbo Zhang  *)
#if OCAML_VERSION =~ ">4.03.0" then
[@@@ocaml.warning "-57"] (* turn off such warning temporarily*)
#end
(*
  http://stackoverflow.com/questions/2846283/what-are-the-rules-for-javascripts-automatic-semicolon-insertion-asi
  ASI catch up
   {[
     a=b
       ++c
       ---
       a=b ++c
     ====================
     a ++
     ---
     a
     ++
     ====================
     a --
     ---
     a
     --
     ====================
     (continue/break/return/throw) a
     ---
     (continue/break/return/throw)
       a
     ====================
   ]}

*)


module P = Ext_pp
module E = Js_exp_make
module S = Js_stmt_make

module L = Js_dump_lit

module Curry_gen = struct 
  
  let pp_optimize_curry (f : P.t) (len : int) = 
    P.string f Js_runtime_modules.curry;
    P.string f L.dot ; 
    P.string f "__";
    P.string f (Printf.sprintf "%d" len)

  let pp_app_any (f : P.t) =    
    P.string f Js_runtime_modules.curry;
    P.string f L.dot ; 
    P.string f "app"

  let pp_app (f : P.t) (len : int) =    
    P.string f Js_runtime_modules.curry; 
    P.string f L.dot; 
    P.string f "_"; 
    P.string f (Printf.sprintf "%d" len)
end 

let return_indent = String.length L.return / Ext_pp.indent_length

let throw_indent = String.length L.throw / Ext_pp.indent_length

type cxt = Ext_pp_scope.t

let semi f = P.string f L.semi
let comma f = P.string f L.comma

let rec iter_lst cxt (f : P.t) ls element inter = 
  match ls with 
  | [] -> cxt 
  | [e] -> element cxt f e 
  | e::r -> 
    let acxt = element cxt f e  in 
    inter f; 
    iter_lst acxt f r element inter


(* Parentheses are required when the expression
   starts syntactically with "{" or "function"
   TODO:  be more conservative, since Google Closure will handle
   the precedence correctly, we also need people read the code..
   Here we force parens for some alien operators

   If we move assign into a statement, will be less?
   TODO: construct a test case that do need parenthesisze for expression
   IIE does not apply (will be inlined?)
*)

(* e = function(x){...}(x);  is good
*)        
let exp_need_paren  (e : J.expression) =
  match e.expression_desc with
  | Call ({expression_desc = Fun _; },_,_) -> true
  (* | Caml_uninitialized_obj _  *)
  | Raw_js_code (_, Exp)
  | Fun _ 
  | Raw_js_function _ 
  | Object _ -> true
  | Raw_js_code (_,Stmt)
  | Length _
  | Call _
  | Caml_block_tag _
  | Seq _
  | Dot _
  | Cond _
  | Bin _
  | Is_null_or_undefined _
  | String_index _
  | Array_index _
  | String_append _
  | Char_of_int _
  | Char_to_int _
  | Var _
  | Undefined
  | Null
  | Str _
  | Unicode _
  | Array _
  | Optional_block _
  | Caml_block  _
  | FlatCall _
  | Typeof _
  | Number _
  | Js_not _
  | Bool _
  | New _
    -> false

let comma_strings f ls =     
  iter_lst 
  () f 
  ls 
  (fun _ f  a  -> P.string f a )
  comma


let comma_idents (cxt: cxt) f ls =     
  iter_lst cxt f ls   
  Ext_pp_scope.ident
  comma

let pp_paren_params 
  (inner_cxt : cxt) (f : Ext_pp.t) 
  (lexical : Ident.t list) : unit =   
  P.string f L.lparen;
  ignore @@ comma_idents inner_cxt f lexical;
  P.string f L.rparen

(** Print as underscore for unused vars, may not be 
    needed in the future *)  
let ipp_ident cxt f id (un_used : bool) =
  Ext_pp_scope.ident cxt f (
    if un_used then
      Ext_ident.make_unused () 
    else
      id)

let pp_var_assign cxt f id  =       
  P.string f L.var ; 
  P.space f ; 
  let acxt = Ext_pp_scope.ident cxt f id in 
  P.space f ; 
  P.string f L.eq ; 
  P.space f ;
  acxt 

let pp_var_assign_this cxt f id =   
  let cxt = pp_var_assign cxt f id in 
  P.string f L.this;
  P.space f ;
  semi f ;
  P.newline f ;
  cxt 

let pp_var_declare cxt f id = 
  P.string f L.var ; 
  P.space f ; 
  let acxt = Ext_pp_scope.ident cxt f id in 
  semi f ;
  acxt 

let pp_direction f (direction : J.for_direction) =   
  match direction with
  | Upto -> P.string f L.plus_plus
  | Downto -> P.string f L.minus_minus 

let return_sp f = 
    P.string f L.return ; P.space f   

let bool f b =     
  P.string f (if b then  L.true_ else L.false_)

let comma_sp f =   
  comma f ; P.space f 
let comma_nl f = 
  comma f ; P.newline f 

let debugger_nl f =   
  P.newline f ; 
  P.string f L.debugger;
  semi f ;
  P.newline f 

let break_nl f =   
  P.string f L.break;
  P.space f ;
  semi f;
  P.newline f

let continue f s =   
  P.string f L.continue;
  P.space f ;
  P.string f s;
  semi f

let formal_parameter_list cxt f offset l env  = 
    iter_lst cxt f l Ext_pp_scope.ident comma_sp 
(* IdentMap *)
(*
f/122 -->
  f/122 is in the map
  if in, use the old mapping
  else
    check  f,
     if in last bumped id
     else
        use "f", register it

  check "f"
         if not , use "f", register stamp -> 0
         else
           check stamp
             if in  use it
             else check last bumped id, increase it and register
*)
type name =
  | No_name
  | Name_top  of Ident.t
  | Name_non_top of Ident.t


(**
   Turn [function f (x,y) { return a (x,y)} ] into [Curry.__2(a)],
   The idea is that [Curry.__2] will guess the arity of [a], if it does
   hit, then there is no cost when passed
*)

let is_var (b : J.expression)  a = 
  match b.expression_desc with 
  | Var (Id i) -> Ident.same i a 
  | _ -> false

(* TODO: refactoring
   Note that {!pp_function} could print both statement and expression when [No_name] is given
*)
let rec
  try_optimize_curry cxt f len function_id =
  Curry_gen.pp_optimize_curry f len ; 
  P.paren_group f 1 (fun _ -> expression 1 cxt f function_id  )


and  pp_function is_method
    cxt (f : P.t) ?(name=No_name)  (return : bool)
    (l : Ident.t list) (b : J.block) (env : Js_fun_env.t ) : cxt =
  match b, (name,  return)  with
  | [ {statement_desc =
         Return {return_value =
                   {expression_desc =
                      Call(({expression_desc = Var v ; _} as function_id),
                           ls ,
                           {arity = ( Full | NA as arity(* see #234*));
                            (* TODO: need a case to justify it*)
                            call_info =
                              (Call_builtin_runtime | Call_ml )})}}}],
    ((_, false) | (No_name, true))
    when
      (* match such case:
         {[ function(x,y){ return u(x,y) } ]}
         it can be optimized in to either [u] or [Curry.__n(u)]
      *)
      not is_method &&
      Ext_list.for_all2_no_exn ls l  is_var  ->
    let optimize  len p cxt f v =
      if p then try_optimize_curry cxt f len function_id
      else
        vident cxt f v in
    let len = List.length l in (* length *)
    (match name with
     | Name_top i | Name_non_top i  ->       
       let cxt = pp_var_assign cxt f i in 
       let cxt = optimize len (arity = NA && len <= 8) cxt f v in
       semi f ;
       cxt
     | No_name ->
       if return then 
         return_sp f ;
       optimize len (arity = NA && len <=8) cxt f v)

  | _, _  ->
    let set_env : Ident_set.t = (** identifiers will be printed following*)
      match name with
      | No_name ->
        Js_fun_env.get_unbounded env
      | Name_top id | Name_non_top id -> 
        Ident_set.add id (Js_fun_env.get_unbounded env ) in
    (* the context will be continued after this function *)
    let outer_cxt = Ext_pp_scope.merge set_env cxt in

    (* the context used to be printed inside this function

       when printing a function,
       only the enclosed variables and function name matters,
       if the function does not capture any variable, then the context is empty
    *)
    let inner_cxt = Ext_pp_scope.sub_scope outer_cxt set_env in


    (* (if not @@ Js_fun_env.is_empty env then *)
    (* pp_comment  f (Some (Js_fun_env.to_string env))) ; *)
    let param_body () =
      if is_method then
        match l with 
        | [] -> assert false 
        | this::arguments -> 
          let cxt = P.paren_group f 1 (fun _ ->
              formal_parameter_list inner_cxt  f 1 arguments env )
          in
          P.space f ;
          ignore @@ P.brace_vgroup f 1 (fun _ ->
              let cxt =
                if Js_fun_env.get_unused env 0 then cxt 
                else  pp_var_assign_this cxt f this in
              function_body cxt f b
            );
      else 
        let cxt = 
          P.paren_group f 1 (fun _ -> formal_parameter_list inner_cxt  f 0 l env ) in
        P.space f ;
        ignore @@ P.brace_vgroup f 1 (fun _ -> function_body cxt f b ) in
    let lexical : Ident_set.t = Js_fun_env.get_lexical_scope env in
    let enclose  lexical  return =
      let handle lexical =
        if  Ident_set.is_empty lexical
        then
          (if return then
             return_sp f ;
           match name with
           | No_name ->
             (* see # 1692, add a paren for annoymous function for safety  *)
             P.paren_group f 1  (fun _ ->
                 P.string f L.function_;
                 P.space f ;
                 param_body ())                
           | Name_non_top x  ->
             ignore @@ pp_var_assign inner_cxt f x ; 
             P.string f L.function_;
             P.space f ;
             param_body ();
             semi f 
           | Name_top x  ->
             P.string f L.function_;
             P.space f ;
             ignore (Ext_pp_scope.ident inner_cxt f x);
             param_body ())
        else
          (* print as
             {[(function(x,y){...} (x,y))]}
          *)
          let lexical = Ident_set.elements lexical in
          (if return then 
             return_sp f             
           else
             match name with
             | No_name -> ()
             | Name_non_top name | Name_top name->
               ignore @@ pp_var_assign inner_cxt f name              
          )
          ;
          P.string f L.lparen;
          P.string f L.function_;
          pp_paren_params inner_cxt f lexical; 
          P.brace_vgroup f 0  (fun _ ->
              return_sp f;
              P.string f L.function_;
              P.space f ;
              (match name with
               | No_name  -> ()
               | Name_non_top x | Name_top x -> ignore (Ext_pp_scope.ident inner_cxt f x));
              param_body ());
          pp_paren_params inner_cxt f lexical;     
          P.string f L.rparen;
          match name with
          | No_name -> () (* expression *)
          | _ -> semi f (* has binding, a statement *)  in
      handle 
        (match name with
         | Name_top name | Name_non_top name  when Ident_set.mem name lexical ->
           (*TODO: when calculating lexical we should not include itself *)
           Ident_set.remove name lexical
         | _ -> lexical) in
    enclose lexical return;
    outer_cxt


(* Assume the cond would not change the context,
    since it can be either [int] or [string]
*)
and pp_one_case_clause : 'a .
  _ -> P.t -> (P.t -> 'a -> unit) -> 'a J.case_clause -> _
  = fun cxt f  pp_cond
    ({switch_case; switch_body ; should_break } : _ J.case_clause) ->
    let cxt =
      P.group f 1  (fun _ ->
          P.group f 1 (fun _ ->
              P.string f L.case;
              P.space f ;
              pp_cond  f switch_case; (* could be integer or string *)
              P.space f ;
              P.string f L.colon  );
          P.space f;
          P.group f 1 (fun _ ->
              let cxt =
                match switch_body with
                | [] -> cxt
                | _ ->
                  P.newline f ;
                  statement_list false cxt  f switch_body
              in
              (if should_break then
                 begin
                   P.newline f ;
                   P.string f L.break;
                   semi f;
                 end) ;
              cxt))
    in
    P.newline f;
    cxt

and loop_case_clauses  :  'a . cxt ->
  P.t -> (P.t -> 'a -> unit) -> 'a J.case_clause list -> cxt
  = fun  cxt  f pp_cond cases ->
    Ext_list.fold_left cases cxt (fun x acc -> pp_one_case_clause acc f pp_cond x)              

and vident cxt f  (v : J.vident) =
  match v with
  | Id v | Qualified(v, _, None) ->
    Ext_pp_scope.ident cxt f v
  | Qualified (id, (Ml | Runtime),  Some name) ->
    let cxt = Ext_pp_scope.ident cxt f id in
    P.string f L.dot;
    P.string f (Ext_ident.convert  name);
    cxt
  | Qualified (id, External _, Some name) ->
    let cxt = Ext_pp_scope.ident cxt f id in
    Js_dump_property.property_access f name ;
    cxt


and expression l cxt  f (exp : J.expression) : cxt =
  pp_comment_option f exp.comment ;
  expression_desc cxt l f exp.expression_desc

and expression_desc cxt (level:int) f x : cxt  =
  match x with
  | Null ->
    P.string f L.null; cxt 
  | Undefined -> 
    P.string f L.undefined; cxt 
  | Var v ->
    vident cxt f v
  | Bool b ->
    bool f b ; cxt
  | Seq (e1, e2) ->
    P.cond_paren_group f (level > 0) 1 (fun () ->
      let cxt = expression 0 cxt f e1 in
      comma_sp f;
      expression 0 cxt f e2 )
  | Fun (method_, l, b, env) ->  (* TODO: dump for comments *)
    pp_function method_ cxt f false  l b env
  (* TODO:
     when [e] is [Js_raw_code] with arity
     print it in a more precise way
     It seems the optimizer already did work to make sure
     {[
       Call (Raw_js_code (s, Exp i), el, {Full})
       when Ext_list.length_equal el i
     ]}
  *)

  | Call (e, el, info) ->
    P.cond_paren_group f (level > 15) 1  (fun _ -> 
      P.group f 1 (fun _ ->
          match info, el  with
          | {arity  = Full }, _
          | _, [] ->
            let cxt = expression 15 cxt f e in
            P.paren_group f 1 (fun _ -> arguments cxt  f el )

          | _ , _ ->
            let len = List.length el in
            if 1 <= len && len <= 8 then
              begin
                Curry_gen.pp_app f len ; 
                P.paren_group f 1 (fun _ -> arguments cxt f (e::el))
              end
            else
              begin
                Curry_gen.pp_app_any f ;
                P.paren_group f 1
                  (fun _ -> arguments cxt f [ e ; E.array Mutable el])
              end))
  | FlatCall(e,el) ->
    P.group f 1 (fun _ ->
        let cxt = expression 15 cxt f e in
        P.string f L.dot;
        P.string f L.apply;
        P.paren_group f 1 (fun _ ->
            P.string f L.null;
            comma_sp f ;
            expression 1 cxt f el
          )
      )
  | Char_to_int e ->
    (match e.expression_desc with
     | String_index (a,b) ->
       P.group f 1 (fun _ ->
           let cxt = expression 15 cxt f a in
           P.string f L.dot;
           P.string f L.char_code_at;
           P.paren_group f 1 (fun _ -> expression 0 cxt f b);
         )
     | _ ->
       P.group f 1 (fun _ ->
           let cxt = expression 15 cxt f e in
           P.string f L.dot;
           P.string f L.char_code_at;
           P.string f "(0)";
           cxt))
  | Char_of_int e ->
    P.group f 1 (fun _ ->
        P.string f L.string_cap;
        P.string f L.dot;
        P.string f L.fromCharcode;
        P.paren_group f 1 (fun _ -> arguments cxt f [e])
      )  
  | Unicode s ->
    P.string f "\"";
    P.string f s ;
    P.string f "\"";
    cxt
  | Str (_, s) ->
    (*TODO --
       when utf8-> it will not escape '\\' which is definitely not we want
    *)
    Js_dump_string.pp_string f  s;
    cxt
  | Raw_js_function (s,params) ->   
    P.string f L.function_; 
    P.space f ; 
    P.paren_group f 1 (fun _ ->
        comma_strings f params
      );
    P.brace f (fun _ -> P.string f s);
    cxt 
  | Raw_js_code (s,info) ->
    (match info with
     | Exp ->
       P.string f L.lparen;
       P.string f s ;
       P.string f L.rparen;
       cxt
     | Stmt ->
       P.newline f  ;
       P.string f s ;
       P.newline f ;
       cxt)
  
  | Number v ->
    let s =
      match v with
      | Float {f = v} ->
        Js_number.caml_float_literal_to_js_string v
      (* attach string here for float constant folding?*)
      | Int { i = v; _}
        -> Int32.to_string v (* check , js convention with ocaml lexical convention *)
      | Uint i
        -> Format.asprintf "%lu" i
      | Nint i -> Nativeint.to_string i in
    let need_paren =
      if s.[0] = '-'
      then level > 13  (* Negative numbers may need to be parenthesized. *)
      else level = 15  (* Parenthesize as well when followed by a dot. *)
           && s.[0] <> 'I' (* Infinity *)
           && s.[0] <> 'N' (* NaN *) in
    let action = fun _ -> P.string f s  in
    (
      if need_paren
      then P.paren f  action
      else action ()
    );
    cxt
  | Is_null_or_undefined e ->
    P.cond_paren_group f (level > 0) 1 (fun _ ->
        let cxt = expression 1 cxt f e in
        P.space f ;
        P.string f "==";
        P.space f ;
        P.string f L.null;
        cxt)  
  | Js_not e ->
    P.cond_paren_group f (level > 13) 1 (fun _ -> 
      P.string f "!" ;
      expression 13 cxt f e
    )
  | Typeof e
    ->
    P.string f "typeof";
    P.space f;
    expression 13 cxt f e
  | Bin (Eq, {expression_desc = Var i },
         {expression_desc =
            (
              Bin(
                (Plus as op), {expression_desc = Var j}, delta)
            | Bin(
                (Plus as op), delta, {expression_desc = Var j})
            | Bin(
                (Minus as op), {expression_desc = Var j}, delta)
            )
         })
    when Js_op_util.same_vident i j ->
    (* TODO: parenthesize when necessary *)
    begin match delta, op with
      | {expression_desc = Number (Int { i =  1l; _})}, Plus
      (* TODO: float 1. instead,
           since in JS, ++ is a float operation
      *)
      | {expression_desc = Number (Int { i =  -1l; _})}, Minus
        ->
        P.string f L.plusplus;
        P.space f ;
        vident cxt f i

      | {expression_desc = Number (Int { i =  -1l; _})}, Plus
      | {expression_desc = Number (Int { i =  1l; _})}, Minus
        ->
        P.string f L.minusminus;
        P.space f ;
        vident cxt f i;
      | _, _ ->
        let cxt = vident cxt f i in
        P.space f ;
        if op = Plus then P.string f "+="
        else P.string f "-=";
        P.space f ;
        expression 13 cxt  f delta
    end
  | Bin (Eq, {expression_desc = Array_index({expression_desc = Var i; _},
                                       {expression_desc = Number (Int {i = k0 })}
                                      ) },
         {expression_desc =
            (Bin((Plus as op),
                 {expression_desc = Array_index(
                      {expression_desc = Var j; _},
                      {expression_desc = Number (Int {i = k1; })}
                    ); _}, delta)
            | Bin((Plus as op), delta,
                  {expression_desc = Array_index(
                       {expression_desc = Var j; _},
                       {expression_desc = Number (Int {i = k1; })}
                     ); _})
            | Bin((Minus as op),
                  {expression_desc = Array_index(
                       {expression_desc = Var j; _},
                       {expression_desc = Number (Int {i = k1; })}
                     ); _}, delta)

            )})
    when  k0 = k1 && Js_op_util.same_vident i j
    (* Note that
       {[x = x + 1]}
       is exactly the same  (side effect, and return value)
       as {[ ++ x]}
       same to
       {[ x = x + a]}
       {[ x += a ]}
       they both return the modified value too
    *)
    (* TODO:
       handle parens..
    *)
    ->
    let aux cxt f vid i =
      let cxt = vident cxt f vid in
      P.string f "[";
      P.string f (Int32.to_string  i);
      P.string f"]";
      cxt in
    (** TODO: parenthesize when necessary *)
    (match delta, op with
     | {expression_desc = Number (Int { i =  1l; _})}, Plus
     | {expression_desc = Number (Int { i =  -1l; _})}, Minus
       ->
       P.string f L.plusplus;
       P.space f ;
       aux cxt f i k0
     | {expression_desc = Number (Int { i =  -1l; _})}, Plus
     | {expression_desc = Number (Int { i =  1l; _})}, Minus
       ->
       P.string f L.minusminus;
       P.space f ;
       aux cxt f  i k0
     | _, _ ->
       let cxt = aux cxt f i k0 in
       P.space f ;
       if op = Plus then P.string f "+="
       else P.string f "-=";
       P.space f ;
       expression 13 cxt  f delta)
  | Bin (Minus, {expression_desc = Number (Int {i=0l;_} | Float {f = "0."})}, e)
    (* TODO:
       Handle multiple cases like
       {[ 0. - x ]}
       {[ 0.00 - x ]}
       {[ 0.000 - x ]}
    *)
    ->
    P.cond_paren_group f (level > 13 ) 1 (fun _ -> 
      P.string f "-" ;
      expression 13 cxt f e
    )
  | Bin (op, e1, e2) ->
    let (out, lft, rght) = Js_op_util.op_prec op in
    let need_paren =
      level > out || (match op with Lsl | Lsr | Asr -> true | _ -> false) in
    (* We are more conservative here, to make the generated code more readable
          to the user *)
    P.cond_paren_group f need_paren 1  (fun _ -> 
      let cxt = expression lft cxt  f e1 in
      P.space f;
      P.string f (Js_op_util.op_str op);
      P.space f;
      expression rght cxt   f e2)
  | String_append (e1, e2) ->
    let op : Js_op.binop = Plus in
    let (out, lft, rght) = Js_op_util.op_prec op in
    let need_paren =
      level > out || (match op with Lsl | Lsr | Asr -> true | _ -> false) in
    P.cond_paren_group f need_paren 1 (fun _ -> 
      let cxt = expression  lft cxt f e1 in
      P.space f ;
      P.string f "+";
      P.space f;
      expression rght  cxt   f e2)
  | Array (el,_) ->
    (** TODO: simplify for singleton list *)
      (match el with
      | []| [ _ ] -> P.bracket_group f 1 @@ fun _ -> array_element_list  cxt f el
      | _ -> P.bracket_vgroup f 1 @@ fun _ -> array_element_list  cxt f el)
  | Optional_block (e,identity) -> 
    expression level cxt f  
      (if identity then e 
       else       
         E.runtime_call Js_runtime_modules.js_primitive "some" [e])
  | Caml_block( el, mutable_flag, tag, tag_info)
    ->
    (* Note that, if we ignore more than tag [0] we loose some information
       with regard tag  *)

      (* TODO: for numbers like 248, 255 we can reverse engineer to make it
         [Obj.xx_flag], but we can not do this in runtime libraries
      *)

      if Js_fold_basic.needBlockRuntime tag tag_info then begin 
        match tag_info with 
        | Blk_record labels ->
          P.string f L.caml_block;
          P.string f L.dot ;
          P.string f L.block_record;
          P.paren_group f 1 
          (fun _ -> arguments cxt f 
            [E.array Immutable
             (Ext_array.to_list_f E.str labels);
              E.array mutable_flag 
              (List.map (fun (x : J.expression)   -> {x with comment = None}) el) ]
          )
        | Blk_module (Some labels) ->         
          P.string f L.caml_block;
          P.string f L.dot ;
          P.string f L.block_local_module;
          P.paren_group f 1 
          (fun _ -> arguments cxt f 
            [E.array Immutable
             (Ext_list.map labels E.str);
              E.array mutable_flag
              (List.map (fun (x :J.expression) -> {x with comment = None}) el)
            ]
          )
         | Blk_variant name ->  
          P.string f L.caml_block;
          P.string f L.dot ;
          P.string f L.block_poly_var;
          P.paren_group f 1 
          (fun _ -> arguments cxt f 
            [ 
              E.str name;
              E.array mutable_flag el]
          )        
         | Blk_constructor(name,number) ->
           let no_tag_attached = 
             number = 1 && Js_fold_basic.tag_is_zero tag in 
           if !Js_config.debug then 
             (
               P.string f L.caml_block;
               P.string f L.dot ;
               if no_tag_attached then 
                 begin 
                   P.string f L.block_simple_variant;
                   P.paren_group f 1 
                     (fun _ -> arguments cxt f 
                         [E.str name; E.array mutable_flag el]) 
                 end
               else 
                 begin 
                   P.string f L.block_variant;
                   P.paren_group f 1 (fun _ -> arguments cxt f 
                                         [ E.str name; tag ; E.array mutable_flag el]
                                     )
                 end
             )
           else 
             (if no_tag_attached then 
                expression_desc cxt level f (Array (el, mutable_flag))
              else  
                begin 
                  P.string f L.caml_block;
                  P.string f L.dot ;
                  P.string f L.caml_block_create;
                  P.paren_group f 1
                    (fun _ -> arguments cxt f [tag; E.array mutable_flag el])
                end
             )
         |  _  ->
           begin 
             P.string f L.caml_block;
             P.string f L.dot ;
             P.string f L.caml_block_create;
             P.paren_group f 1
               (fun _ -> arguments cxt f [tag; E.array mutable_flag el])
           end 
      end 
      else     
        expression_desc cxt level f  (Array (el, mutable_flag))

  | Caml_block_tag e ->
    P.group f 1 (fun _ ->
        let cxt = expression 15 cxt f  e in
        P.string f L.dot ;
        P.string f L.tag ;
        cxt)
  | Array_index (e, p)

  | String_index (e,p)
    ->
    P.cond_paren_group f (level > 15) 1 (fun _ -> 
      P.group f 1 @@ fun _ ->
      let cxt = expression 15 cxt f e in
      P.bracket_group f 1 @@ fun _ ->
      expression 0 cxt f p )
  | Length (e, _) ->
    (** Todo: check parens *)
    P.cond_paren_group f (level > 15) 1 (fun _ -> 
      let cxt = expression 15 cxt f e in
      P.string f L.dot;
      P.string f L.length;
      cxt)
  | Dot (e, s,normal) ->
    P.cond_paren_group f (level > 15) 1 (fun _ -> 
      let cxt = expression 15 cxt f e in
      Js_dump_property.property_access f s ;
      (* See [ .obj_of_exports]
         maybe in the ast level we should have
         refer and export
      *)
      cxt) 
  | New (e,  el) ->
    P.cond_paren_group f (level > 15) 1 (fun _ ->
      P.group f 1 @@ fun _ ->
      P.string f L.new_;
      P.space f;
      let cxt = expression 16 cxt f e in
      P.paren_group f 1 @@ fun _ ->
      match el with
      | Some el  -> arguments cxt f el
      | None -> cxt)
  | Cond (e, e1, e2) ->
    let action () =
      (* P.group f 1 @@ fun _ ->  *)
      let cxt =  expression 3 cxt f e in
      P.space f;
      P.string f L.question;
      P.space f;
      (*
            [level 1] is correct, however
            to make nice indentation , force nested conditional to be parenthesized
          *)
      let cxt = P.group f 1 (fun _ -> expression 3 cxt f e1) in
      (* let cxt = (P.group f 1 @@ fun _ -> expression 1 cxt f e1) in *)
      P.space f;
      P.string f L.colon;
      P.space f ;

      (* idem *)
      P.group f 1 @@ fun _ -> expression 3 cxt f e2
      (* P.group f 1 @@ fun _ -> expression 1 cxt f e2 *)
    in
    if level > 2 then P.paren_vgroup f 1 action else action ()

  | Object lst ->
    match lst with
    | [] -> P.string f "{ }" ; cxt
    | _ ->
      let action () =
        P.brace_vgroup f 1 @@ fun _ ->
        property_name_and_value_list cxt f lst in
      if level > 1 then
        (* #1946 object literal is easy to be
           interpreted as block statement
           here we avoid parens in such case
           {[
             var f = { x : 2 , y : 2}
           ]}
        *)
        P.paren_group f 1 action
      else action ()

and property_name_and_value_list cxt f l =     
  iter_lst cxt f l (fun cxt f (pn,e) -> 
    Js_dump_property.property_key f pn ;
    P.string f L.colon;
    P.space f;
    expression 1 cxt f e
  ) comma_nl      
and array_element_list cxt f el : cxt =
  iter_lst cxt f el (fun cxt f e  -> expression 1 cxt f e ) comma_nl
 
and arguments cxt f l : cxt =
  iter_lst cxt f l (fun cxt f e  -> expression 1 cxt f e) comma_sp
  
and variable_declaration top cxt f
    (variable : J.variable_declaration) : cxt =
  (* TODO: print [const/var] for different backends  *)
  match variable with
  | {ident = i; value =  None; ident_info ; _} ->
    if ident_info.used_stats = Dead_pure then cxt
    else pp_var_declare cxt f i 
  | { ident = name; value =  Some e; ident_info = {used_stats; _}} ->
    match used_stats with
    | Dead_pure ->
      cxt
    | Dead_non_pure ->
      (* Make sure parens are added correctly *)
      statement_desc top cxt f (J.Exp e)
    | _ ->
      match e, top  with
      | {expression_desc = Fun (method_, params, b, env ); comment = _}, _ ->
        pp_function method_ cxt f
          ~name:(if top then Name_top name else Name_non_top name)
          false params b env
      | _, _ ->
        let cxt = pp_var_assign cxt f name in 
        let cxt = expression 1 cxt f e in
        semi f;
        cxt

    
and ipp_comment : 'a . P.t -> 'a  -> unit = fun   f comment ->
  ()


(** don't print a new line -- ASI
    FIXME: this still does not work in some cases...
    {[
      return /* ... */
      [... ]
    ]}
*)

and pp_comment f comment =
  if String.length comment > 0 then
    begin
      P.string f "/* "; P.string f comment ; P.string f " */"
    end

and pp_comment_option f comment  =
  match comment with
  | None -> ()
  | Some x -> pp_comment f x
and statement top cxt f
    ({statement_desc = s;  comment ; _} : J.statement)  : cxt =

  pp_comment_option f comment ;
  statement_desc top cxt f s

and statement_desc top cxt f (s : J.statement_desc) : cxt =
  match s with
  | Block [] ->
    ipp_comment f  L.empty_block; (* debugging*)
    cxt
  | Exp {expression_desc = Var _;}
    -> (* Does it make sense to optimize here? *)
    semi f; cxt
  | Exp e ->
    let cxt =
      (
        if exp_need_paren  e
        then P.paren_group f 1
        else P.group f 0
      ) (fun _ -> expression 0 cxt f e ) in
    semi f;
    cxt
  | Block b -> (* No braces needed here *)
    ipp_comment f L.start_block;
    let cxt = statement_list top cxt  f b in
    ipp_comment f  L.end_block;
    cxt
  | Variable l ->
    variable_declaration top cxt  f l

  | If (e, s1,  s2) -> (* TODO: always brace those statements *)
    P.string f L.if_;
    P.space f;
    let cxt = P.paren_group f 1 (fun _ -> expression 0 cxt f e) in
    P.space f;
    let cxt = block cxt f s1 in
    (match s2 with
     | None | (Some [])
     | Some [{statement_desc = (Block [] | Exp {expression_desc = Var _;} ); }]
       -> P.newline f; cxt
     | Some [{statement_desc = If _} as nest]
     | Some [{statement_desc = Block [ {statement_desc = If _ ; _} as nest] ; _}]
       ->
       P.space f;
       P.string f L.else_;
       P.space f;
       statement false cxt f nest
     | Some s2 ->
       P.space f;
       P.string f L.else_;
       P.space f ;
       block  cxt f s2)
  | While (label, e, s, _env) ->  (*  FIXME: print scope as well *)
    begin
      (match label with
       | Some i ->
         P.string f i ;
         P.string f L.colon;
         P.newline f ;
       | None -> ());
      let cxt =
        match e.expression_desc with
        | Number (Int {i = 1l}) ->
          P.string f L.while_;
          P.string f L.lparen;
          P.string f L.true_;
          P.string f L.rparen;
          P.space f ;
          cxt
        | _ ->
          P.string f L.while_;
          let cxt = P.paren_group f 1 (fun _ ->  expression 0 cxt f e) in
          P.space f ;
          cxt
      in
      let cxt = block cxt f s in
      semi f;
      cxt
    end
  | ForRange (for_ident_expression, finish, id, direction, s, env) ->
    let action cxt  =
      P.vgroup f 0 @@ fun _ ->
      let cxt = P.group f 0 @@ fun _ ->
        (* The only place that [semi] may have semantics here *)
        P.string f L.for_ ;
        P.paren_group f 1 @@ fun _ ->
        let cxt, new_id =
          match for_ident_expression, finish.expression_desc with
          | Some ident_expression , (Number _ | Var _ ) ->
            let cxt = pp_var_assign cxt f id in  
            expression 0 cxt f ident_expression, None
          | Some ident_expression, _ ->
            let cxt = pp_var_assign cxt f id in 
            let cxt = expression 1 cxt f ident_expression in
            P.space f ;
            comma f;  
            let id = Ext_ident.create (Ident.name id ^ "_finish") in
            let cxt = Ext_pp_scope.ident cxt f id in
            P.space f ;
            P.string f L.eq;
            P.space f;
            expression 1 cxt f finish, Some id
          | None, (Number _ | Var _) ->
            cxt, None
          | None , _ ->
            let id = Ext_ident.create (Ident.name id ^ "_finish") in
            let cxt = pp_var_assign cxt f id in 
            expression 15 cxt f finish, Some id in
        semi f ;
        P.space f;
        let cxt = Ext_pp_scope.ident cxt f id in
        P.space f;
        let right_prec  =
          match direction with
          | Upto ->
            let (_,_,right) = Js_op_util.op_prec Le  in
            P.string f L.le;
            right
          | Downto ->
            let (_,_,right) = Js_op_util.op_prec Ge in
            P.string f L.ge ;
            right
        in
        P.space f ;
        let cxt  =
          expression   right_prec cxt  f 
            (match new_id with
             | Some i -> E.var i
             | None -> finish) in
        semi f;
        P.space f;
        pp_direction f direction;
        Ext_pp_scope.ident cxt f id in
      block  cxt f s  in
    let lexical = Js_closure.get_lexical_scope env in
    if Ident_set.is_empty lexical
    then action cxt
    else
      (* unlike function,
         [print for loop] has side effect,
         we should take it out
      *)
      let inner_cxt = Ext_pp_scope.merge lexical cxt in
      let lexical = Ident_set.elements lexical in
      P.vgroup f 0
        (fun _ ->
           P.string f L.lparen;
           P.string f L.function_;               
           pp_paren_params inner_cxt f lexical; 
           let cxt = P.brace_vgroup f 0  (fun _ -> action inner_cxt) in               
           pp_paren_params inner_cxt f lexical; 
           P.string f L.rparen;
           semi f;
           cxt
        )
  | Continue s -> continue f s ; cxt
    (* P.newline f;  #2642 *)    
  | Debugger ->  debugger_nl f ; cxt
  | Break -> break_nl f; cxt

  | Return {return_value = e} ->
    begin match e with
      | {expression_desc = Fun (method_,  l, b, env); _} ->
        let cxt =
          pp_function method_ cxt f true l b env in
        semi f ; cxt
      | e ->
        return_sp f ;
        (* P.string f "return ";(\* ASI -- when there is a comment*\) *)
        P.group f return_indent (fun _ ->
            let cxt =  expression 0 cxt f e in
            semi f;
            cxt)
        (* There MUST be a space between the return and its
           argument. A line return will not work *)
    end
  | Int_switch (e, cc, def) ->
    P.string f L.switch;
    P.space f;
    let cxt = P.paren_group f 1 (fun _ ->  expression 0 cxt f e) in
    P.space f;
    P.brace_vgroup f 1 @@ fun _ ->
    let cxt = loop_case_clauses cxt f (fun f i -> P.string f (string_of_int i) ) cc in
    (match def with
     | None -> cxt
     | Some def ->
       P.group f 1 @@ fun _ ->
       P.string f L.default;
       P.string f L.colon;
       P.newline f;
       statement_list  false cxt  f def
    )

  | String_switch (e, cc, def) ->
    P.string f L.switch;
    P.space f;
    let cxt = P.paren_group f 1 @@ fun _ ->  expression 0 cxt f e in
    P.space f;
    P.brace_vgroup f 1 (fun _ ->
        let cxt = loop_case_clauses cxt f (fun f i -> Js_dump_string.pp_string f i ) cc in
        match def with
        | None -> cxt
        | Some def ->
          P.group f 1 (fun _ ->
          P.string f L.default;
          P.string f L.colon;
          P.newline f;
          statement_list  false cxt  f def ))
  | Throw e ->
    P.string f L.throw;
    P.space f ;
    P.group f throw_indent  (fun _ ->
        let cxt = expression 0 cxt f e in
        semi f ; cxt)

  (* There must be a space between the return and its
     argument. A line return would not work *)
  | Try (b, ctch, fin) ->
    P.vgroup f 0 @@ fun _->
    P.string f L.try_;
    P.space f ;
    let cxt = block cxt f b in
    let cxt =
      match ctch with
      | None ->
        cxt
      | Some (i, b) ->
        P.newline f;
        P.string f "catch (";
        let cxt = Ext_pp_scope.ident cxt f i in
        P.string f ")";
        block cxt f b in
    match fin with
    | None -> cxt
    | Some b ->
      P.group f 1 (fun _ ->
          P.string f L.finally;
          P.space f;
          block cxt f b)

and function_body cxt f b =
  match b with
  | []     -> cxt
  | [s]    ->
    begin match s.statement_desc with
    | If (bool,
          then_,
          Some [{
              statement_desc =
                Return {return_value = {expression_desc = Undefined}} }])
        ->
        statement false cxt f {s with statement_desc = If(bool,then_,None)}
    | _ ->        
      statement false  cxt f  s
    end
  | s :: r ->
    let cxt = statement false cxt f s in
    P.newline f;
    function_body cxt f  r

(* similar to [block] but no braces *)
and statement_list top cxt f  b =
  iter_lst cxt f b  (fun cxt f s -> statement top cxt f s )
    (if top then 
      (fun f -> P.newline f ; P.force_newline f )
      else P.newline
    )
    (* (fun f -> P.newline f ; if top then P.force_newline f ) *)

and block cxt f b =
  (* This one is for '{' *)
  P.brace_vgroup f 1 (fun _ -> statement_list false cxt   f b )




(* let program f cxt   ( x : J.program ) =
  let () = P.force_newline f in
  let cxt =  statement_list true cxt f x.block  in
  let () = P.force_newline f in
  Js_dump_import_export.exports cxt f x.exports *)

(* let dump_program (x : J.program) oc =
  ignore (program (P.from_channel oc)  Ext_pp_scope.empty  x ) *)

let string_of_block  block
  =
  let buffer  = Buffer.create 50 in
  let f = P.from_buffer buffer in
  let _scope =  statement_list true Ext_pp_scope.empty  f block in
  P.flush  f ();
  Buffer.contents buffer



let string_of_expression e =
  let buffer  = Buffer.create 50 in
  let f = P.from_buffer buffer in
  let _scope =  expression 0  Ext_pp_scope.empty  f e in
  P.flush  f ();
  Buffer.contents buffer

