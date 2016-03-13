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

(* module P = Ext_format *)
module P = Ext_pp
module E = Js_exp_make 
module S = Js_stmt_make 

module L = struct
  let function_ = "function"
  let var = "var" (* should be able to switch to [let] easily*)
  let return = "return"
  let eq = "="
  let require = "require"
  let goog_require = "goog.require" 
  let goog_module = "goog.module"
  let lparen = "("
  let rparen = ")"
  let exports = "exports"
  let dot = "."
  let comma = ","
  let colon = ":"
  let throw = "throw"
  let default = "default"
  let length = "length"
  let char_code_at = "charCodeAt"
  let new_ = "new"
  let array = "Array"
  let question = "?"      
  let plusplus = "++"
  let minusminus = "--"
  let semi = ";"
  let else_ = "else"
  let if_ = "if"
  let while_ = "while"
  let empty_block = "empty_block"
  let start_block = "start_block"
  let end_block = "end_block"
  let json = "JSON"
  let stringify = "stringify"
  let console = "console"
  let define = "define"
  let break = "break"
  let strict_directive = "'use strict';"
  let true_ = "true"
  let false_ = "false"
  let curry = "app" (* curry arbitrary args *)
  let tag = "tag"
  let bind = "bind"
  let math = "Math"
end
let return_indent = (String.length L.return / Ext_pp.indent_length) 

let throw_indent = (String.length L.throw / Ext_pp.indent_length) 


let semi f = P.string f L.semi

let op_prec, op_str  =
  Js_op_util.(op_prec, op_str)

let best_string_quote s =
  let simple = ref 0 in
  let double = ref 0 in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '\'' -> incr simple
    | '"' -> incr double
    | _ -> ()
  done;
  if !simple < !double
  then '\''
  else '"'


(**
   same as {!Js_dump.ident} except it generates a string instead of doing the printing
*)
let str_of_ident (cxt : Ext_pp_scope.t) (id : Ident.t)   =
  if Ext_ident.is_js id then (* reserved by compiler *)
     ( id.name , cxt) 
  else 
    (* For fast/debug mode, we can generate the name as 
       [Printf.sprintf "%s$%d" name id.stamp] which is 
       not relevant to the context       
    *)    
    let name = Ext_ident.convert id.name in
    let i,new_cxt = Ext_pp_scope.add_ident  id cxt in
    (* Attention: 
       $$Array.length, due to the fact that global module is 
       always printed in the begining(via imports), so you get a gurantee, 
       (global modules will not be printed as [List$1]) 

       However, this means we loose the ability of dynamic loading, is it a big 
       deal? we can fix this by a scanning first, since we already know which 
       modules are global

       check [test/test_global_print.ml] for regression

    *)
    (if i == 0 then 
       name 
     else
       Printf.sprintf"%s$%d" name i), new_cxt 


let ident (cxt : Ext_pp_scope.t) f (id : Ident.t) : Ext_pp_scope.t  =
  let str, cxt = str_of_ident cxt id in
  P.string f str; 
  cxt   

(** Avoid to allocate single char string too many times*)
let array_str1 =
  Array.init 256 (fun i -> String.make 1 (Char.chr i)) 

(** For conveting 
 
*)
let array_conv =
  [|"0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "a"; "b"; "c"; "d";
    "e"; "f"|]



(* https://mathiasbynens.be/notes/javascript-escapes *)
let pp_string f ?(quote='"') ?(utf=false) s =
  let pp_raw_string f ?(utf=false) s = 
    let l = String.length s in
    for i = 0 to l - 1 do
      let c = String.unsafe_get s i in
      match c with
      | '\b' -> P.string f "\\b"
      | '\012' -> P.string f "\\f"
      | '\n' -> P.string f "\\n"
      | '\r' -> P.string f "\\r"
      | '\t' -> P.string f "\\t"
      (* This escape sequence is not supported by IE < 9
               | '\011' -> "\\v"
         IE < 9 treats '\v' as 'v' instead of a vertical tab ('\x0B'). 
         If cross-browser compatibility is a concern, use \x0B instead of \v.

         Another thing to note is that the \v and \0 escapes are not allowed in JSON strings.
      *)
      | '\000' when i = l - 1 || (let next = String.unsafe_get s (i + 1) in (next < '0' || next > '9'))
        -> P.string f "\\0"

      | '\\' when not utf -> P.string f "\\\\"


      | '\000' .. '\031'  | '\127'->
        let c = Char.code c in
        P.string f "\\x";
        P.string f (Array.unsafe_get array_conv (c lsr 4));
        P.string f (Array.unsafe_get array_conv (c land 0xf))
      | '\128' .. '\255' when not utf ->
        let c = Char.code c in
        P.string f "\\x";
        P.string f (Array.unsafe_get array_conv (c lsr 4));
        P.string f (Array.unsafe_get array_conv (c land 0xf))
      (* | '\'' -> P.string f "\\'" *)
      (* | '\"' -> P.string f "\\\"" *)
      | _ ->
        begin 
          (if c = quote  then
             P.string f "\\");           
          P.string f (Array.unsafe_get array_str1 (Char.code c))
        end
    done
  in
  let quote_s = String.make 1 quote in
  P.string f quote_s;
  pp_raw_string f ~utf s ;
  P.string f quote_s
;;


(* TODO: check utf's correct semantics *)
let pp_quote_string f s = 
  pp_string f ~utf:false ~quote:(best_string_quote s ) s 

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

let rec pp_function 
    cxt (f : P.t) ?name  return 
    (l : Ident.t list) (b : J.block) (env : Js_fun_env.t ) =  
  let ipp_ident cxt f id un_used = 
    if un_used then 
      ident cxt f (Ext_ident.make_unused ())
    else 
      ident cxt f id  in
  let rec formal_parameter_list cxt (f : P.t) l =
    let rec aux i cxt l = 
      match l with
      | []     -> cxt
      | [id]    -> ipp_ident cxt f id (Js_fun_env.get_unused env i)
      | id :: r -> 
        let cxt = ipp_ident cxt f id (Js_fun_env.get_unused env i) in
        P.string f L.comma; P.space f;
        aux (i + 1) cxt  r
    in
    match l with 
    | [] -> cxt 
    | [i] -> 
      (** necessary, since some js libraries like [mocha]...*)
      if Js_fun_env.get_unused env 0 then cxt else ident cxt f i 
    | _ -> 
      aux 0 cxt l  in

  let rec aux  cxt f ls  =
    match ls with
    | [] -> cxt
    | [x] -> ident cxt f x
    | y :: ys ->
        let cxt = ident cxt f y in
        P.string f L.comma;
        aux cxt f ys  in

  let set_env = (** identifiers will be printed following*)
    match name with 
    | None ->
        Js_fun_env.get_bound env 
    | Some id -> Ident_set.add id (Js_fun_env.get_bound env )
  in
  (* the context will be continued after this function *)
  let outer_cxt = Ext_pp_scope.merge set_env cxt in  

  (* the context used to be printed inside this function

     when printing a function, 
     only the enclosed variables and function name matters, 
     if the function does not capture any variable, then the context is empty     
  *)
  let inner_cxt = Ext_pp_scope.sub_scope outer_cxt set_env in

  (
    (* (if not @@ Js_fun_env.is_empty env then *)
    (* pp_comment  f (Some (Js_fun_env.to_string env))) ; *)
    let action return = 

      (if return then begin 
          P.string f L.return ;
          P.space f
        end else ());
      P.string f L.function_;
      P.space f ;
      (match name with None  -> () | Some x -> ignore (ident inner_cxt f x));
      let body_cxt = P.paren_group f 1 (fun _ -> 
          formal_parameter_list inner_cxt  f l )
      in
      P.space f ;
      ignore @@ P.brace_vgroup f 1 (fun _ -> statement_list false body_cxt f b );
    in
    let lexical = Js_fun_env.get_lexical_scope env in
    let enclose action lexical  return = 

      if  Ident_set.is_empty lexical  
      then
        action return 
      else
        let lexical = Ident_set.elements lexical in
        (if return then begin 
            P.string f L.return ; 
            P.space f
          end else ());
        P.string f "(";
        P.string f L.function_; 
        P.string f "(";
        ignore @@ aux inner_cxt f lexical;
        P.string f ")";
        P.brace_vgroup f 0  (fun _ -> action true);
        P.string f "(";
        ignore @@ aux inner_cxt f lexical;
        P.string f ")";
        P.string f ")"

    in
    enclose action lexical return 
  ) ;
  outer_cxt


(* Assume the cond would not change the context, 
    since it can be either [int] or [string]
 *)
and output_one : 'a . 
    _ -> P.t -> (P.t -> 'a -> unit) -> 'a J.case_clause -> _
    = fun cxt f  pp_cond
    ({case = e; body = (sl,break)} : _ J.case_clause) -> 
  let cxt = 
    P.group f 1 @@ fun _ -> 
      P.group f 1 @@ (fun _ -> 
        P.string f "case ";
        pp_cond  f e;
        P.space f ;
        P.string f L.colon  );
      
      P.space f;
      P.group f 1 @@ fun _ ->
        let cxt =
          match sl with 
          | [] -> cxt 
          | _ ->
              P.newline f ;
              statement_list false cxt  f sl
        in
        (if break then 
          begin
            P.newline f ;
            P.string f L.break;
            semi f;
          end) ;
        cxt
  in
  P.newline f;
  cxt 

and loop  :  'a . Ext_pp_scope.t ->
  P.t -> (P.t -> 'a -> unit) -> 'a J.case_clause list -> Ext_pp_scope.t
      = fun  cxt  f pp_cond cases ->
  match cases with 
  | [] -> cxt 
  | [x] -> output_one cxt f pp_cond x
  | x::xs ->
      let cxt = output_one cxt f pp_cond x 
      in loop  cxt f pp_cond  xs 

and vident cxt f  (v : J.vident) =
  begin match v with 
  | Id v | Qualified(v, _, None) ->  
      ident cxt f v
  | Qualified (id,_, Some name) ->
      let cxt = ident cxt f id in
      P.string f L.dot;
      P.string f (Ext_ident.convert name);
      cxt
  end

and expression l cxt  f (exp : J.expression) : Ext_pp_scope.t = 
  pp_comment_option f exp.comment ;
  expression_desc cxt l f exp.expression_desc

and
  expression_desc cxt (l:int) f x : Ext_pp_scope.t  =
  match x with
  | Var v ->
    vident cxt f v 
  | Bool b -> 
    (if  b then P.string f L.true_ else P.string f L.false_ ) ; cxt 
  | Seq (e1, e2) ->
    let action () = 
      let cxt = expression 0 cxt f e1 in
      P.string f L.comma ;
      P.space f ;
      expression 0 cxt f e2  in
    if l > 0 then 
      P.paren_group f 1 action
    else action ()

  | Fun (l, b, env) ->  (* TODO: dump for comments *)
    pp_function cxt f false  l b env

  | Call (e, el, info) ->
    let action () = 
      P.group f 1 (fun _ -> 
          match info, el  with
          | {arity  = Full }, _ 
          | _, [] -> 
            let cxt = expression 15 cxt f e in 
            P.paren_group f 1 (fun _ -> arguments cxt  f el )  

          | _ , _ -> 
            (* ipp_comment f (Some "!") *)
            P.string f  Js_config.curry; 
            P.string f L.dot;
            let len = List.length el in
            if 1 <= len && len <= 8 then  
              begin
                P.string f (Printf.sprintf "app%d" len);
                P.paren_group f 1 (fun _ -> arguments cxt f (e::el))
              end
            else 
              begin 
                P.string f  L.curry;                
                P.paren_group f 1 (fun _ -> arguments cxt f [ e ; E.arr Mutable el])
              end)
    in
    if l > 15 then P.paren_group f 1 action   
    else action ()
  | Bind (a,b) -> 
    begin
      expression_desc cxt l f  
        (Call ({expression_desc = Dot(a,L.bind, true); comment = None }, [b], {arity = Full}))
    end    
  (* | Tag_ml_obj e ->  *)
  (*   P.group f 1 (fun _ ->  *)
  (*       P.string f "Object.defineProperty"; *)
  (*       P.paren_group f 1 (fun _ -> *)
  (*           let cxt = expression 1 cxt f e in *)
  (*           P.string f L.comma; *)
  (*           P.space f ;  *)
  (*           P.string f {|"##ml"|}; *)
  (*           P.string f L.comma; *)
  (*           P.string f {|{"value" : true, "writable" : false}|} ;  *)
  (*           cxt )) *)

  | FlatCall(e,el) -> 
    P.group f 1 (fun _ -> 
        let cxt = expression 15 cxt f e in
        P.string f ".apply";
        P.paren_group f 1 (fun _ ->
            P.string f "null";
            P.string f L.comma;
            P.space f ; 
            expression 1 cxt f el
          )
      )
  | String_of_small_int_array (e) -> 
    let action () = 
      P.group f 1 (fun _ -> 
          P.string f "String.fromCharCode.apply";
          P.paren_group f 1 (fun _ -> 
              P.string f "null";
              P.string f L.comma;
              expression 1 cxt  f e  ) ) 
    in
    if l > 15 then P.paren_group f 1 action   
    else action ()

  | Array_append (e, el) -> 
    P.group f 1 (fun _ -> 
        let cxt = expression 15 cxt f e in
        P.string f ".concat";
        P.paren_group f 1 (fun _ -> arguments cxt f [el]))

  | Array_copy e -> 
    P.group f 1 (fun _ -> 
        let cxt = expression 15 cxt f e in
        P.string f ".slice";
        P.string f "()" ;
        cxt 
      )

  | Dump (level, el) -> 
    let obj = 
      match level with 
      | Log -> "log"
      | Info -> "info"
      | Warn -> "warn"
      | Error -> "error" in
    P.group f 1 (fun _ -> 
        P.string f L.console;
        P.string f L.dot;
        P.string f obj ;
        P.paren_group f 1 (fun _ -> arguments cxt f el))
  | Json_stringify e 
    -> 
    P.group f 1 (fun _ -> 
        P.string f L.json ;
        P.string f L.dot;
        P.string f L.stringify; 
        P.paren_group f 1 (fun _ -> expression 0 cxt f e )        
      )    
  | Char_to_int e -> 
    begin match e.expression_desc with 
      | String_access (a,b) -> 
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
            cxt)
    end

  | Char_of_int e -> 
    P.group f 1 (fun _ -> 
        P.string f "String";
        P.string f L.dot;
        P.string f "fromCharCode";
        P.paren_group f 1 (fun _ -> arguments cxt f [e])
      )


  | Math (name, el) -> 
    P.group f 1 (fun _ ->
        P.string f L.math;
        P.string f L.dot;
        P.string f name;
        P.paren_group f 1 (fun _ -> arguments cxt f el)
      )

  | Str (_, s) ->
    (*TODO --
       when utf8-> it will not escape '\\' which is definitely not we want
     *)
    let quote = best_string_quote s in 
    pp_string f (* ~utf:(kind = `Utf8) *) ~quote s; cxt 
  | Raw_js_code (s,info) -> 
    begin match info with 
    | Exp -> 
      P.string f s ; cxt 
    | Stmt -> 
      P.newline f  ;
      P.string f s ;
      P.newline f ;
      cxt 
    end
  | Number v ->
    let s = 
      match v with 
      | Float {f = v} -> 
        Js_number.caml_float_literal_to_js_string v 
       (* attach string here for float constant folding?*)
      | Int { i = v; _} 
        -> Int32.to_string v (* check , js convention with ocaml lexical convention *)in
    let need_paren =
      if s.[0] = '-'
      then l > 13  (* Negative numbers may need to be parenthesized. *)
      else l = 15  (* Parenthesize as well when followed by a dot. *)
           && s.[0] <> 'I' (* Infinity *)
           && s.[0] <> 'N' (* NaN *)
    in
    let action = fun _ -> P.string f s  in
    (
      if need_paren 
      then P.paren f  action
      else action ()
    ); 
    cxt 

  | Int_of_boolean e -> 
    let action () = 
      P.group f 0 @@ fun _ -> 
        P.string f "+" ;
        expression 13 cxt f e 
    in
    (* need to tweak precedence carefully 
       here [++x --> +(+x)]
    *)
    if l > 12 
    then P.paren_group f 1 action 
    else action ()

  | Not e ->
    let action () = 
      P.string f "!" ;
      expression 13 cxt f e 
    in
    if l > 13 
    then P.paren_group f 1 action 
    else action ()
  | Typeof e 
    -> 
    P.string f "typeof"; 
    P.space f;
    expression 13 cxt f e     
  | Caml_block_set_tag(a,b) -> 
    expression_desc cxt l f 
      (Bin(Eq, 
           {expression_desc = Caml_block_tag a; comment = None},
           b
          ))
  | Caml_block_set_length(a,b) -> 
    expression_desc cxt l f 
      (Bin(Eq, 
           {expression_desc = Length (a,Caml_block); comment = None},
           b
          ))
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
  | Bin (Eq, {expression_desc = Access({expression_desc = Var i; _},
                                       {expression_desc = Number (Int {i = k0 })}
                                      ) },
         {expression_desc = 
            (Bin((Plus as op), 
                 {expression_desc = Access(
                      {expression_desc = Var j; _},
                      {expression_desc = Number (Int {i = k1; })}
                    ); _}, delta)
            | Bin((Plus as op), delta,
                  {expression_desc = Access(
                       {expression_desc = Var j; _},
                       {expression_desc = Number (Int {i = k1; })}
                     ); _})
            | Bin((Minus as op), 
                  {expression_desc = Access(
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

    begin match delta, op with 
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
        expression 13 cxt  f delta
    end
  | Anything_to_string e -> 
    (* Note that we should not apply any smart construtor here, 
       it's purely  a convenice for pretty-printing
    *)    
    expression_desc cxt l f (Bin (Plus, {expression_desc = Str (true,""); comment = None}, e))    

  | Bin (Minus, {expression_desc = Number (Int {i=0l;_} | Float {f = "0."})}, e) 
      (* TODO:
         Handle multiple cases like
         {[ 0. - x ]}
         {[ 0.00 - x ]}
         {[ 0.000 - x ]}
       *)
    ->
    let action () = 
      P.string f "-" ;
      expression 13 cxt f e 
    in
    if l > 13 then P.paren_group f 1 action 
    else action ()

  | Bin (op, e1, e2) ->
    let (out, lft, rght) = op_prec op in
    let need_paren =
      l > out || (match op with Lsl | Lsr | Asr -> true | _ -> false) in

    let action () = 
      (* We are more conservative here, to make the generated code more readable
          to the user
       *)

      let cxt = expression lft cxt  f e1 in
      P.space f; 
      P.string f (op_str op);
      P.space f;
      expression rght cxt   f e2 
    in
    if need_paren 
    then P.paren_group f 1 action 
    else action ()

  | String_append (e1, e2) -> 
    let op : Js_op.binop = Plus in
    let (out, lft, rght) = op_prec op in
    let need_paren =
      l > out || (match op with Lsl | Lsr | Asr -> true | _ -> false) in

    let action () = 
      let cxt = expression  lft cxt f e1 in
      P.space f ;
      P.string f "+";
      P.space f;
      expression rght  cxt   f e2 
    in
    if need_paren then P.paren_group f 1 action else action ()

  | Array (el,_) ->
    (** TODO: simplify for singleton list *)
    begin match el with 
      | []| [ _ ] -> P.bracket_group f 1 @@ fun _ -> array_element_list  cxt f el 
      | _ -> P.bracket_vgroup f 1 @@ fun _ -> array_element_list  cxt f el 
    end
  | Caml_uninitialized_obj (tag, size) 
    -> 
    expression_desc cxt l f (Object [Length, size ; Tag, tag])    
  | Caml_block( el, mutable_flag, tag, tag_info) 
    -> 
    (* Note that, if we ignore more than tag [0] we loose some information 
       with regard tag  *)
    begin match tag.expression_desc, tag_info with 

    | Number (Int { i = 0l ; _})  , 
      (Tuple | Array | Variant _ | Record _ | NA | Module _
      |  Constructor ("Some" | "::")) 
      (* Hack to optimize option which is really pervasive in ocaml, 
         we need concrete benchmark to support this
      *) 
      -> expression_desc cxt l f  (Array (el, mutable_flag))
    (* TODO: for numbers like 248, 255 we can reverse engineer to make it 
       [Obj.xx_flag], but we can not do this in runtime libraries
    *)

    | _, _
      -> 
      expression_desc cxt l f 
        (J.Object (
            let length, rev_list = 
              List.fold_left (fun (i,acc) v -> 
                (i+1, (Js_op.Int_key i, v) :: acc)                
              ) (0, []) el in
            List.rev_append rev_list 
              [(Js_op.Length, E.small_int length) ;  (Js_op.Tag, tag)]
          )
        )
    end
  | Caml_block_tag e ->
    P.group f 1 (fun _ ->  
        let cxt = expression 15 cxt f  e in
        P.string f L.dot ;
        P.string f L.tag ;
        cxt)
  | Access (e, e') 

  | String_access (e,e')
    ->
    let action () = 
      P.group f 1 @@ fun _ -> 
        let cxt = expression 15 cxt f e in
        P.bracket_group f 1 @@ fun _ -> 
          expression 0 cxt f e' 
    in
    if l > 15 then P.paren_group f 1 action else action ()

  | Length (e, _) -> 
    let action () =  (** Todo: check parens *)
      let cxt = expression 15 cxt f e in
      P.string f L.dot;
      P.string f L.length;
      cxt  in
    if l > 15 then P.paren_group f 1 action else action ()

  | Dot (e, nm,normal) ->
    if normal then 
      begin 
        let action () = 
          let cxt = expression 15 cxt f e in
          P.string f L.dot;
          P.string f (Ext_ident.convert nm); 
          (* See [Js_program_loader.obj_of_exports] 
             maybe in the ast level we should have 
             refer and export
          *)
          cxt in
        if l > 15 then P.paren_group f 1 action else action ()
      end
    else begin
      let action () = 
        P.group f 1 @@ fun _ -> 
          let cxt = expression 15 cxt f e in
          (P.bracket_group f 1 @@ fun _ -> 
              pp_string f (* ~utf:(kind = `Utf8) *) ~quote:( best_string_quote nm) nm);
          cxt 
      in
      if l > 15 then P.paren_group f 1 action else action ()
    end

  | New (e,  el) ->
    let action () = 
      P.group f 1 @@ fun _ -> 
        P.string f L.new_;
        P.space f;
        let cxt = expression 16 cxt f e in
        P.paren_group f 1 @@ fun _ -> 
          match el with 
          | Some el  -> arguments cxt f el  
          | None -> cxt
    in
    if l > 15 then P.paren_group f 1 action else action ()

  | Array_of_size e ->
    let action () = 
      P.group f 1 @@ fun _ -> 
        P.string f L.new_;
        P.space f;
        P.string f L.array;
        P.paren_group f 1 @@ fun _ -> expression 0 cxt f e
    in
    if l > 15 then P.paren_group f 1 action else action ()

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
      let cxt = (P.group f 1 @@ fun _ -> expression 3 cxt f e1) in
      (* let cxt = (P.group f 1 @@ fun _ -> expression 1 cxt f e1) in *)
      P.space f;
      P.string f L.colon;
      P.space f ; 

      (* idem *)
      P.group f 1 @@ fun _ -> expression 3 cxt f e2
      (* P.group f 1 @@ fun _ -> expression 1 cxt f e2 *)
    in
    if l > 2 then P.paren_vgroup f 1 action else action ()

  | Object lst ->
    P.brace_vgroup f 1 @@ fun _ -> 
      property_name_and_value_list cxt f lst

and property_name cxt f (s : J.property_name) : unit =
  match s with
  | Tag -> P.string f L.tag
  | Length -> P.string f L.length
  | Key s -> 
    pp_string f ~utf:true ~quote:(best_string_quote s) s
  | Int_key i -> P.string f (string_of_int i)

and property_name_and_value_list cxt f l : Ext_pp_scope.t =
  match l with
  | [] -> cxt
  | [(pn, e)] ->
    property_name cxt  f pn ;
    P.string f L.colon;
    P.space f;
    expression 1 cxt f e 
  | (pn, e) :: r ->
    property_name cxt f pn ; 
    P.string f L.colon;
    P.space f;
    let cxt = expression 1 cxt f e in
    P.string f L.comma;
    P.newline f;
    property_name_and_value_list cxt f r

and array_element_list cxt f el : Ext_pp_scope.t =
  match el with
  | []     -> cxt 
  | [e]    ->  expression 1 cxt f e
  | e :: r ->
    let cxt =  expression 1 cxt f e 
    in
    P.string f L.comma; P.newline f; array_element_list cxt f r

and arguments cxt f l : Ext_pp_scope.t =
  match l with
  | []     -> cxt 
  | [e]    ->   expression 1 cxt f e
  | e :: r -> 
    let cxt =   expression 1 cxt f e in
    P.string f L.comma; P.space f; arguments cxt f r

and variable_declaration top cxt f 
    (variable : J.variable_declaration) : Ext_pp_scope.t = 
  (* TODO: print [const/var] for different backends  *)
  match variable with
  | {ident = i; value =  None; ident_info ; _} -> 
    if ident_info.used_stats = Dead_pure 
    then cxt
    else 
      begin
        P.string f L.var;
        P.space f;
        let cxt = ident cxt  f i in
        semi f ; 
        cxt
      end 
  | { ident = i ; value =  Some e; ident_info = {used_stats; _}} ->
    begin match used_stats with
      | Dead_pure -> 
        cxt 
      | Dead_non_pure -> 
        (* Make sure parens are added correctly *)
        statement_desc top cxt f (J.Exp e)
      | _ -> 
        begin match e, top  with 
          | {expression_desc = Fun (params, b, env ); comment = _}, true -> 
            pp_function cxt f ~name:i false params b env 
          | _, _ -> 
              P.string f L.var;
              P.space f;
              let cxt = ident cxt f i in
              P.space f ;
              P.string f L.eq;
              P.space f ;
              let cxt = expression 1 cxt f e in
              semi f;
              cxt 
        end
    end
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
    P.string f "/* "; P.string f comment ; P.string f " */" 

and pp_comment_option f comment  = 
    match comment with 
    | None -> ()
    | Some x -> pp_comment f x
and statement top cxt f 
    ({statement_desc = s;  comment ; _} : J.statement)  : Ext_pp_scope.t =

  pp_comment_option f comment ;
  statement_desc top cxt f s 

and statement_desc top cxt f (s : J.statement_desc) : Ext_pp_scope.t = 
  match s with
  | Block [] -> 
      ipp_comment f  L.empty_block; (* debugging*)
      cxt

  | Block b -> (* No braces needed here *)
      ipp_comment f L.start_block;
      let cxt = statement_list top cxt  f b in
      ipp_comment f  L.end_block;
      cxt
  | Variable l ->
      variable_declaration top cxt  f l
  | Exp {expression_desc = Var _;}-> (* Does it make sense to optimize here? *)
      semi f; cxt 
  | Exp e ->
    (* Parentheses are required when the expression
       starts syntactically with "{" or "function" 
       TODO:  be more conservative, since Google Closure will handle
       the precedence correctly, we also need people read the code..
       Here we force parens for some alien operators

       If we move assign into a statement, will be less?
       TODO: construct a test case that do need parenthesisze for expression
       IIE does not apply (will be inlined?)
    *)

    let rec need_paren  (e : J.expression) =
      match e.expression_desc with
      | Call ({expression_desc = Fun _; },_,_) -> true
      | Caml_uninitialized_obj _ 
      | Raw_js_code (_, Exp) 
      | Fun _ | Object _ -> true
      | Raw_js_code (_,Stmt)
      | Caml_block_set_tag _ 
      | Length _ 
      | Caml_block_set_length _ 
      | Anything_to_string _ 
      | String_of_small_int_array _
      | Call _ 
      | Array_append _ 
      | Array_copy _ 
      | Caml_block_tag _ 
      | Seq _
      | Dot _
      | Cond _
      | Bin _ 
      | String_access _ 
      | Access _
      | Array_of_size _ 
      | String_append _ 
      | Char_of_int _ 
      | Char_to_int _
      | Dump _
      | Json_stringify _ 
      | Math _
      | Var _ 
      | Str _ 
      | Array _ 
      | Caml_block  _ 
      | FlatCall _ 
      | Typeof _
      | Bind _ 
      | Number _
      | Not _ 
      | Bool _
      | New _ 
      | Int_of_boolean _ -> false
      (* e = function(x){...}(x);  is good
       *)
    in
    let cxt = 
      (
        if need_paren  e 
        then (P.paren_group f 1)
        else (P.group f 0)
      ) (fun _ -> expression 0 cxt f e ) in
    semi f;
    cxt 

  | If (e, s1,  s2) -> (* TODO: always brace those statements *)
    P.string f L.if_;
    P.space f;
    let cxt = P.paren_group f 1 @@ fun _ -> expression 0 cxt f e in
    P.space f;
    let cxt =
      block cxt f s1
    in
    begin match s2 with 
     | None | (Some []) |Some [{statement_desc = Block []; }]
       -> P.newline f; cxt
     | Some [{statement_desc = If _} as nest]
     | Some [{statement_desc = Block [ {statement_desc = If _ ; _} as nest] ; _}]
       ->
       P.newline f;
       P.string f L.else_;
       P.space f;
       statement false cxt f nest 
     | Some s2 -> 
       P.newline f;
       P.string f L.else_;
       P.space f ;
       block  cxt f s2 
    end

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
              P.string f "(";
              P.string f L.true_;
              P.string f ")"; 
              P.space f ;
              cxt 
          | _ -> 
              P.string f L.while_;
              let cxt = P.paren_group f 1 @@ fun _ ->  expression 0 cxt f e in
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
            P.string f "for";
            P.paren_group f 1 @@ fun _ -> 
              let cxt, new_id = 
                (match for_ident_expression, finish.expression_desc with 
                 | Some ident_expression , (Number _ | Var _ ) -> 
                   P.string f L.var;
                   P.space f;
                   let cxt  =  ident cxt f id in
                   P.space f; 
                   P.string f L.eq;
                   P.space f;
                   expression 0 cxt f ident_expression, None
                 | Some ident_expression, _ -> 
                   P.string f L.var;
                   P.space f;
                   let cxt  =  ident cxt f id in
                   P.space f;
                   P.string f L.eq;
                   P.space f; 
                   let cxt = expression 1 cxt f ident_expression in
                   P.space f ; 
                   P.string f L.comma;
                   let id = Ext_ident.create (Ident.name id ^ "_finish") in
                   let cxt = ident cxt f id in
                   P.space f ; 
                   P.string f L.eq;
                   P.space f;
                   expression 1 cxt f finish, Some id
                 | None, (Number _ | Var _) -> 
                   cxt, None 
                 | None , _ -> 
                   P.string f L.var;
                   P.string f " ";
                   let id = Ext_ident.create (Ident.name id ^ "_finish") in
                   let cxt = ident cxt f id in
                   P.string f " = ";
                   expression 15 cxt f finish, Some id
                ) in
              semi f ; 
              P.space f;
              let cxt = ident cxt f id in
              let right_prec  = 

                match direction with 
                | Upto -> 
                  let (_,_,right) = op_prec Le  in
                  P.string f "<=";
                  right
                | Downto -> 
                  let (_,_,right) = op_prec Ge in
                  P.string f ">=" ;
                  right
              in
              P.space f ; 
              let cxt  = 
                match new_id with 
                | Some i -> expression   right_prec cxt  f (E.var i)
                | None -> expression  right_prec cxt  f finish
              in
              semi f; 
              P.space f;
              let ()  = 
                match direction with 
                | Upto -> P.string f "++"
                | Downto -> P.string f "--" in
              ident cxt f id
        in
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
      let _enclose action inner_cxt lexical   = 
        let rec aux  cxt f ls  =
          match ls with
          | [] -> cxt
          | [x] -> ident cxt f x
          | y :: ys ->
            let cxt = ident cxt f y in
            P.string f L.comma;
            aux cxt f ys  in
        P.vgroup f 0
          (fun _ ->
             (
               P.string f "(function(";
               ignore @@ aux inner_cxt f lexical;
               P.string f ")";
               let cxt = P.brace_vgroup f 0  (fun _ -> action inner_cxt) in
               P.string f "(";
               ignore @@ aux inner_cxt f lexical;
               P.string f ")";
               P.string f ")";
               semi f;
               cxt
             )) 
      in
      _enclose action inner_cxt lexical

  | Continue s ->
    P.string f "continue "; (* ASI becareful...*)
    P.string f s;
    semi f;
    P.newline f;
    cxt
  | Debugger
    -> 
    P.newline f ;
    P.string f "debugger";
    semi f ;
    P.newline f;
    cxt 
  | Break
    ->
    P.string f "break ";
    semi f;
    P.newline f; 
    cxt

  | Return {return_value = e} ->
    begin match e with
      | {expression_desc = Fun ( l, b, env); _} ->
        let cxt = pp_function cxt f true l b env in
        semi f ; cxt 
      | e ->
        P.string f L.return ;
        P.space f ;

        (* P.string f "return ";(\* ASI -- when there is a comment*\) *)
        P.group f return_indent @@ fun _ -> 
          let cxt =  expression 0 cxt f e in
          semi f;
          cxt 
          (* There MUST be a space between the return and its
             argument. A line return will not work *)
    end
  | Int_switch (e, cc, def) ->
    P.string f "switch";  
    P.space f;
    let cxt = P.paren_group f 1 @@ fun _ ->  expression 0 cxt f e 
    in
    P.space f;
    P.brace_vgroup f 1 @@ fun _ -> 
      let cxt = loop cxt f (fun f i -> P.string f (string_of_int i) ) cc in
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
    P.string f "switch";
    P.space f;
    let cxt = P.paren_group f 1 @@ fun _ ->  expression 0 cxt f e 
    in
    P.space f;
    P.brace_vgroup f 1 @@ fun _ -> 
      let cxt = loop cxt f (fun f i -> pp_quote_string f i ) cc in
      (match def with
       | None -> cxt
       | Some def ->
         P.group f 1 @@ fun _ -> 
           P.string f L.default;
           P.string f L.colon;
           P.newline f;
           statement_list  false cxt  f def )

  | Throw e ->
    P.string f L.throw;
    P.space f ;
    P.group f throw_indent @@ fun _ -> 

      let cxt = expression 0 cxt f e in
      semi f ; cxt 

  (* There must be a space between the return and its
     argument. A line return would not work *)
  | Try (b, ctch, fin) ->
    P.vgroup f 0 @@ fun _-> 
      P.string f "try";
      P.space f ; 
      let cxt = block cxt f b in
      let cxt = 
        match ctch with
        | None ->
          cxt
        | Some (i, b) ->
          P.newline f;
          P.string f "catch (";
          let cxt = ident cxt f i in
          P.string f ")";
          block cxt f b
      in 
      begin match fin with
        | None -> cxt
        | Some b ->
          P.group f 1 @@ fun _ -> 
            P.string f "finally";
            P.space f;
            block cxt f b 
      end
(* similar to [block] but no braces *)
and statement_list top cxt f  b =
  match b with
  | []     -> cxt
  | [s]    -> statement top  cxt f  s
  | s :: r -> 
    let cxt = statement top cxt f s in
    P.newline f;
    (if top then P.force_newline f);
    statement_list top cxt f  r

and block cxt f b =
  (* This one is for '{' *)
  P.brace_vgroup f 1 (fun _ -> statement_list false cxt   f b )


let exports cxt f (idents : Ident.t list) = 
  let outer_cxt, reversed_list, margin = 
    List.fold_left (fun (cxt, acc, len ) (id : Ident.t) -> 
        let s = Ext_ident.convert id.name in        
        let str,cxt  = str_of_ident cxt id in         
        cxt, ( (s,str) :: acc ) , max len (String.length s)   )
      (cxt, [], 0)  idents in    
  P.newline f ;
  Ext_list.rev_iter (fun (s,export) -> 
      P.group f 0 @@ (fun _ ->  
          P.string f L.exports;
          P.string f L.dot;
          P.string f s; 
          P.nspace f (margin - String.length s +  1) ;
          P.string f L.eq;
          P.space f;
          P.string f export;          
          semi f;);
      P.newline f;
    ) reversed_list;
  outer_cxt  


(* Node style *)
let requires require_lit cxt f (modules : (Ident.t * string) list ) =
  P.newline f ; 
  (* the context used to print the following program *)  
  let outer_cxt, reversed_list, margin  =
    List.fold_left
      (fun (cxt, acc, len) (id,s) ->
         let str, cxt = str_of_ident cxt id  in
         cxt, ((str,s) :: acc), (max len (String.length str))
      )
      (cxt, [], 0)  modules in
  P.force_newline f ;    
  Ext_list.rev_iter (fun (s,file) ->
      P.string f L.var;
      P.space f ;
      P.string f s ;
      P.nspace f (margin - String.length s + 1) ;
      P.string f L.eq;
      P.space f;
      P.string f require_lit;
      P.paren_group f 0 @@ (fun _ ->
          pp_string f ~utf:true ~quote:(best_string_quote s) file  );
      semi f ;
      P.newline f ;
    ) reversed_list;
  outer_cxt

let program f cxt   ( x : J.program ) = 
  let () = P.force_newline f in
  let cxt =  statement_list true cxt f x.block  in
  let () = P.force_newline f in
  exports cxt f x.exports

let goog_program f goog_package x  = 
  P.newline f ;
  P.string f L.goog_module;
  P.string f "(";
  P.string f (Printf.sprintf "%S" goog_package);
  P.string f ")";
  semi f ;
  let cxt = requires L.goog_require ( Ext_pp_scope.empty)  f x.J.modules in
  program f cxt x.program  

let node_program f ( x : J.deps_program) = 
  let cxt = requires L.require ( Ext_pp_scope.empty)  f x.modules in
  program f cxt x.program  
  

let amd_program f 
    (  x : J.deps_program)
  = 
  P.newline f ; 
  let cxt = Ext_pp_scope.empty in
  P.vgroup f 1 @@ fun _ -> 
  P.string f L.define;
  P.string f "([";
  P.string f (Printf.sprintf "%S" L.exports);

  List.iter (fun (_,s) ->
      P.string f L.comma ;
      P.space f; 
      pp_string f ~utf:true ~quote:(best_string_quote s) s;
    ) x.modules ;
  P.string f "]";
  P.string f L.comma;
  P.newline f;
  P.string f L.function_;
  P.string f "(";
  P.string f L.exports;

  let cxt = 
    List.fold_left (fun cxt (id,_) ->         
        P.string f L.comma;
        P.space f ; 
        ident cxt f id
      ) cxt x.modules     
  in
  P.string f ")";
  let v = P.brace_vgroup f 1 @@ (fun _ -> 
      let () = P.string f L.strict_directive in 
      program f cxt x.program
    ) in
  P.string f ")";
  v
;;

let pp_deps_program ( program  : J.deps_program) (f : Ext_pp.t) = 
  begin
    P.string f "// Generated CODE, PLEASE EDIT WITH CARE";
    P.newline f; 
    P.string f L.strict_directive; 
    P.newline f ;    
    ignore (match Js_config.get_env () with 
     | Browser ->
        (node_program f program)
     | NodeJS -> 
       begin match Sys.getenv "OCAML_AMD_MODULE" with 
         | exception Not_found -> 
           node_program f program
           (* amd_program f program *)
         | _ -> amd_program f program
       end 
     | Goog opt -> 
       let goog_package = 
         let v = Lam_current_unit.get_module_name () in
         match opt with 
         | None 
         | Some ""
           -> v 
         | Some x -> x ^ "." ^ v 
       in 
       goog_program f goog_package  program
      ) ;
    P.newline f ;
    P.string f (
      match program.side_effect with
      | None -> "/* No side effect */"
      | Some v -> Printf.sprintf "/* %s Not a pure module */" v );
    P.newline f;
    P.flush f ()
  end

let dump_program (x : J.program) oc = 
  ignore (program (P.from_channel oc)  Ext_pp_scope.empty  x )

let dump_deps_program 
    x 
    (oc : out_channel) = 
  pp_deps_program x (P.from_channel oc)

let string_of_block  block  
  = 
  let buffer  = Buffer.create 50 in
  begin
    let f = P.from_buffer buffer in
    let _scope =  statement_list true Ext_pp_scope.empty  f block in
    P.flush  f ();
    Buffer.contents buffer     
  end



 
