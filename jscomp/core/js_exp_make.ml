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






let no_side_effect = Js_analyzer.no_side_effect_expression

type binary_op =   ?comment:string -> J.expression -> J.expression -> J.expression 
type unary_op =  ?comment:string -> J.expression -> J.expression
(*
  remove pure part of the expression
  and keep the non-pure part while preserve the semantics 
  (modulo return value)
 *)
let rec extract_non_pure (x : J.expression)  = 
  match x.expression_desc with 
  | Var _
  | Str _
  | Number _ -> None (* Can be refined later *)
  | Access (a,b) -> 
    begin match extract_non_pure a , extract_non_pure b with 
      | None, None -> None
      | _, _ -> Some x 
    end
  | Array (xs,_mutable_flag)  ->
    if List.for_all (fun x -> extract_non_pure x = None)  xs then
      None 
    else Some x 
  | Seq (a,b) -> 
    begin match extract_non_pure a , extract_non_pure b with 
      | None, None  ->  None
      | Some u, Some v ->  
        Some { x with expression_desc =  Seq(u,v)}
      (* may still have some simplification*)
      | None, (Some _ as v) ->  v
      | (Some _ as u), None -> u 
    end
  | _ -> Some x 

type t = J.expression 

let mk ?comment exp : t = 
  {expression_desc = exp ; comment  }

let var ?comment  id  : t = 
  {expression_desc = Var (Id id); comment }

let runtime_var_dot ?comment (x : string)  (e1 : string) : J.expression = 
  {expression_desc = 
     Var (Qualified(Ext_ident.create_js x,Runtime, Some e1)); comment }

let runtime_var_vid  x  e1 : J.vident = 
  Qualified(Ext_ident.create_js x,Runtime, Some e1)

let ml_var_dot ?comment ( id  : Ident.t) e : J.expression =     
  {expression_desc = Var (Qualified(id, Ml, Some e)); comment }

(** 
   module as a value 
   {[
     var http = require("http")
   ]}
*)
let external_var_dot ?comment  ~external_name:name ?dot (id : Ident.t) : t = 
  {expression_desc = Var (Qualified(id, External name,  dot)); comment }


let ml_var ?comment (id : Ident.t) : t  = 
  {expression_desc = Var (Qualified (id, Ml, None)); comment}

let str ?(pure=true)  ?comment s : t =  
  {expression_desc = Str (pure,s); comment}

let unicode ?comment s : t = 
  {expression_desc = Unicode (s); comment}

let raw_js_code ?comment info s : t =
  {expression_desc = Raw_js_code (s,info) ; comment }

let anything_to_string ?comment (e : t) : t =  
  match e.expression_desc with 
  | Str _ -> e 
  | _ -> {expression_desc = Anything_to_string e ; comment}

let arr ?comment mt es : t  = 
  {expression_desc = Array (es,mt) ; comment}

let sep = " : "
let merge_outer_comment comment (e : t )  = 
  match e.comment with
  | None -> {e with comment = Some comment}
  | Some s -> { e with 
                comment 
                = Some (comment ^ sep ^ s)} 

let make_block ?comment tag tag_info es mutable_flag : t = 
  let comment = 
    match comment with 
    | None -> Lam_compile_util.comment_of_tag_info tag_info 
    | _ -> comment in
  let es = 
    match tag_info with 
    | Blk_record des 
      when Array.length des <> 0 
      -> 
      List.mapi (fun i (e : t) -> merge_outer_comment des.(i) e) es
    (* TODO: may overriden its previous comments *)
    | Blk_module (Some des) 
      ->  Ext_list.map2  merge_outer_comment 
            des es
    | _ -> es 
  in
  {
    expression_desc = Caml_block( es, mutable_flag, tag,tag_info) ;
    comment 
  }    

let uninitialized_object ?comment tag size : t = 
  { expression_desc = Caml_uninitialized_obj(tag,size); comment }

let uninitialized_array ?comment (e : t) : t  = 
  match e.expression_desc with 
  | Number (Int {i = 0l; _}) -> arr ?comment NA []
  | _ -> {comment; expression_desc = Array_of_size e}


module L = Literals
(* Invariant: this is relevant to how we encode string
*)           
let typeof ?comment (e : t) : t = 
  match e.expression_desc with 
  | Number _ 
  | Length _ 
    -> str ?comment L.js_type_number
  | Str _ 
    -> str ?comment L.js_type_string
  | Array _
    -> str ?comment L.js_type_object
  | _ -> {expression_desc = Typeof e ; comment }




let new_ ?comment e0 args : t = 
  { expression_desc = New (e0,  Some args ); comment}


let unit   : t = 
  {expression_desc = Number (Int {i = 0l; c = None}) ; comment = Some "()" }



let math ?comment v args  : t = 
  {comment ; expression_desc = Math(v,args)}

(* we can do constant folding here, but need to make sure the result is consistent
   {[
     let f x = string_of_int x        
     ;; f 3            
   ]}     
   {[
     string_of_int 3
   ]}
*)
let int_to_string ?comment (e : t) : t = 
  anything_to_string ?comment e 

(* Attention: Shared *mutable state* is evil, [Js_fun_env.empty] is a mutable state ..
*)    

let ocaml_fun 
    ?comment  
    ?immutable_mask
    params block  : t = 
  let len = List.length params in
  {
    expression_desc = 
      Fun (false, params,block, Js_fun_env.empty ?immutable_mask len ); 
    comment
  }

let method_
    ?comment  
    ?immutable_mask
    params block  : t = 
  let len = List.length params in
  {
    expression_desc = 
      Fun (true, params,block, Js_fun_env.empty ?immutable_mask len ); 
    comment
  }

(** This is coupuled with {!Caml_obj.caml_update_dummy} *)
let dummy_obj ?comment ()  : t = 
  {comment  ; expression_desc = J.Array ([],Mutable)}

let is_instance_array ?comment e : t = 
  {comment; expression_desc = Bin(InstanceOf, e , str L.js_array_ctor) }

(* TODO: complete 
    pure ...
*)        
let rec seq ?comment (e0 : t) (e1 : t) : t = 
  match e0.expression_desc, e1.expression_desc with 
  | (Seq( a, {expression_desc = Number _ ;  })
    | Seq( {expression_desc = Number _ ;  },a)), _
    -> 
    seq ?comment a e1
  | _, ( Seq( {expression_desc = Number _ ;  }, a)) -> 
    (* Return value could not be changed*)
    seq ?comment e0 a
  | _, ( Seq(a,( {expression_desc = Number _ ;  } as v ) ))-> 
    (* Return value could not be changed*)
    seq ?comment (seq  e0 a) v

  | _ -> 
    {expression_desc = Seq(e0,e1); comment}

let fuse_to_seq x xs = 
  if xs = [] then x  
  else List.fold_left seq x xs 

let empty_string_literal : t = 
  {expression_desc = Str (true,""); comment = None}  

let zero_int_literal : t =   
  {expression_desc = Number (Int {i = 0l; c = None}) ; comment = None}
let one_int_literal : t = 
  {expression_desc = Number (Int {i = 1l; c = None}) ; comment = None}
let two_int_literal : t = 
  {expression_desc = Number (Int {i = 2l; c = None}) ; comment = None}
let three_int_literal : t = 
  {expression_desc = Number (Int {i = 3l; c = None}) ; comment = None}
let four_int_literal : t =   
  {expression_desc = Number (Int {i = 4l; c = None}) ; comment = None}
let five_int_literal : t = 
  {expression_desc = Number (Int {i = 5l; c = None}) ; comment = None}
let six_int_literal : t = 
  {expression_desc = Number (Int {i = 6l; c = None}) ; comment = None}
let seven_int_literal : t = 
  {expression_desc = Number (Int {i = 7l; c = None}) ; comment = None}
let eight_int_literal : t = 
  {expression_desc = Number (Int {i = 8l; c = None}) ; comment = None}
let nine_int_literal : t = 
  {expression_desc = Number (Int {i = 9l; c = None}) ; comment = None}

let obj_int_tag_literal : t =
  {expression_desc = Number (Int {i = 248l; c = None}) ; comment = None}

(* let small_int_array = Array.create 100  None *)


let int ?comment ?c  i : t = 
  {expression_desc = Number (Int {i; c}) ; comment}

let small_int i : t = 
  match i with 
  | 0 -> zero_int_literal
  | 1 -> one_int_literal
  | 2 -> two_int_literal
  | 3 -> three_int_literal
  | 4 -> four_int_literal
  | 5 -> five_int_literal
  | 6 -> six_int_literal
  | 7 -> seven_int_literal
  | 8 -> eight_int_literal
  | 9 -> nine_int_literal
  | 248 -> obj_int_tag_literal
  | i -> int (Int32.of_int i) 


let access ?comment (e0 : t)  (e1 : t) : t =
  match e0.expression_desc, e1.expression_desc with
  | Array (l,_mutable_flag) , Number (Int {i; _}) when no_side_effect e0-> 
    List.nth l  (Int32.to_int i)  (* Float i -- should not appear here *)
  | _ ->
    { expression_desc = Access (e0,e1); comment} 

let string_access ?comment (e0 : t)  (e1 : t) : t = 
  match e0.expression_desc, e1.expression_desc with
  | Str (_,s) , Number (Int {i; _}) 
    ->  (* Don't optimize {j||j} *)
    let i = Int32.to_int i  in
    if i >= 0 && i < String.length s then 
      (* TODO: check exception when i is out of range..
         RangeError?
      *)
      str (String.make 1 s.[i])
    else     { expression_desc = String_access (e0,e1); comment} 
  | _ ->
    { expression_desc = String_access (e0,e1); comment} 

let index ?comment (e0 : t)  e1 : t = 
  match e0.expression_desc with
  | Array (l,_mutable_flag)  when no_side_effect e0 -> 
    List.nth l  (Int32.to_int e1)  (* Float i -- should not appear here *)
  | Caml_block (l,_mutable_flag, _, _)  when no_side_effect e0 -> 
    List.nth l  (Int32.to_int e1)  (* Float i -- should not appear here *)
  | _ -> { expression_desc = Access (e0, int ?comment e1); comment = None} 


let index_addr ?comment ~yes ~no (e0 : t)  e1 : t = 
  match e0.expression_desc with
  | Array (l,_mutable_flag)  when no_side_effect e0 -> 
    no
  | Caml_block (l,_mutable_flag, _, _)  when no_side_effect e0 -> 
    no
  | _ ->
    yes ({ expression_desc = Access (e0, int ?comment e1); comment = None} : t) 

let call ?comment ~info e0 args : t = 
  {expression_desc = Call(e0,args,info); comment }

let flat_call ?comment e0 es : t = 
  (* TODO: optimization when es is known at compile time
      to be an array
  *)
  {expression_desc = FlatCall (e0,es); comment }

(* Dot .....................**)        
let runtime_call ?comment module_name fn_name args = 
  call ?comment 
    ~info:Js_call_info.builtin_runtime_call
    (runtime_var_dot  module_name fn_name) args

let runtime_ref module_name fn_name = 
  runtime_var_dot  module_name fn_name


(* only used in property access, 
    Invariant: it should not call an external module .. *)
let js_var ?comment  (v : string) =
  var ?comment (Ext_ident.create_js v )

let js_global ?comment  (v : string) =
  var ?comment (Ext_ident.create_js v )

(** used in normal property
    like [e.length], no dependency introduced
*)
let dot ?comment (e0 : t)  (e1 : string) : t = 
  { expression_desc = Dot (e0,  e1, true); comment} 


let undefined  = var Ext_ident.undefined


let nil = var Ext_ident.nil

(** coupled with the runtime *)
let is_caml_block ?comment (e : t) : t = 
  {expression_desc = Bin ( NotEqEq, dot e L.js_prop_length , undefined); 
   comment}

(* This is a property access not external module *)

let array_length ?comment (e : t) : t = 
  match e.expression_desc with 
  (* TODO: use array instead? *)
  | (Array (l, _) | Caml_block(l,_,_,_)) when no_side_effect e 
    -> int ?comment (Int32.of_int (List.length l))
  | _ -> { expression_desc = Length (e, Array) ; comment }

let string_length ?comment (e : t) : t =
  match e.expression_desc with 
  | Str(_,v) -> int ?comment (Int32.of_int (String.length v)) 
  (* No optimization for {j||j}*)
  | _ -> { expression_desc = Length (e, String) ; comment }

let bytes_length ?comment (e : t) : t = 
  match e.expression_desc with 
  (* TODO: use array instead? *)
  | Array (l, _) -> int ?comment (Int32.of_int (List.length l))
  | Str(_,v) -> int ?comment (Int32.of_int @@ String.length v)
  (* No optimization for unicode *)
  | _ -> { expression_desc = Length (e, Bytes) ; comment }

let function_length ?comment (e : t) : t = 
  match e.expression_desc with 
  | Fun(b, params, _, _) -> 
    let params_length = 
      List.length params in
    int ?comment 
      (Int32.of_int 
         (if b then params_length - 1 
          else params_length))
  (* TODO: optimize if [e] is know at compile time *)
  | _ -> { expression_desc = Length (e, Function) ; comment }

(** no dependency introduced *)
let js_global_dot ?comment (x : string)  (e1 : string) : t = 
  { expression_desc = Dot (js_var x,  e1, true); comment} 

let char_of_int ?comment (v : t) : t = 
  match v.expression_desc with
  | Number (Int {i; _}) ->
    str  (String.make 1(Char.chr (Int32.to_int i)))
  | Char_to_int v -> v 
  | _ ->  {comment ; expression_desc = Char_of_int v}

let char_to_int ?comment (v : t) : t = 
  match v.expression_desc with 
  | Str (_, x) -> (* No optimization for .. *)
    assert (String.length x = 1) ;
    int ~comment:(Printf.sprintf "%S"  x )  
      (Int32.of_int @@ Char.code x.[0])
  | Char_of_int v -> v 
  | _ -> {comment; expression_desc = Char_to_int v }

let array_append ?comment e el : t = 
  { comment ; expression_desc = Array_append (e, el)}

let array_copy ?comment e : t = 
  { comment ; expression_desc = Array_copy e}

(* Note that this return [undefined] in JS, 
    it should be wrapped to avoid leak [undefined] into 
    OCaml
*)    
let dump ?comment level el : t = 
  {comment ; expression_desc = Dump(level,el)}

let to_json_string ?comment e : t = 
  { comment; expression_desc = Json_stringify e }

let rec string_append ?comment (e : t) (el : t) : t = 
  match e.expression_desc , el.expression_desc  with 
  | Str(_,a), String_append ({expression_desc = Str(_,b)}, c) ->
    string_append ?comment (str (a ^ b)) c 
  | String_append (c,{expression_desc = Str(_,b)}), Str(_,a) ->
    string_append ?comment c (str (b ^ a))
  | String_append (a,{expression_desc = Str(_,b)}),
    String_append ({expression_desc = Str(_,c)} ,d) ->
    string_append ?comment (string_append a (str (b ^ c))) d 
  | Str (_,a), Str (_,b) -> str ?comment (a ^ b)
  | _, Anything_to_string b -> string_append ?comment e b 
  | Anything_to_string b, _ -> string_append ?comment b el
  | _, _ -> {comment ; expression_desc = String_append(e,el)}


let obj ?comment properties : t = 
  {expression_desc = Object properties; comment }

(* currently only in method call, no dependency introduced
*)
let var_dot ?comment (x : Ident.t)  (e1 : string) : t = 
  {expression_desc = Dot (var x,  e1, true); comment} 


let bind_call ?comment obj  (e1 : string) args  : t = 
  call ~info:Js_call_info.dummy {expression_desc = 
                                   Bind ({expression_desc = Dot (obj,  e1, true); comment} , obj);
                                 comment = None } args 

let bind_var_call ?comment (x : Ident.t)  (e1 : string) args  : t = 
  let obj =  var x in 
  call ~info:Js_call_info.dummy {expression_desc = 
                                   Bind ({expression_desc = Dot (obj,  e1, true); comment} , obj);
                                 comment = None } args 


(* Dot .....................**)        




let assign ?comment e0 e1 : t = {expression_desc = Bin(Eq, e0,e1); comment}


(** Convert a javascript boolean to ocaml boolean
    It's necessary for return value
     this should be optmized away for [if] ,[cond] to produce 
    more readable code
*)         
let bool_of_boolean ?comment (e : t) : t = 
  match e.expression_desc with 
  | Int_of_boolean _
  | Number _ -> e 
  | _ -> {comment ; expression_desc = Int_of_boolean e}

let to_number ?comment (e : t) : t = 
  match e.expression_desc with 
  | Int_of_boolean _
  | Anything_to_number _
  | Number _ -> e 
  | _ -> {comment ; expression_desc = Anything_to_number e}

let caml_true  = int ~comment:"true" 1l (* var (Jident.create_js "true") *)

let caml_false  = int ~comment:"false" 0l

let bool v = if  v then caml_true else caml_false

(** Here we have to use JS [===], and therefore, we are introducing 
    Js boolean, so be sure to convert it back to OCaml boolean
*)
let rec triple_equal ?comment (e0 : t) (e1 : t ) : t = 
  match e0.expression_desc, e1.expression_desc with
  | Var (Id ({name = "undefined"|"null"} as id)), 
    (Char_of_int _ | Char_to_int _ 
    | Bool _ | Number _ | Typeof _ | Int_of_boolean _ 
    | Fun _ | Array _ | Caml_block _ )
    when Ext_ident.is_js id && no_side_effect e1 -> 
    caml_false (* TODO: rename it as [caml_false] *)
  | 
    (Char_of_int _ | Char_to_int _ 
    | Bool _ | Number _ | Typeof _ | Int_of_boolean _ 
    | Fun _ | Array _ | Caml_block _ ),  Var (Id ({name = "undefined"|"null"; } as id))
    when Ext_ident.is_js id && no_side_effect e0 -> 
    caml_false
  | Str (_,x), Str (_,y) ->  (* CF*)
    bool (Ext_string.equal x y)
  | Char_to_int a , Char_to_int b -> 
    triple_equal ?comment a b 
  | Char_to_int a , Number (Int {i; c = Some v}) 
  | Number (Int {i; c = Some v}), Char_to_int a  -> 
    triple_equal ?comment a (str (String.make 1 v))
  | Number (Int {i = i0; _}), Number (Int {i = i1; _}) 
    -> 
    bool (i0 = i1)      
  | Char_of_int a , Char_of_int b -> 
    triple_equal ?comment a b 
  | _ -> 
    bool_of_boolean  {expression_desc = Bin(EqEqEq, e0,e1); comment}


(** Arith operators *)
(* Dot .....................**)        

let float ?comment f : t = 
  {expression_desc = Number (Float {f}); comment}

let zero_float_lit : t = 
  {expression_desc = Number (Float {f = "0." }); comment = None}


let float_mod ?comment e1 e2 : J.expression = 
  { comment ; 
    expression_desc = Bin (Mod, e1,e2)
  }

let bin ?comment (op : J.binop) e0 e1 : t =
  match op with
  | EqEqEq -> triple_equal ?comment e0 e1
  | _ -> {expression_desc = Bin(op,e0,e1); comment}

(* | (Bin (NotEqEq, e1,  *)
(*         {expression_desc = Var (Id ({name = "undefined"; _} as id))}) *)
(*   | Bin (NotEqEq,  *)
(*          {expression_desc = Var (Id ({name = "undefined"; _} as id))},  *)
(*          e1) *)
(*   ),  *)
(*   _ when Ext_ident.is_js id ->  *)
(*   and_ e1 e2 *)
(* TODO: Constant folding, Google Closure will do that?,
   Even if Google Clsoure can do that, we will see how it interact with other
   optimizations
   We wrap all boolean functions here, since OCaml boolean is a 
   bit different from Javascript, so that we can change it in the future
*)
let rec and_ ?comment (e1 : t) (e2 : t) : t = 
  match e1.expression_desc, e2.expression_desc with 
  |  Int_of_boolean e1 , Int_of_boolean e2 ->
    bool_of_boolean @@ and_ ?comment e1 e2

  (*
     {[ a && (b && c) === (a && b ) && c ]}
     is not used: benefit is not clear 
     | Int_of_boolean e10, Bin(And, {expression_desc = Int_of_boolean e20 }, e3) 
      -> 
      and_ ?comment 
        { e1 with expression_desc 
                  = 
                    J.Int_of_boolean { expression_desc = Bin (And, e10,e20); comment = None}
        }
        e3
  *)
  (* Note that 
     {[ "" && 3 ]}
     return  "" instead of false, so [e1] is indeed useful
  *)

  (* optimization if [e1 = e2], then and_ e1 e2 -> e2
     be careful for side effect        
  *)
  | Var i, Var j when Js_op_util.same_vident  i j 
    -> 
    e1
  | Var i, 
    (Bin (And,   {expression_desc = Var j ; _}, _) 
    | Bin (And ,  _, {expression_desc = Var j ; _}))
    when Js_op_util.same_vident  i j 
    ->
    e2          
  | _, _ ->     
    { expression_desc = Bin(And, e1,e2) ; comment }


let rec or_ ?comment (e1 : t) (e2 : t) = 
  match e1.expression_desc, e2.expression_desc with 
  | Int_of_boolean e1 , Int_of_boolean e2
    ->
    bool_of_boolean @@ or_ ?comment e1 e2
  | Var i, Var j when Js_op_util.same_vident  i j 
    -> 
    e1
  | Var i, 
    (Bin (Or,   {expression_desc = Var j ; _}, _) 
    | Bin (Or ,  _, {expression_desc = Var j ; _}))
    when Js_op_util.same_vident  i j 
    -> e2          
  | _, _ ->     
    {expression_desc = Bin(Or, e1,e2); comment }


(* return a value of type boolean *)
(* TODO: 
     when comparison with Int
     it is right that !(x > 3 ) -> x <= 3 *)
let rec not ({expression_desc; comment} as e : t) : t =
  match expression_desc with 
  | Number (Int {i; _}) -> 
    if i <> 0l then caml_false else caml_true
  | Int_of_boolean  x -> js_not  x  e
  | Caml_not e -> e
  | Js_not e -> e 
  (* match expression_desc with  *)
  (* can still hapen after some optimizations *)
  | Bin(EqEqEq , e0,e1) 
    -> {expression_desc = Bin(NotEqEq, e0,e1); comment}
  | Bin(NotEqEq , e0,e1) -> {expression_desc = Bin(EqEqEq, e0,e1); comment}
  | Bin(Lt, a, b) -> {e with expression_desc = Bin (Ge,a,b)}
  | Bin(Ge,a,b) -> {e with expression_desc = Bin (Lt,a,b)}
  | Bin(Le,a,b) -> {e with expression_desc = Bin (Gt,a,b)}
  | Bin(Gt,a,b) -> {e with expression_desc = Bin (Le,a,b)}
  | x -> {expression_desc = Caml_not e ; comment = None}
and js_not ({expression_desc; comment} as e : t) origin : t =
  match expression_desc with 
  | Bin(EqEqEq , e0,e1) 
    -> 
    bool_of_boolean {expression_desc = Bin(NotEqEq, e0,e1); comment}
  | Bin(NotEqEq , e0,e1) -> 
    bool_of_boolean {expression_desc = Bin(EqEqEq, e0,e1); comment}
  | Bin(Lt, a, b) -> 
    bool_of_boolean {e with expression_desc = Bin (Ge,a,b)}
  | Bin(Ge,a,b) -> 
    bool_of_boolean {e with expression_desc = Bin (Lt,a,b)}
  | Bin(Le,a,b) -> 
    bool_of_boolean {e with expression_desc = Bin (Gt,a,b)}
  | Bin(Gt,a,b) -> 
    bool_of_boolean {e with expression_desc = Bin (Le,a,b)}
  | _ -> {expression_desc = Caml_not origin; comment = None}

let rec ocaml_boolean_under_condition (b : t) =
  match b.expression_desc with 
  | Int_of_boolean b -> ocaml_boolean_under_condition b 
  | Bin (And, x,y) -> 
    let x' = ocaml_boolean_under_condition x in 
    let y' = ocaml_boolean_under_condition y in 
    if x == x' && y==y' then b 
    else {b with expression_desc = Bin(And,x',y')}
  | Bin(Or,x,y) ->
    let x' = ocaml_boolean_under_condition x in 
    let y' = ocaml_boolean_under_condition y in 
    if x == x' && y == y' then b 
    else {b with expression_desc = Bin(Or,x',y')}
  (** TODO: settle down Not semantics *)
  | Caml_not u
    -> 
    let u' = ocaml_boolean_under_condition u in 
    {b with expression_desc = Js_not u'}
  | Js_not u 
    -> 
    let u' = ocaml_boolean_under_condition u in 
    if u' == u then b 
    else {b with expression_desc = Js_not u'} 
  | _ -> b 

let rec econd ?comment (b : t) (t : t) (f : t) : t = 
  match b.expression_desc , t.expression_desc, f.expression_desc with

  | Number ((Int { i = 0l; _}) ), _, _ 
    -> f  (* TODO: constant folding: could be refined *)
  | (Number _ | Array _ | Caml_block _), _, _ when no_side_effect b 
    -> t  (* a block can not be false in OCAML, CF - relies on flow inference*)

  | Bool true, _, _ -> t 
  | Bool false,  _, _ -> f
  | (Bin (Bor, v , {expression_desc = Number (Int {i = 0l ; _})})), _, _
    -> econd v t f 
  (* TODO: could be more non undefined cases 
     check [caml_obj_is_block]
     acutally we should avoid introducing undefined
     as much as we can, this kind of inlining and mirco-optimization
     can be done after we can inline runtime in the future 
  *)
  (* | Bin (NotEqEq, ({expression_desc = Length _; _} as e1) , *)
  (*        {expression_desc = Var (Id ({name = "undefined"; _} as id))}), *)
  (*   _, _  *)
  (*   when Ext_ident.is_js id -> *)
  (*   econd e1 t f *)

  | ((Bin ((EqEqEq, {expression_desc = Number (Int { i = 0l; _}); _},x)) 
     | Bin (EqEqEq, x,{expression_desc = Number (Int { i = 0l; _});_}))), _, _ 
    -> 
    econd ?comment x f t 

  | (Bin (Ge, 
          ({expression_desc = Length _ ;
            _}), {expression_desc = Number (Int { i = 0l ; _})})), _, _ 
    -> f

  | (Bin (Gt, 
          ({expression_desc = Length _;
            _} as pred ), {expression_desc = Number (Int {i = 0l; })})), _, _
    ->
    (** Add comment when simplified *)
    econd ?comment pred t f 

  | _, (Cond (p1, branch_code0, branch_code1)), _
    when Js_analyzer.eq_expression branch_code1 f
    ->
    (* {[
         if b then (if p1 then branch_code0 else branch_code1)
         else branch_code1         
       ]}
       is equivalent to         
       {[
         if b && p1 then branch_code0 else branch_code1           
       ]}         
    *)      
    econd (and_ b p1) branch_code0 f
  | _, (Cond (p1, branch_code0, branch_code1)), _
    when Js_analyzer.eq_expression branch_code0 f
    ->
    (* the same as above except we revert the [cond] expression *)      
    econd (and_ b (not p1)) branch_code1 f

  | _, _, (Cond (p1', branch_code0, branch_code1))
    when Js_analyzer.eq_expression t branch_code0 
    (*
       {[
         if b then branch_code0 else (if p1' then branch_code0 else branch_code1)           
       ]}         
       is equivalent to         
       {[
         if b or p1' then branch_code0 else branch_code1           
       ]}         
    *)
    ->
    econd (or_ b p1') t branch_code1
  | _, _, (Cond (p1', branch_code0, branch_code1))
    when Js_analyzer.eq_expression t branch_code1
    ->
    (* the same as above except we revert the [cond] expression *)      
    econd (or_ b (not p1')) t branch_code0

  | Caml_not e, _, _ 
  | Js_not e, _, _ 
    ->
    econd ?comment e f t 
  | Int_of_boolean  b, _, _  -> econd ?comment  b t f
  (* | Bin (And ,{expression_desc = Int_of_boolean b0},b1), _, _  -> *)
  (*   econd ?comment { b with expression_desc = Bin (And , b0,b1)} t f *)
  | _ -> 
    let b  = ocaml_boolean_under_condition b in 
    (* if b' <> b then *)
    (*   econd ?comment b' t f  *)
    (* else  *)
    if Js_analyzer.eq_expression t f then
      if no_side_effect b then t else seq  ?comment b t
    else
      {expression_desc = Cond(b,t,f); comment}


let rec float_equal ?comment (e0 : t) (e1 : t) : t = 
  match e0.expression_desc, e1.expression_desc with     
  | Number (Int {i = i0 ; _}), Number (Int {i = i1; }) -> 
    bool (i0 = i1)
  | (Bin(Bor, 
         {expression_desc = Number(Int {i = 0l; _})}, 
         ({expression_desc = Caml_block_tag _; _} as a ))
    |
      Bin(Bor, 
          ({expression_desc = Caml_block_tag _; _} as a),
          {expression_desc = Number (Int {i = 0l; _})})), 
    Number (Int {i = 0l; _})
    ->  (** (x.tag | 0) === 0  *)
    not  a     
  | (Bin(Bor, 
         {expression_desc = Number(Int {i = 0l; _})}, 
         ({expression_desc = Caml_block_tag _; _} as a ))
    |
      Bin(Bor, 
          ({expression_desc = Caml_block_tag _; _} as a),
          {expression_desc = Number (Int {i = 0l; _})}))
  , Number _  ->  (* for sure [i <> 0 ]*)
    (* since a is integer, if we guarantee there is no overflow 
       of a
       then [a | 0] is a nop unless a is undefined
       (which is applicable when applied to tag),
       obviously tag can not be overflowed. 
       if a is undefined, then [ a|0===0 ] is true 
       while [a === 0 ] is not true
       [a|0 === non_zero] is false and [a===non_zero] is false
       so we can not eliminate when the tag is zero          
    *)
    float_equal ?comment a e1
  | Number (Float {f = f0; _}), Number (Float {f = f1 ; }) when f0 = f1 -> 
    caml_true

  | Char_to_int a , Char_to_int b ->
    float_equal ?comment a b
  | Char_to_int a , Number (Int {i; c = Some v})
  | Number (Int {i; c = Some v}), Char_to_int a  ->
    float_equal ?comment a (str (String.make 1 v))
  | Char_of_int a , Char_of_int b ->
    float_equal ?comment a b

  | _ ->  
    bool_of_boolean {expression_desc = Bin(EqEqEq, e0,e1); comment}


let int_equal = float_equal 
let rec string_equal ?comment (e0 : t) (e1 : t) : t = 
  match e0.expression_desc, e1.expression_desc with     
  | Str (_, a0), Str(_, b0) 
    -> bool  (Ext_string.equal a0 b0)
  | Unicode a0, Unicode b0 -> bool (Ext_string.equal a0 b0)
  | _ , _ 
    ->
    bool_of_boolean {expression_desc = Bin(EqEqEq, e0,e1); comment}     


let is_type_number ?comment (e : t) : t = 
  string_equal ?comment (typeof e) (str "number")    


let string_of_small_int_array ?comment xs : t = 
  {expression_desc = String_of_small_int_array xs; comment}



(* we are calling [Caml_primitive.primitive_name], since it's under our
   control, we should make it follow the javascript name convention, and
   call plain [dot]
*)          

let null ?comment () =     
  js_global ?comment "null"

let tag ?comment e : t = 
  {expression_desc = 
     Bin (Bor, {expression_desc = Caml_block_tag e; comment }, zero_int_literal );
   comment = None }    


let bind ?comment fn obj  : t = 
  {expression_desc = Bind (fn, obj) ; comment }


(* according to the compiler, [Btype.hash_variant], 
   it's reduced to 31 bits for hash
*)

let public_method_call meth_name obj label cache args = 
  let len = List.length args in 
  (* econd (int_equal (tag obj ) obj_int_tag_literal) *)
  if len <= 7 then          
    runtime_call Js_runtime_modules.caml_oo_curry 
      ("js" ^ string_of_int (len + 1) )
      (label:: ( int cache) :: obj::args)
  else 
    runtime_call Js_runtime_modules.caml_oo_curry "js"
      [label; 
       int cache;
       obj ;  
       arr NA (obj::args)
      ]

(* TODO: handle arbitrary length of args .. 
   we can reduce part of the overhead by using
   `__js` -- a easy ppx {{ x ##.hh }} 
   the downside is that no way to swap ocaml/js implementation 
   for object part, also need encode arity..
   how about x#|getElementById|2|
*)
(* ( *)
(*   let fn = bind (dot obj meth_name) obj in *)
(*   if len = 0 then  *)
(*     dot obj meth_name *)
(*     (\* Note that when no args supplied,  *)
(*        it is not necessarily a function, [bind] *)
(*        is dangerous *)
(*        so if user write such code *)
(*        {[ *)
(*          let  u = x # say in *)
(*          u 3               *)
(*        ]}     *)
(*        It's reasonable to drop [this] support        *)
(*     *\) *)
(*   else if len <=8 then  *)
(*     let len_str = string_of_int len in *)
(*     runtime_call Js_config.curry (Literals.app ^len_str)  *)
(*       (fn ::  args) *)
(*   else  *)
(*     runtime_call Js_config.curry Literals.app_array            *)
(*       [fn  ; arr NA args ]             *)
(* ) *)

let set_tag ?comment e tag : t = 
  seq {expression_desc = Caml_block_set_tag (e,tag); comment } unit 


(* Note that [lsr] or [bor] are js semantics *)
let rec int32_bor ?comment (e1 : J.expression) (e2 : J.expression) : J.expression = 
  match e1.expression_desc, e2.expression_desc with 
  | Number (Int {i = i1} | Uint i1), Number (Int {i = i2})
    -> int ?comment (Int32.logor i1 i2)

  | _, (Bin(Lsr,e2, {expression_desc = Number(Int{i=0l} | Uint 0l | Nint 0n) ; _})) ->
    int32_bor  e1 e2
  | (Bin(Lsr,e1, {expression_desc = Number(Int{i=0l} | Uint 0l | Nint 0n) ; _})), _ ->
    int32_bor  e1 e2
  | (Bin(Lsr,_, {expression_desc = Number(Int{i} | Uint i ) ; _})),
    Number(Int{i=0l} | Uint 0l | Nint 0n)
    when i > 0l  -> (* a >>> 3 | 0 -> a >>> 3 *)
    e1

  | Bin (Bor, e1, {expression_desc = Number(Int{i=0l} | Uint 0l | Nint 0n) ; _} ), 
    Number(Int{i=0l} | Uint 0l | Nint 0n) ->
    int32_bor e1 e2  
  | _ -> 
    { comment ; 
      expression_desc = Bin (Bor, e1,e2)
    }

(* Arithmatic operations
   TODO: distinguish between int and float
   TODO: Note that we have to use Int64 to avoid integer overflow, this is fine
   since Js only have .

   like code below 
   {[
     MAX_INT_VALUE - (MAX_INT_VALUE - 100) + 20
   ]}

   {[
     MAX_INT_VALUE - x + 30
   ]}

   check: Re-association: avoid integer overflow
*) 
let  to_int32  ?comment (e : J.expression)  : J.expression = 
  int32_bor ?comment e zero_int_literal
(* TODO: if we already know the input is int32, [x|0] can be reduced into [x] *)
let nint ?comment n : J.expression =
  {expression_desc = Number (Nint n); comment }

let uint32 ?comment n : J.expression =
  {expression_desc = Number (Uint n); comment }



let string_comp cmp ?comment  e0 e1 = 
  bool_of_boolean @@ bin ?comment cmp e0 e1

let set_length ?comment e tag : t = 
  seq {expression_desc = Caml_block_set_length (e,tag); comment } unit 
let obj_length ?comment e : t = 
  to_int32 {expression_desc = Length (e, Caml_block); comment }

let rec int_comp (cmp : Lambda.comparison) ?comment  (e0 : t) (e1 : t) = 
  match cmp, e0.expression_desc, e1.expression_desc with
  | _, Call ({
      expression_desc = 
        Var (Qualified 
               (_, Runtime, 
                Some ("caml_int_compare" | "caml_int32_compare"))); _}, 
      [l;r], _), 
    Number (Int {i = 0l})
    -> int_comp cmp l r (* = 0 > 0 < 0 *)
  | Ceq, _, _ -> int_equal e0 e1 
  | _ ->          
    bool_of_boolean @@ bin ?comment (Lam_compile_util.jsop_of_comp cmp) e0 e1

let float_comp cmp ?comment  e0 e1 = 
  bool_of_boolean @@ bin ?comment (Lam_compile_util.jsop_of_comp cmp) e0 e1

let js_comp cmp ?comment  e0 e1 = 
  bool_of_boolean @@ bin ?comment (Lam_compile_util.jsop_of_comp cmp) e0 e1


let rec int32_lsr ?comment
    (e1 : J.expression) 
    (e2 : J.expression) : J.expression =
  let aux i1 i =
    uint32 (Int32.shift_right_logical i1 i) in    
  match e1.expression_desc, e2.expression_desc with
  | Number (Int { i = i1} | Uint i1 ), Number( Int {i = i2} | Uint i2)
    -> aux i1 (Int32.to_int i2)
  | Number (Nint i1), Number( Int {i = i2} | Uint i2)
    ->
    aux (Nativeint.to_int32 i1) (Int32.to_int i2)    
  | Number (Nint i1), Number (Nint i2)
    ->
    aux (Nativeint.to_int32 i1) (Nativeint.to_int i2)
  | (Bin(Lsr, _, _)), Number (Int {i = 0l} | Uint 0l | Nint 0n) 
    ->  e1 (* TODO: more opportunities here *)
  | Bin(Bor, e1, {expression_desc = Number (Int {i=0l;_} | Uint 0l | Nint 0n) ; _}),
    Number (Int {i = 0l} | Uint 0l | Nint 0n) 
    -> int32_lsr ?comment e1 e2
  | _, _ ->
    { comment ; 
      expression_desc = Bin (Lsr, e1,e2) (* uint32 *)
    }

let to_uint32 ?comment (e : J.expression)  : J.expression =
  int32_lsr ?comment e zero_int_literal


(* TODO: 
   we can apply a more general optimization here, 
   do some algebraic rewerite rules to rewrite [triple_equal]           
*)        
let rec is_out ?comment (e : t) (range : t) : t  = 
  begin match range.expression_desc, e.expression_desc with 

    | Number (Int {i = 1l}), Var _ 
      ->         
      not (or_ (triple_equal e zero_int_literal) (triple_equal e one_int_literal))                  
    | Number (Int {i = 1l}), 
      (
        Bin (Plus , {expression_desc = Number (Int {i ; _}) }, {expression_desc = Var _; _})
      | Bin (Plus, {expression_desc = Var _; _}, {expression_desc = Number (Int {i ; _}) }))

      ->
      not (or_ (triple_equal e (int (Int32.neg i ))) (triple_equal e (int (Int32.sub Int32.one  i))))        
    | Number (Int {i = 1l}), 
      Bin (Minus ,  ({expression_desc = Var _; _} as x), {expression_desc = Number (Int {i ; _}) })        
      ->           
      not (or_ (triple_equal x (int (Int32.add i  1l ))) (triple_equal x (int i)))        
    (* (x - i >>> 0 ) > k *)          
    | Number (Int {i = k}), 
      Bin (Minus ,  ({expression_desc = Var _; _} as x), 
           {expression_desc = Number (Int {i ; _}) })        
      ->           
      (or_ (int_comp Cgt x (int (Int32.add i  k)))  (int_comp Clt x  (int i)))
    | Number (Int {i = k}), Var _  
      -> 
      (* Note that js support [ 1 < x < 3], 
         we can optimize it into [ not ( 0<= x <=  k)]           
      *)        
      or_ (int_comp Cgt e (int ( k)))  (int_comp Clt e  zero_int_literal)

    | _, Bin (Bor ,
              ({expression_desc =
                  (Bin((Plus | Minus ) ,
                       {expression_desc = Number (Int {i ; _}) }, {expression_desc = Var _; _})
                  |Bin((Plus | Minus ) ,
                       {expression_desc = Var _; _}, {expression_desc = Number (Int {i ; _}) } ))
               } as e), {expression_desc = Number (Int {i=0l} | Uint 0l | Nint 0n); _})
      ->  
      (* TODO: check correctness *)
      is_out ?comment e range 
    | _, _ ->
      int_comp ?comment Cgt (to_uint32 e)  range 
  end

let rec float_add ?comment (e1 : t) (e2 : t) = 
  match e1.expression_desc, e2.expression_desc with 
  | Number (Int {i;_}), Number (Int {i = j;_}) -> 
    int ?comment (Int32.add i  j)
  | _, Number (Int {i = j; c}) when j < 0l -> 
    float_minus ?comment e1 
      {e2 with expression_desc = Number (Int {i = Int32.neg j; c})}       

  | Bin(Plus, a1 , ({expression_desc = Number (Int {i = k; _})}  )), 
    Number (Int { i =j; _}) -> 
    {comment ; expression_desc = Bin(Plus, a1,  (int (Int32.add k  j)))}
  (* bin ?comment Plus a1 (int (k + j)) *)

  (* TODO remove commented code  ?? *)
  (* | Bin(Plus, a0 , ({expression_desc = Number (Int a1)}  )), *)
  (*     Bin(Plus, b0 , ({expression_desc = Number (Int b1)}  )) *)
  (*   ->  *)
  (*   bin ?comment Plus a1 (int (a1 + b1)) *)

  (* | _, Bin(Plus,  b0, ({expression_desc = Number _}  as v)) *)
  (*   -> *)
  (*     bin ?comment Plus (bin ?comment Plus e1 b0) v *)
  (* | Bin(Plus, a1 , ({expression_desc = Number _}  as v)), _ *)
  (* | Bin(Plus, ({expression_desc = Number _}  as v),a1), _ *)
  (*   ->  *)
  (*     bin ?comment Plus (bin ?comment Plus a1 e2 ) v  *)
  (* | Number _, _ *)
  (*   ->  *)
  (*     bin ?comment Plus  e2 e1 *)
  | _ -> {comment ; expression_desc = Bin(Plus, e1,e2)}
(* bin ?comment Plus e1 e2 *)
(* associative is error prone due to overflow *)
and float_minus ?comment  (e1 : t) (e2 : t) : t = 
  match e1.expression_desc, e2.expression_desc with 
  | Number (Int {i;_}), Number (Int {i = j;_}) -> 
    int ?comment (Int32.sub i  j)
  | _ ->  {comment ; expression_desc = Bin(Minus, e1,e2)}
(* bin ?comment Minus e1 e2 *)



let unchecked_int32_add ?comment e1 e2 = 
  float_add ?comment e1 e2

let int32_add ?comment e1 e2 = 
  to_int32 @@ float_add ?comment e1 e2


let int32_minus ?comment e1 e2 : J.expression = 
  to_int32 @@  float_minus ?comment e1 e2

let unchecked_int32_minus ?comment e1 e2 : J.expression = 
  float_minus ?comment e1 e2



let float_div ?comment e1 e2 = 
  bin ?comment Div e1 e2 
let float_notequal ?comment e1 e2 = 
  bin ?comment NotEqEq e1 e2


let int32_asr ?comment e1 e2 : J.expression = 
  { comment ; 
    expression_desc = Bin (Asr, e1,e2)
  }

(** Division by zero is undefined behavior*)
let int32_div ~checked ?comment 
    (e1 : t) (e2 : t) : t = 
  match e1.expression_desc, e2.expression_desc with 
  | Length _ , Number (Int {i = 2l} | Uint 2l | Nint 2n)
    -> int32_asr e1 one_int_literal 
  | e1_desc , Number (Int {i = i1} ) when i1 <> 0l
    -> 
    begin match e1_desc with 
      | Number(Int {i = i0})
        -> 
        int (Int32.div i0 i1)
      | _ -> to_int32 (float_div ?comment e1 e2)
    end
  | _, _ -> 
    if checked  then 
      runtime_call Js_runtime_modules.int32 "div" [e1; e2]
    else to_int32 (float_div ?comment e1 e2)


let int32_mod ~checked ?comment e1 (e2 : t) : J.expression = 
  match e2.expression_desc with 
  | Number (Int {i }) when i <> 0l 
    -> 
    { comment ; 
      expression_desc = Bin (Mod, e1,e2)
    }

  | _ -> 
    if checked then 
      runtime_call Js_runtime_modules.int32 "mod_" [e1;e2]
    else 
      { comment ; 
        expression_desc = Bin (Mod, e1,e2)
      }


let float_mul ?comment e1 e2 = 
  bin ?comment Mul e1 e2 




let int32_lsl ?comment (e1 : J.expression) (e2 : J.expression) : J.expression = 
  match e1, e2  with 
  | {expression_desc = Number (Int {i = i0} | Uint i0)}, 
    {expression_desc = Number (Int {i = i1} | Uint i1)}
    -> int ?comment (Int32.shift_left i0 (Int32.to_int i1))
  | _ -> 
    { comment ; 
      expression_desc = Bin (Lsl, e1,e2)
    }


let int32_mul ?comment 
    (e1 : J.expression) 
    (e2 : J.expression) : J.expression = 
  match e1, e2 with 
  | {expression_desc = Number (Int {i = 0l}|  Uint 0l | Nint 0n); _}, x
  | x, {expression_desc = Number (Int {i = 0l}|  Uint 0l | Nint 0n); _} 
    when Js_analyzer.no_side_effect_expression x 
    -> zero_int_literal
  | {expression_desc = Number (Int{i = i0}); _}, {expression_desc = Number (Int {i = i1}); _}
    -> int (Int32.mul i0 i1)
  | e , {expression_desc = Number (Int {i = i0} | Uint i0 ); _}
  | {expression_desc = Number (Int {i = i0} | Uint i0 ); _}, e 
    -> 
    let i =  Ext_pervasives.is_pos_pow i0  in 
    if i >= 0 then 
      int32_lsl e (small_int i)
    else 
      runtime_call ?comment Js_runtime_modules.int32 Literals.imul [e1;e2]
  | _ -> 
    runtime_call ?comment Js_runtime_modules.int32 Literals.imul [e1;e2]

let unchecked_int32_mul ?comment e1 e2 : J.expression = 
  { comment ; 
    expression_desc = Bin (Mul, e1,e2)
  }



let rec int32_bxor ?comment (e1 : t) (e2 : t) : J.expression = 
  match e1.expression_desc, e2.expression_desc with 
  | Number (Int {i = i1}), Number (Int {i = i2})
    -> int ?comment (Int32.logxor i1 i2)
  | _, (Bin(Lsr,e2, {expression_desc = Number(Int{i=0l} | Uint 0l | Nint 0n) ; _})) ->
    int32_bxor  e1 e2
  | (Bin(Lsr,e1, {expression_desc = Number(Int{i=0l} | Uint 0l | Nint 0n) ; _})), _ ->
    int32_bxor  e1 e2

  | _ -> 
    { comment ; 
      expression_desc = Bin (Bxor, e1,e2)
    }

let rec int32_band ?comment (e1 : J.expression) (e2 : J.expression) : J.expression = 
  match e1.expression_desc with 
  | Bin (Bor ,a, {expression_desc = Number (Int {i = 0l})})
    -> 
    (* Note that in JS
       {[ -1 >>> 0 & 0xffffffff = -1]} is the same as 
       {[ (-1 >>> 0 | 0 ) & 0xffffff ]}
    *)
    int32_band a e2
  | _  ->
    { comment ; 
      expression_desc = Bin (Band, e1,e2)
    }


(* let int32_bin ?comment op e1 e2 : J.expression =  *)
(*   {expression_desc = Int32_bin(op,e1, e2); comment} *)


(* TODO -- alpha conversion 
    remember to add parens..
*)
let of_block ?comment ?e block : t = 
  call ~info:Js_call_info.ml_full_call
    {
      comment ;
      expression_desc = 
        Fun (false, [], 
             begin match e with 
               | None -> block 
               | Some e -> 
                 Ext_list.append block  
                   [{J.statement_desc = Return {return_value = e } ;
                     comment}]
             end
            , Js_fun_env.empty 0)
    } []

let is_nil ?comment x = triple_equal ?comment x nil 

let js_bool ?comment x : t = 
  { comment; 
    expression_desc = Bool x
  }

let is_undef ?comment x = triple_equal ?comment x undefined

let is_null_undefined ?comment (x: t) : t = 
  match x.expression_desc with 
  | Var (Id ({name = "undefined" | "null"} as id))
    when Ext_ident.is_js id 
    -> caml_true
  | Number _ | Array _ | Caml_block _ -> caml_false
  | _ -> 
    bool_of_boolean
      { comment ; 
        expression_desc = Is_null_undefined_to_boolean x 
      }
let not_implemented ?comment (s : string) : t =  
  runtime_call
    Js_runtime_modules.missing_polyfill
    "not_implemented" 
    [str (s ^ " not implemented by bucklescript yet\n")]

(* call ~info:Js_call_info.ml_full_call *)
(*   { *)
(*     comment ; *)
(*     expression_desc =  *)
(*       Fun (false,[], ( *)
(*           [{J.statement_desc = *)
(*               Throw (str ?comment  *)
(*                        (s ^ " not implemented by bucklescript yet\n")) ; *)
(*             comment}]) , *)
(*            Js_fun_env.empty 0) *)
(*   } [] *)


