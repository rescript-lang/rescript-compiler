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



let prim = "Caml_primitive" 

let exceptions = "Caml_exceptions"

let io = "Caml_io"

let sys = "Caml_sys"

let lex_parse = "Caml_lexer"

let obj_runtime = "Caml_obj_runtime"

let array = "Caml_array"

let format = "Caml_format"

let string = "Caml_string"

let float = "Caml_float"

let oo = "Caml_oo"

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

(* TODO Should this comment be removed *)
(* let  non_pure_output_of_exp ( x : J.expresion) : Js_output.t  =  *)
(*   let rec aux x =  *)
(*   match x with  *)
(*   | Var _ *)
(*   | Str _ *)
(*   | Number _ -> `Empty *)
(*   | Access (a,b) ->  *)
(*       begin match aux a , aux b with  *)
(*       | `Empty, `Empty -> `Empty *)
(*       | _, _ -> `Exp x  *)
(*       end *)
(*   | Array xs  -> *)
(*       if List.for_all (fun x -> aux x = `Empty)  xs then *)
(*         `Empty *)
(*       else `Exp x  *)
(*   | Seq (a,b) ->  *)
(*       begin match aux a , aux b with  *)
(*       | `Empty, `Empty  ->  `Empty *)
(*       | `Exp u, `Exp v ->   *)
(*           `Block ([S.exp u],  v) *)
(*       | None, ((`Exp _) as v) ->  v *)
(*       | (`Exp _ as u), None -> u  *)
(*       | `Block (b1, e1), `Block (b2, e2) ->  *)
(*           `Block (b1 @ (S.exp e1 :: b2 ) , e2) *)
(*       end *)
(*   | _ -> `Exp  x  in *)
(*   match aux x with *)
(*   | `Empty ->  Js_output.dummy *)
  
let rec is_constant (x : J.expression)  = 
  match x.expression_desc with 
  | Access (a,b) -> is_constant a && is_constant b 
  | Str (b,_) -> b
  | Number _ -> true (* Can be refined later *)
  | Array (xs,_mutable_flag)  -> List.for_all is_constant  xs 
  | _ -> false 

module Exp = struct 
  (* type nonrec t = t  a [bug in pretty printer] *)
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

  let external_var_dot ?comment (id : Ident.t) name fn : t = 
    {expression_desc = Var (Qualified(id, External name, Some fn)); comment }

  let ml_var ?comment (id : Ident.t) : t  = 
    {expression_desc = Var (Qualified (id, Ml, None)); comment}
      
  let str ?(pure=true) ?comment s : t =  {expression_desc = Str (pure,s); comment}

  (* Shared mutable state is evil 
      [Js_fun_env.empty] is a mutable state ..
   *)    
  let efun ?comment  ?immutable_mask
      params block  : t = 
    let len = List.length params in
    {
      expression_desc = Fun ( params,block, Js_fun_env.empty ?immutable_mask len ); 
      comment
    }

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

  let rec econd ?comment (b : t) (t : t) (f : t) : t = 
    match b.expression_desc , t.expression_desc, f.expression_desc with
    | Number ((Int { i = 0; _}) ), _, _ 
      -> f  (* TODO: constant folding: could be refined *)
    | (Number _ | Array _), _, _ 
      -> t  (* a block can not be false in OCAML, CF - relies on flow inference*)

    | ((Bin ((EqEqEq, {expression_desc = Number (Int { i = 0; _}); _},x)) 
       | Bin (EqEqEq, x,{expression_desc = Number (Int { i = 0; _});_}))), _, _ 
      -> 
      econd ?comment x f t 

    | (Bin (Ge, 
            ({expression_desc = 
                (String_length _ 
                | Array_length _ | Bytes_length _ | Function_length _ );
              _}), {expression_desc = Number (Int { i = 0 ; _})})), _, _ 
      -> f

    | (Bin (Gt, 
            ({expression_desc = 
                (String_length _ 
                | Array_length _ | Bytes_length _ | Function_length _ );
              _} as pred ), {expression_desc = Number (Int {i = 0; })})), _, _
      ->
      (** Add comment when simplified *)
      econd ?comment pred t f 
    | Not e, _, _ -> econd ?comment e f t 
    | Int_of_boolean  b, _, _  -> econd ?comment  b t f

    | _ -> 
      if Js_analyzer.eq_expression t f then
        if no_side_effect b then t else seq  ?comment b t
      else
        {expression_desc = Cond(b,t,f); comment}

  let int ?comment ?c  i : t = 
    {expression_desc = Number (Int {i; c}) ; comment}

  let access ?comment (e0 : t)  (e1 : t) : t = 
    match e0.expression_desc, e1.expression_desc with
    | Array (l,_mutable_flag) , Number (Int {i; _}) when no_side_effect e0-> 
      List.nth l  i  (* Float i -- should not appear here *)
    | _ ->
      { expression_desc = Access (e0,e1); comment} 

  let string_access ?comment (e0 : t)  (e1 : t) : t = 
    match e0.expression_desc, e1.expression_desc with
    | Str (_,s) , Number (Int {i; _}) when i >= 0 && i < String.length s -> 
      (* TODO: check exception when i is out of range..
         RangeError?
       *)
      str (String.make 1 s.[i])
    | _ ->
        { expression_desc = String_access (e0,e1); comment} 

  let index ?comment (e0 : t)  (e1 : int) : t = 
    match e0.expression_desc with
    | Array (l,_mutable_flag)  when no_side_effect e0 -> 
        List.nth l  e1  (* Float i -- should not appear here *)
    | _ -> { expression_desc = Access (e0, int e1); comment} 

  let call ?comment ?info e0 args : t = 
    let info = match info with 
      | None -> Js_call_info.dummy
      | Some x -> x in
    {expression_desc = Call(e0,args,info); comment }

  let flat_call ?comment e0 es : t = 
    (* TODO: optimization when es is known at compile time
        to be an array
     *)
    {expression_desc = FlatCall (e0,es); comment }

  (* Dot .....................**)        
  let runtime_call module_name fn_name args = 
    call ~info:{arity=Full} (runtime_var_dot  module_name fn_name) args

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
  (* This is a property access not external module *)
  
  let array_length ?comment (e : t) : t = 
    match e.expression_desc with 
      (* TODO: use array instead? *)
    | Array (l, _) -> int ?comment (List.length l)
    | _ -> { expression_desc = Array_length e ; comment }

  let string_length ?comment (e : t) : t =
    match e.expression_desc with 
    | Str(_,v) -> int ?comment (String.length v)
    | _ -> { expression_desc = String_length e ; comment }

  let bytes_length ?comment (e : t) : t = 
    match e.expression_desc with 
      (* TODO: use array instead? *)
    | Array (l, _) -> int ?comment (List.length l)
    | Str(_,v) -> int ?comment (String.length v)
    | _ -> { expression_desc = Bytes_length e ; comment }

  let function_length ?comment (e : t) : t = 
    match e.expression_desc with 
    | Fun(params, _, _) -> int ?comment (List.length params)
     (* TODO: optimize if [e] is know at compile time *)
    | _ -> { expression_desc = Function_length e ; comment }

      (** no dependency introduced *)
  let js_global_dot ?comment (x : string)  (e1 : string) : t = 
    { expression_desc = Dot (js_var x,  e1, true); comment} 

  let char_of_int ?comment (v : t) : t = 
    match v.expression_desc with
    | Number (Int {i; _}) ->
        str  (String.make 1(Char.chr i))
    | Char_to_int v -> v 
    | _ ->  {comment ; expression_desc = Char_of_int v}

  let char_to_int ?comment (v : t) : t = 
    match v.expression_desc with 
    | Str (_, x) ->
        assert (String.length x = 1) ;
        int ~comment:(Printf.sprintf "%S"  x )  
          (Char.code x.[0])
    | Char_of_int v -> v 
    | _ -> {comment; expression_desc = Char_to_int v }
      
  let array_append ?comment e el : t = 
    { comment ; expression_desc = Array_append (e, el)}

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
    | _, _ -> {comment ; expression_desc = String_append(e,el)}






  let float_mod ?comment e1 e2 : J.expression = 
    { comment ; 
      expression_desc = Bin (Mod, e1,e2)
    }
          
  let obj ?comment properties : t = 
    {expression_desc = Object properties; comment }

  let tag_ml_obj ?comment e : t = 
    {comment; expression_desc = Tag_ml_obj e  }

  (* currently only in method call, no dependency introduced
   *)
  let var_dot ?comment (x : Ident.t)  (e1 : string) : t = 
    {expression_desc = Dot (var x,  e1, true); comment} 

  (* Dot .....................**)        
    
  let float ?comment f : t = 
    {expression_desc = Number (Float {f}); comment}

  let zero_float_lit : t = 
    {expression_desc = Number (Float {f = "0." }); comment = None}

  (* let eqeq ?comment e0 e1 : t = {expression_desc = Bin(EqEq, e0,e1); comment} *)

  let assign ?comment e0 e1 : t = {expression_desc = Bin(Eq, e0,e1); comment}

  (** Convert a javascript boolean to ocaml boolean
      It's necessary for return value
       this should be optmized away for [if] ,[cond] to produce 
      more readable code
   *)         
  let to_ocaml_boolean ?comment (e : t) : t = 
    match e.expression_desc with 
    | Int_of_boolean _
    | Number _ -> e 
    | _ -> {comment ; expression_desc = Int_of_boolean e}

  let true_  = int ~comment:"true" 1 (* var (Jident.create_js "true") *)

  let false_  = int ~comment:"false" 0

  let bool v = if  v then true_ else false_

  let rec triple_equal ?comment (e0 : t) (e1 : t ) : t = 
    match e0.expression_desc, e1.expression_desc with
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
        to_ocaml_boolean  {expression_desc = Bin(EqEqEq, e0,e1); comment}

  let rec float_equal ?comment (e0 : t) (e1 : t) : t = 
    match e0.expression_desc, e1.expression_desc with     
    | Number (Int {i = i0 ; _}), Number (Int {i = i1; }) -> 
      bool (i0 = i1)
    | Number (Float {f = f0; _}), Number (Float {f = f1 ; }) when f0 = f1 -> 
      true_
    | _ -> 
      to_ocaml_boolean {expression_desc = Bin(EqEqEq, e0,e1); comment}     
  let rec string_equal ?comment (e0 : t) (e1 : t) : t = 
    match e0.expression_desc, e1.expression_desc with     
    | Str (_, a0), Str(_, b0) 
      -> bool  (Ext_string.equal a0 b0)
    | _ , _ 
      ->
      to_ocaml_boolean {expression_desc = Bin(EqEqEq, e0,e1); comment}     

  let bin ?comment (op : J.binop) e0 e1 : t = 
    match op with 
    | EqEqEq -> triple_equal ?comment e0 e1
    | _ -> {expression_desc = Bin(op,e0,e1); comment}

  let arr ?comment mt es : t  = 
    {expression_desc = Array (es,mt) ; comment}

  let uninitialized_array ?comment (e : t) : t  = 
    match e.expression_desc with 
    | Number (Int {i = 0 ; _}) -> arr ?comment NA []
    | _ -> {comment; expression_desc = Array_of_size e}



  (* Invariant: this is relevant to how we encode string
  *)           
  let typeof ?comment (e : t) : t = 
    match e.expression_desc with 
    | Number _ 
    | Array_length _ 
    | String_length _ 
      -> str ?comment "number"
    | Str _ 
      -> str ?comment "string" 

    | Array _
      -> str ?comment "object"
    | _ -> {expression_desc = Typeof e ; comment }

  let is_type_number ?comment (e : t) : t = 
    string_equal ?comment (typeof e) (str "number")    

  (* TODO remove this comment ? *)
  (* let un ?comment op e : t  = {expression_desc = Un(op,e); comment} *)

  (* return a value of type boolean *)
  (* TODO: 
       when comparison with Int
       it is right that !(x > 3 ) -> x <= 3 *)
  let rec not ({expression_desc; comment} as e : t) : t =
    match expression_desc with 
    | Bin(EqEqEq , e0,e1)
      -> {expression_desc = Bin(NotEqEq, e0,e1); comment}
    | Bin(NotEqEq , e0,e1) -> {expression_desc = Bin(EqEqEq, e0,e1); comment}

    (* Note here the compiled js use primtive comparison only 
       for *primitive types*, so it is safe to do such optimization,
       for generic comparison, this does not hold        
    *)
    | Bin(Lt, a, b) -> {e with expression_desc = Bin (Ge,a,b)}
    | Bin(Ge,a,b) -> {e with expression_desc = Bin (Lt,a,b)}          
    | Bin(Le,a,b) -> {e with expression_desc = Bin (Gt,a,b)}
    | Bin(Gt,a,b) -> {e with expression_desc = Bin (Le,a,b)}

    | Number (Int {i; _}) -> 
      if i != 0 then false_ else true_
    | Int_of_boolean  e -> not e
    | Not e -> e 
    | x -> {expression_desc = Not e ; comment = None}

  let new_ ?comment e0 args : t = 
    { expression_desc = New (e0,  Some args ); comment}

    (** cannot use [boolean] in js   *)
  let unknown_lambda ?(comment="unknown")  (lam : Lambda.lambda ) : t = 
       str ~pure:false ~comment (Lam_util.string_of_lambda lam)

  let unknown_primitive ?(comment="unknown") (p : Lambda.primitive) : t = 
    str ~pure:false ~comment (Lam_util.string_of_primitive p) 

  let unit  () = int ~comment:"()" 0;; (* TODO: add a comment *)

  let undefined ?comment () = js_global ?comment "undefined"

  let math ?comment v args  : t = 
    {comment ; expression_desc = Math(v,args)}

  (* handle comment *)

  let inc ?comment (e : t ) =
    match e with
    | {expression_desc = Number (Int ({i; _} as v));_ } -> 
        {e with expression_desc = Number (Int {v with i  = i + 1} )} (*comment ?*)
    | _ -> bin ?comment Plus e (int 1 )


  (* TODO: Constant folding, Google Closure will do that?,
     Even if Google Clsoure can do that, we will see how it interact with other
     optimizations
     We wrap all boolean functions here, since OCaml boolean is a 
     bit different from Javascript, so that we can change it in the future
   *)
  let rec and_ ?comment (e1 : t) (e2 : t) = 
    match e1, e2 with 
    | {expression_desc = Int_of_boolean e1;_} , 
      {expression_desc = Int_of_boolean e2;_} -> 
        and_ ?comment e1 e2
    | {expression_desc = Int_of_boolean e1; _} , 
      e2  -> and_ ?comment e1 e2
    | e1, {expression_desc = Int_of_boolean e2;_}
      -> and_ ?comment e1 e2
    | e1, e2 ->     
        to_ocaml_boolean @@ bin ?comment And e1 e2 

  let rec or_ ?comment (e1 : t) (e2 : t) = 
    match e1, e2 with 
    | {expression_desc = Int_of_boolean e1;_} , 
      {expression_desc = Int_of_boolean e2;_} -> 
        or_ ?comment e1 e2
    | {expression_desc = Int_of_boolean e1;_} , 
      e2  -> or_ ?comment e1 e2
    | e1, {expression_desc = Int_of_boolean e2;_}
      -> or_ ?comment e1 e2
    | e1, e2 ->     
        to_ocaml_boolean @@ bin ?comment Or e1 e2 

  let string_of_small_int_array ?comment xs : t = 
    {expression_desc = String_of_small_int_array xs; comment}
      

  
  let dec ?comment (e : t ) =
     match e with
     | {expression_desc = Number (Int ({i; _} as v));_ } -> 
         {e with expression_desc = Number (Int ({ v with i = i - 1 }))} (*comment ?*)
     | _ -> bin ?comment Minus e (int 1 )



  (* we are calling [Caml_primitive.primitive_name], since it's under our
     control, we should make it follow the javascript name convention, and
     call plain [dot]
   *)          

  let null ?comment () =     
    js_global ?comment "null"

  let tag ?comment e = index ?comment e 0

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
  let rec to_int32  ?comment (e : J.expression)  : J.expression = 
    let expression_desc =  e.expression_desc in
    match expression_desc  with 
    | Bin(Bor, a, {expression_desc = Number (Int {i = 0});  _})
      -> 
      to_int32 ?comment a
    | _ ->
      { comment ;
        expression_desc = Bin (Bor, {comment = None; expression_desc }, int 0)
      }

  let rec to_uint32 ?comment (e : J.expression)  : J.expression = 
    { comment ; 
      expression_desc = Bin (Lsr, e , int 0)
    }

  let string_comp cmp ?comment  e0 e1 = 
    to_ocaml_boolean @@ bin ?comment cmp e0 e1
  let int_comp cmp ?comment  e0 e1 = 
    to_ocaml_boolean @@ bin ?comment (Lam_compile_util.jsop_of_comp cmp) e0 e1
  let float_comp cmp ?comment  e0 e1 = 
    to_ocaml_boolean @@ bin ?comment (Lam_compile_util.jsop_of_comp cmp) e0 e1

  (* TODO: 
     we can apply a more general optimization here, 
     do some algebraic rewerite rules to rewrite [triple_equal]           
  *)        
  let is_out ?comment (e : t) (range : t) : t  = 
    begin match range.expression_desc, e.expression_desc with 

      | Number (Int {i = 1}), Var _ 
        ->         
        not (or_ (triple_equal e (int 0)) (triple_equal e (int 1)))                  
      | Number (Int {i = 1}), 
        (
         Bin (Plus , {expression_desc = Number (Int {i ; _}) }, {expression_desc = Var _; _})
       | Bin (Plus, {expression_desc = Var _; _}, {expression_desc = Number (Int {i ; _}) })
        ) 
        ->
        not (or_ (triple_equal e (int ( -i ))) (triple_equal e (int (1 - i))))        
      | Number (Int {i = 1}), 
        Bin (Minus ,  ({expression_desc = Var _; _} as x), {expression_desc = Number (Int {i ; _}) })        
        ->           
        not (or_ (triple_equal x (int ( i + 1 ))) (triple_equal x (int i)))        
      (* (x - i >>> 0 ) > k *)          
      | Number (Int {i = k}), 
        Bin (Minus ,  ({expression_desc = Var _; _} as x), 
             {expression_desc = Number (Int {i ; _}) })        
        ->           
        (or_ (int_comp Cgt x (int (i + k)))  (int_comp Clt x  (int i)))
      | Number (Int {i = k}), Var _  
        -> 
        (* Note that js support [ 1 < x < 3], 
           we can optimize it into [ not ( 0<= x <=  k)]           
        *)        
        or_ (int_comp Cgt e (int ( k)))  (int_comp Clt e  (int 0))

      | _, _ ->
        int_comp ?comment Cgt (to_uint32 e)  range 
    end

  let rec float_add ?comment (e1 : t) (e2 : t) = 
    match e1.expression_desc, e2.expression_desc with 
    | Number (Int {i;_}), Number (Int {i = j;_}) -> 
      int ?comment (i + j)
    | _, Number (Int {i = j; c}) when j < 0 -> 
      float_minus ?comment e1 {e2 with expression_desc = Number (Int {i = -j; c})}       

    | Bin(Plus, a1 , ({expression_desc = Number (Int {i = k; _})}  )), 
        Number (Int { i =j; _}) -> 
      bin ?comment Plus a1 (int (k + j))

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
    | _ -> 
      bin ?comment Plus e1 e2
  (* associative is error prone due to overflow *)
  and float_minus ?comment  (e1 : t) (e2 : t) : t = 
    match e1.expression_desc, e2.expression_desc with 
    | Number (Int {i;_}), Number (Int {i = j;_}) -> 
      int ?comment (i - j)
    | _ -> 
      bin ?comment Minus e1 e2




  let int32_add ?comment e1 e2 = 
    (* to_int32 @@  *)float_add ?comment e1 e2


  let int32_minus ?comment e1 e2 : J.expression = 
     (* to_int32 @@ *)  float_minus ?comment e1 e2

  let prefix_inc ?comment (i : J.vident)  = 
    let v : t = {expression_desc = Var i; comment = None} in
    assign ?comment  v (int32_add v (int 1))

  let prefix_dec ?comment i  = 
    let v : t = {expression_desc = Var i; comment = None} in
    assign ?comment v (int32_minus v (int 1))

  let float_mul ?comment e1 e2 = 
    bin ?comment Mul e1 e2 

  let float_div ?comment e1 e2 = 
    bin ?comment Div e1 e2 
  let float_notequal ?comment e1 e2 = 
    bin ?comment NotEqEq e1 e2

  let int32_div ?comment e1 e2 : J.expression = 
    to_int32 (float_div ?comment e1 e2)
    

  (* TODO: call primitive *)    
  let int32_mul ?comment e1 e2 : J.expression = 
    { comment ; 
      expression_desc = Bin (Mul, e1,e2)
    }
  

  (* TODO: check division by zero *)                
  let int32_mod ?comment e1 e2 : J.expression = 
    { comment ; 
      expression_desc = Bin (Mod, e1,e2)
    }

  let int32_lsl ?comment e1 e2 : J.expression = 
    { comment ; 
      expression_desc = Bin (Lsl, e1,e2)
    }

  (* TODO: optimization *)    
  let int32_lsr ?comment
      (e1 : J.expression) 
      (e2 : J.expression) : J.expression = 
    match e1.expression_desc, e2.expression_desc with
    | Number (Int { i = i1}), Number( Int {i = i2})
      ->
      int @@ Int32.to_int 
        (Int32.shift_right_logical
           (Int32.of_int i1) i2)
    | _ ,  Number( Int {i = i2})
      ->
        if i2 = 0 then 
          e1
        else 
          { comment ; 
            expression_desc = Bin (Lsr, e1,e2) (* uint32 *)
          }
    | _, _ ->
        to_int32  { comment ; 
                    expression_desc = Bin (Lsr, e1,e2) (* uint32 *)
                  }

  let int32_asr ?comment e1 e2 : J.expression = 
    { comment ; 
      expression_desc = Bin (Asr, e1,e2)
    }

  let int32_bxor ?comment e1 e2 : J.expression = 
    { comment ; 
      expression_desc = Bin (Bxor, e1,e2)
    }

  let rec int32_band ?comment (e1 : J.expression) (e2 : J.expression) : J.expression = 
    match e1.expression_desc with 
    | Bin (Bor ,a, {expression_desc = Number (Int {i = 0})})
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

  let int32_bor ?comment e1 e2 : J.expression = 
    { comment ; 
      expression_desc = Bin (Bor, e1,e2)
    }

  (* let int32_bin ?comment op e1 e2 : J.expression =  *)
  (*   {expression_desc = Int32_bin(op,e1, e2); comment} *)


    (* TODO -- alpha conversion 
        remember to add parens..
     *)
  let of_block ?comment block e : t = 
    call ~info:{arity=Full}
    {
     comment ;
     expression_desc = 
     Fun ([], (block @ [{J.statement_desc = Return {return_value = e } ;
                         comment}]) , Js_fun_env.empty 0)
   } []
end

module Stmt = struct 
  type t = J.statement 

  let return ?comment e : t = 
    {statement_desc = Return {return_value = e; } ; comment}

  let return_unit ?comment () : t =              
    return ?comment  (Exp.unit ())

  let break ?comment () : t = 
    {comment ; statement_desc = Break }

  let mk ?comment  statement_desc : t = 
    {statement_desc; comment}

  let empty ?comment  () : t = { statement_desc = Block []; comment}

  let throw ?comment v : t = { statement_desc = J.Throw v; comment}
  
  (* avoid nested block *)
  let  rec block ?comment  (b : J.block)   : t =  
    match b with 
    | [{statement_desc = Block bs }  ] -> block bs
    | [b] -> b
    | [] -> empty ?comment ()
    | _ -> {statement_desc = Block b  ; comment}
         
  (* It's a statement, we can discard some values *)       
  let rec exp ?comment (e : Exp.t) : t = 
    match e.expression_desc with 
    | (Seq( {expression_desc = Number _}, b) 
      | Seq( b, {expression_desc = Number _})) -> exp ?comment b 
    | Number _ -> block []
    (* TODO: we can do more *)      
    (* | _ when is_pure e ->  block [] *)
    |  _ -> 
        { statement_desc = Exp e; comment}

  let declare_variable ?comment  ?ident_info  ~kind (v:Ident.t)  : t=
    let property : J.property = 
      match (kind : Lambda.let_kind ) with 
      | (Alias | Strict | StrictOpt )
        -> Immutable 
      | Variable -> Mutable 
    in
    let ident_info  : J.ident_info  = 
      match ident_info with
      | None ->  {used_stats = NA}
      | Some x -> x in
    {statement_desc = 
     Variable { ident = v; value = None; property ; 
                ident_info ;};
     comment}

  let define ?comment  ?ident_info ~kind (v:Ident.t) exp    : t=
    let property : J.property = 
      match (kind : Lambda.let_kind ) with 
      | (Alias | Strict | StrictOpt )
        -> Immutable 
      | Variable -> Mutable 
    in
    let ident_info  : J.ident_info  = 
      match ident_info with
      | None ->  {used_stats = NA}
      | Some x -> x in
    {statement_desc = 
     Variable { ident = v; value =  Some exp; property ; 
                ident_info ;};
     comment}

  let int_switch ?comment   ?declaration ?default (e : J.expression)  clauses : t = 
    match e.expression_desc with 
    | Number (Int {i; _}) -> 
      let continuation =  
        begin match List.find (fun (x : int J.case_clause) -> x.case = i) clauses
          with 
          | case -> fst case.body
          | exception Not_found -> 
          begin match default with
            | Some x ->  x 
            | None -> assert false
          end 
        end in
      begin match declaration, continuation with 
        | Some (kind, did), 
          [ {statement_desc = Exp {expression_desc = Bin(Eq,  {expression_desc = Var (Id id) ; _}, e0); _}; _}]
          when Ident.same did id 
          -> 
          define ?comment ~kind id e0
        | Some(kind,did), _ 
          -> 
          block (declare_variable ?comment ~kind did :: continuation)
        | None, _ -> block continuation
      end

    | _ -> 
      match declaration with 
      | Some (kind, did) -> 
        block [declare_variable ?comment ~kind did ;
               { statement_desc = J.Int_switch (e,clauses, default); comment}]
      | None ->  { statement_desc = J.Int_switch (e,clauses, default); comment}

  let string_switch ?comment ?declaration  ?default (e : J.expression)  clauses : t= 
    match e.expression_desc with 
    | Str (_,s) -> 
      let continuation = 
        begin match List.find 
                      (fun  (x : string J.case_clause) -> x.case = s) clauses
          with 
          | case ->  (fst case.body)
          | exception Not_found -> 
          begin match default with 
            | Some x -> x 
            | None -> assert false 
          end
        end in
      begin match declaration, continuation with 
        | Some (kind, did),
          [ {statement_desc = Exp {expression_desc = Bin(Eq,  {expression_desc = Var (Id id); _}, e0);_} ; _}]
          when Ident.same did id 
          -> 
          define ?comment ~kind id e0
        | Some(kind,did), _ 
          -> 
          block @@ declare_variable ?comment ~kind did :: continuation
        | None, _ -> block continuation
      end
    | _  -> 
      match declaration with 
      | Some (kind,did) -> 
        block [declare_variable ?comment ~kind did ;
               { statement_desc = String_switch (e,clauses, default); comment}]
      | None -> { statement_desc = String_switch (e,clauses, default); comment}


  (* TODO: it also make sense  to extract some common statements 
      between those two branches, it does happen since in OCaml you 
      have to write some duplicated code due to the types system restriction
      example:
      {[
      | Format_subst (pad_opt, fmtty, rest) ->
        buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
        bprint_pad_opt buf pad_opt; buffer_add_char buf '(';
        bprint_fmtty buf fmtty; buffer_add_char buf '%'; buffer_add_char buf ')';
        fmtiter rest false;

      | Scan_char_set (width_opt, char_set, rest) ->
        buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
        bprint_pad_opt buf width_opt; bprint_char_set buf char_set;
        fmtiter rest false;
      ]}
   *)
  let rec if_ ?comment  ?declaration ?else_ (e : J.expression) (then_ : J.block)   : t = 
    let declared = ref false in
    let rec aux ?comment (e : J.expression) (then_ : J.block) (else_ : J.block ) acc   =
      match e.expression_desc, then_, (else_ : J.block ) with 
      | _, [ {statement_desc = Return {return_value = b; _}; _}], 
        [ {statement_desc = Return {return_value = a; _}; _}]
        ->
          return (Exp.econd e b a ) :: acc 
      | _,  [ {statement_desc = 
               Exp {expression_desc = Bin(Eq, ({expression_desc = Var (Id id0); _} as l0), a0); _}; _}], 
           [ {statement_desc = 
              Exp ({ expression_desc = Bin(Eq, 
                                           {expression_desc = Var (Id id1); _}, b0); _}); _}]
        when Ident.same id0 id1 -> 
          begin match declaration with 
          | Some (kind,did)  when Ident.same did id0 -> 
              declared := true;
              define ~kind id0 (Exp.econd e a0 b0) :: acc 
                                                       (* To hit this branch, we also need [declaration] passed down 
                                                          TODO: check how we compile [Lifthenelse]
                                                        *)
          | _ -> 
              exp (Exp.assign l0 (Exp.econd e a0 b0)) :: acc 
          end

      | _,  _,  
           [ {statement_desc = Exp {expression_desc = Number _}; _}]
        ->
          aux ?comment e then_ [] acc 
      | _, [ {statement_desc = Exp {expression_desc = Number _}; _}], _
        ->
          aux ?comment e [] else_ acc 

      | _,  [ {statement_desc = Exp b; _}],  [ {statement_desc = Exp a; _}]
        ->
          exp (Exp.econd e b a) :: acc 
      | _, [], []                                   
        -> exp e :: acc 

      | Not e, _ , _ :: _
        -> aux ?comment e else_ then_ acc
      | _, [], _
        ->
          aux ?comment (Exp.not e) else_ [] acc
      (* Be careful that this re-write may result in non-terminating effect *)
      | _, (y::ys),  (x::xs)
        when Js_analyzer.(eq_statement x y && no_side_effect e)
        ->
          (** here we do agressive optimization, because it can help optimization later,
              move code outside of branch is generally helpful later
           *)
          aux ?comment e ys xs (y::acc)

      |  Number ( Int { i = 0; _}) , _,  _
        ->  
          begin match else_ with 
          | [] -> acc 
          | _ -> block else_ ::acc
          end
      |  (Number _ , _, _
      | (Bin (Ge, 
              ({expression_desc = (String_length _ | Array_length _ | Bytes_length _ | Function_length _ );
               _}), {expression_desc = Number (Int { i = 0; _})})), _ , _)
            (* TODO: always 
                turn [Le] -> into [Ge]
             *)
      -> block then_ :: acc 

    | (
       (Bin ((EqEqEq, {expression_desc = Number (Int {i = 0; _}); _},e)) |
       Bin (EqEqEq, e,{expression_desc = Number (Int {i = 0; _});_}))
     ),  _,  else_ 
          (* TODO: optimize in general of preciate information based on type system 
              like: [if_], [econd]
           *)
      ->
        aux ?comment e else_  then_ acc 

    | ((Bin (Gt, 
             ({expression_desc = 
                 (String_length _ 
                 | Array_length _ | Bytes_length _ | Function_length _ );
               _} as e ), {expression_desc = Number (Int { i = 0; _})}))

      | Int_of_boolean e), _ , _
      ->
      (** Add comment when simplified *)
      aux ?comment e then_ else_ acc 

    | _ -> 
      { statement_desc = If (e, 
                             then_,
                             (match else_ with 
                              | [] -> None
                              |  v -> Some  v)); 
        comment } :: acc in
    let if_block = 
      aux ?comment e then_ (match else_ with None -> [] | Some v -> v) [] in

    match !declared, declaration with 
    | true , _ 
    | _    , None  ->  block (List.rev if_block)
    | false, Some (kind, did) -> block (declare_variable ~kind did :: List.rev if_block )



  let const_variable ?comment  ?exp (v:Ident.t)  : t=
    {statement_desc = 
     Variable {
     ident = v; value = exp; property = Immutable;
     ident_info = {used_stats = NA }   };
     comment}

  let assign ?comment  id e : t = 
    {
      statement_desc = J.Exp ( (Exp.bin Eq (Exp.var id) e)) ;
      comment
    }
  let assign_unit ?comment  id :  t = 
    {
      statement_desc = J.Exp(Exp.bin Eq (Exp.var id) (Exp.unit ()));
      comment
    }
  let declare_unit ?comment  id :  t = 
    {
      statement_desc = 
      J.Variable { ident =  id; 
                   value = Some (Exp.unit ()) ;
                   property = Mutable;
                   ident_info = {used_stats = NA}
                 };
      comment
    }

  let rec while_  ?comment  ?label ?env (e : Exp.t) (st : J.block) : t = 
    match e with 
    | {expression_desc = Int_of_boolean e; _} -> 
      while_ ?comment  ?label  e st
    | _ -> 
      let env = 
        match env with 
        | None -> Js_closure.empty ()
        | Some x -> x in
      {
        statement_desc = While (label, e, st, env);
        comment
      }

  let for_ ?comment   ?env 
      for_ident_expression
      finish_ident_expression id direction (b : J.block) : t =
    let env = 
      match env with 
      | None -> Js_closure.empty ()
      | Some x -> x 
    in
    {
      statement_desc = 
        ForRange (for_ident_expression, finish_ident_expression, id, direction, b, env);
      comment
    }

  let try_ ?comment   ?with_ ?finally body : t = 
    {
      statement_desc = Try (body, with_, finally) ;
      comment
    }

  let unknown_lambda ?(comment="unknown")  (lam : Lambda.lambda ) : t = 
    exp @@ Exp.str ~comment ~pure:false (Lam_util.string_of_lambda lam) 
  
  (* TODO: 
      actually, only loops can be labelled
   *)    
  let continue  ?comment   label : t = 
    { 
      statement_desc = J.Continue  label;
      comment;
    }
end
