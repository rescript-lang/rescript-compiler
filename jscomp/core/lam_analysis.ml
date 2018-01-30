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




(**used in effect analysis, it is sound but not-complete *)
let not_zero_constant ( x : Lam.constant) =  
  match x with 
  | Const_int i  -> i <> 0
  | Const_int32 i  -> i <> 0l
  | Const_int64 i  -> i <> 0L
  | Const_nativeint i -> i <> 0n
  | _ -> false 

let rec no_side_effects (lam : Lam.t) : bool = 
  match lam with 
  | Lvar _ 
  | Lconst _ 
  | Lfunction _ -> true
  | Lam.Lglobal_module _ -> true 
    (* we record side effect in the global level, 
      this expression itself is side effect free
    *)
  | Lprim {primitive;  args; _} -> 
    List.for_all no_side_effects args && 
    (
      match primitive with 
      | Pccall {prim_name ; _} ->
        begin 
          match prim_name,args with 
          | ("caml_register_named_value"
            (* register to c runtime does not make sense  in ocaml *)
            (* | "caml_set_oo_id"  *) (* it does have side effect, just in creation path it happens not to have *)
            | "caml_is_js"
            | "caml_int64_float_of_bits"
             (* more safe to check if arguments are constant *)
            (* non-observable side effect *)    
            | "caml_sys_get_config"
            | "caml_sys_get_argv" (* should be fine *)

            | "caml_create_string" (* TODO: add more *)
            | "caml_make_vect"
            | "caml_obj_dup"
            | "caml_array_dup"
            | "caml_obj_block"
            ), _  -> true 
          | "caml_ml_open_descriptor_in", [Lconst (  (Const_int 0))] -> true 
          | "caml_ml_open_descriptor_out", 
            [Lconst (  (Const_int (1|2))) ]
            -> true
          (* we can not mark it pure
             only when we guarantee this exception is caught...
           *)
          | _ , _-> false
        end 
      | Pmodint
      | Pdivint 
      | Pdivbint _
      | Pmodbint _ 
        -> begin match args with 
          | [_ ; Lconst cst ] -> not_zero_constant cst 
          | _ -> false 
        end
      
      | Pcreate_extension _
      (* | Pcreate_exception _ *)
      | Pjs_boolean_to_bool
      | Pjs_typeof
      | Pis_null
      | Pis_undefined
      | Pis_null_undefined
      | Pnull_to_opt       
      | Pundefined_to_opt         
      | Pnull_undefined_to_opt 
      | Pjs_fn_make _         
      | Pjs_object_create _
        (** TODO: check *)      
      | Pbytes_to_string 
      | Pbytes_of_string 
      | Pglobal_exception _
      | Pmakeblock _  (* whether it's mutable or not *)
      | Pfield _
      | Pfloatfield _ 
      | Pduprecord _ 
      (* Boolean operations *)
      | Psequand | Psequor | Pnot
      (* Integer operations *)
      | Pnegint | Paddint | Psubint | Pmulint 
     
      | Pandint | Porint | Pxorint
      | Plslint | Plsrint | Pasrint
      | Pintcomp _ 
      (* Float operations *)
      | Pintoffloat | Pfloatofint
      | Pnegfloat | Pabsfloat
      | Paddfloat | Psubfloat | Pmulfloat 
      | Pdivfloat
      | Pfloatcomp _ 
      | Pjscomp _
      (* String operations *)
      | Pstringlength 
      | Pstringrefu 
      | Pstringrefs
      | Pbyteslength
      | Pbytesrefu
      | Pbytesrefs
      | Pmakearray _ 
      | Parraylength _ 
      | Parrayrefu _
      | Parrayrefs _ 
      (* Test if the argument is a block or an immediate integer *)
      | Pisint
      (* Test if the (integer) argument is outside an interval *)
      | Pisout
      | Pbintofint _
      | Pintofbint _
      | Pcvtbint _
      | Pnegbint _
      | Paddbint _
      | Psubbint _
      | Pmulbint _
      | Pandbint _
      | Porbint _
      | Pxorbint _
      | Plslbint _
      | Plsrbint _
      | Pasrbint _
      | Pbintcomp _
      (* Operations on big arrays: (unsafe, #dimensions, kind, layout) *)
      | Pbigarrayref _ (* TODO it may raise an exception....*)
      (* Compile time constants *)
      | Pctconst _
      (* Integer to external pointer *)

      | Poffsetint _
      | Pstringadd 
      | Pjs_function_length
      | Pcaml_obj_length
      | Pjs_is_instance_array
      | Pwrap_exn
        -> true
      | Pjs_string_of_small_array
      | Pcaml_obj_set_length        
      | Pjs_apply
      | Pjs_runtime_apply
      | Pjs_call _ 
      | Pinit_mod
      | Pupdate_mod
      | Pjs_unsafe_downgrade _
      | Pdebugger 
      | Pjs_fn_run _ 
      | Pjs_fn_method _ | Pjs_fn_runmethod _
      (* TODO *)
      | Praw_js_code_exp _ 
      | Praw_js_code_stmt _
      | Pbytessetu 
      | Pbytessets
      (* Bitvect operations *)
      | Pbittest
      (* Operations on boxed integers (Nativeint.t, Int32.t, Int64.t) *)
      | Parraysets _
      | Pbigarrayset _
      (* size of the nth dimension of a big array *)
      | Pbigarraydim _
      (* load/set 16,32,64 bits from a string: (unsafe)*)
      | Pstring_load_16 _
      | Pstring_load_32 _
      | Pstring_load_64 _
      | Pstring_set_16 _
      | Pstring_set_32 _
      | Pstring_set_64 _
      (* load/set 16,32,64 bits from a
         (char, int8_unsigned_elt, c_layout) Bigarray.Array1.t : (unsafe) *)
      | Pbigstring_load_16 _
      | Pbigstring_load_32 _
      | Pbigstring_load_64 _
      | Pbigstring_set_16 _
      | Pbigstring_set_32 _
      | Pbigstring_set_64 _
      (* byte swap *)
      | Pbswap16
      | Pbbswap _
      | Parraysetu _ 
      | Poffsetref _ 
      | Praise
      | Plazyforce 
      | Psetfield _ 
      | Psetfloatfield _
      (* | Psetglobal _  *)
        -> false 
    )
  | Llet (_,_, arg,body) -> no_side_effects arg && no_side_effects body 
  | Lswitch (_,_) -> false 
  | Lstringswitch (_,_,_) -> false
  | Lstaticraise _ -> false
  | Lstaticcatch _ -> false 

  (* | "caml_sys_getenv" , [Lconst( (Const_string _))] *)
  (*         -> true *)
  (** not enough, we need know that 
      if it [Not_found], there are no other exceptions 
      can be thrown
  *)
  | Ltrywith (Lprim { primitive = Pccall{prim_name = "caml_sys_getenv"};
                    args = [Lconst _]; _},exn,
              Lifthenelse(Lprim{args =  
                                  [Lvar exn1; 
                                   Lprim {primitive = Pglobal_exception ({name="Not_found"}); args = []; _}]
                               ; _},
                          then_, _)) when Ident.same exn1 exn
    (** we might put this in an optimization pass 
        also make sure when we wrap this in [js] we 
        should follow the same patten, raise [Not_found] 
    *)
    -> no_side_effects then_
  (** It would be nice that we can also analysis some small functions 
      for example [String.contains], 
      [Format.make_queue_elem]
  *)
  | Ltrywith (body,exn,handler) 
    -> no_side_effects body && no_side_effects handler

  | Lifthenelse  (a,b,c) -> 
    no_side_effects a && no_side_effects b && no_side_effects c
  | Lsequence (a,b) -> no_side_effects a && no_side_effects b
  | Lletrec (bindings, body) ->
    List.for_all (fun (_,b) -> no_side_effects b) bindings && no_side_effects body
  | Lwhile _ -> false (* conservative here, non-terminating loop does have side effect *)
  | Lfor _ -> false 
  | Lassign _ -> false (* actually it depends ... *)
  | Lsend _ -> false 
  | Lifused _ -> false 
  | Lapply _ -> false (* we need purity analysis .. *)
  


(* 
    Estimate the size of lambda for better inlining 
    threshold is 1000 - so that we 
 *)
exception Too_big_to_inline

let really_big () = raise Too_big_to_inline

let big_lambda = 1000

let rec size (lam : Lam.t) = 
  try 
    match lam with 
    | Lvar _ ->  1
    | Lconst c -> size_constant c
    | Llet(_, _, l1, l2) -> 1 + size l1 + size l2 
    | Lletrec _ -> really_big ()
    | Lprim{primitive = Pfield _; 
            args =  [ Lglobal_module _]
           ;  _}
      -> 1
    | Lprim {primitive = Praise ; args =  [l ];  _} 
      -> size l
    | Lam.Lglobal_module _ -> 1       
    | Lprim {args = ll; _} -> size_lams 1 ll

    (** complicated 
        1. inline this function
        2. ...
        exports.Make=
        function(funarg)
        {var $$let=Make(funarg);
        return [0, $$let[5],... $$let[16]]}
     *)      
    | Lapply{ fn;
             args; _} -> size_lams (size fn) args
    (* | Lfunction(_, params, l) -> really_big () *)
    | Lfunction {body} -> size body 
    | Lswitch(_, _) -> really_big ()
    | Lstringswitch(_,_,_) -> really_big ()
    | Lstaticraise (i,ls) -> 
        List.fold_left (fun acc x -> size x + acc) 1 ls 
    | Lstaticcatch(l1, (i,x), l2) -> really_big () 
    | Ltrywith(l1, v, l2) -> really_big ()
    | Lifthenelse(l1, l2, l3) -> 1 + size  l1 + size  l2 +  size  l3
    | Lsequence(l1, l2) -> size  l1  +  size  l2
    | Lwhile(l1, l2) -> really_big ()
    | Lfor(flag, l1, l2, dir, l3) -> really_big () 
    | Lassign (_,v) -> 1 + size v  (* This is side effectful,  be careful *)
    | Lsend _  ->  really_big ()
    | Lifused(v, l) -> size l 
  with Too_big_to_inline ->  1000 
and size_constant x = 
  match x with 
  | Const_int _ | Const_char _ 
  | Const_string _  
  | Const_unicode _
  | Const_float _  | Const_int32 _ | Const_int64 _ 
  | Const_nativeint _ 
  | Const_immstring _
  | Const_pointer _ 
  | Const_js_null | Const_js_undefined
  | Const_js_true | Const_js_false
    -> 1 
  | Const_block (_, _, str) 
    ->  List.fold_left (fun acc x -> acc + size_constant x ) 0 str
  | Const_float_array xs  -> List.length xs

and size_lams acc (lams : Lam.t list) = 
  List.fold_left (fun acc l -> acc  + size l ) acc lams
let args_all_const args =
  List.for_all (fun x -> match x with Lam.Lconst _ -> true | _ -> false) args
    
let exit_inline_size = 7 
let small_inline_size = 5

(** destruct pattern will work better 
    if it is closed lambda, otherwise
    you can not do full evaluation

    We still should avoid inline too big code, 

    ideally we should also evaluate its size after inlining, 
    since after partial evaluation, it might still be *very big*
*)
let destruct_pattern (body : Lam.t) params args =
  let rec aux v params args =
    match params, args with
    | x::xs, b::bs ->
      if Ident.same x v then Some b
      else aux v xs bs
    | [] , _ -> None
    | x::xs, [] -> assert false                  
  in   
  match body with
  | Lswitch (Lvar v , switch)
    ->
    begin match aux v params args with
      | Some (Lam.Lconst _ as lam) ->
        size (Lam.switch lam switch) < small_inline_size
      | Some _ | None -> false
    end        
  | Lifthenelse(Lvar v, then_, else_)
    ->
    begin match aux v params args with
      | Some (Lconst _ as lam) ->
        size (Lam.if_ lam then_ else_) < small_inline_size
      | Some _ | None -> false          
    end      
  | _ -> false
    
(** Hints to inlining *)
let ok_to_inline_fun_when_app ~body params args =
  let s = size body in
  s < small_inline_size ||
  (destruct_pattern body params args) ||  
  (args_all_const args &&
   (s < 10 && no_side_effects body )) 


let eq_comparison ( p : Lam.comparison) (p1:Lam.comparison) = 
  match p with 
  | Cge -> p1 =  Cge
  | Cgt -> p1 =  Cgt
  | Cle -> p1 =  Cle
  | Clt -> p1 =  Clt 
  | Ceq -> p1 =  Ceq 
  | Cneq -> p1 =  Cneq 

let eq_array_kind (p : Lam.array_kind) (p1 : Lam.array_kind) = 
  match p with 
  | Pgenarray -> p1 = Pgenarray
  | Paddrarray -> p1 = Paddrarray 
  | Pintarray -> p1 = Pintarray
  | Pfloatarray -> p1 = Pfloatarray 

let eq_boxed_integer (p : Lam.boxed_integer) (p1 : Lam.boxed_integer ) = 
  match p with 
  | Pnativeint -> p1 = Pnativeint 
  | Pint32 -> p1 = Pint32
  | Pint64 -> p1 = Pint64

let eq_bigarray_kind (p : Lam.bigarray_kind) (p1 : Lam.bigarray_kind) = 
  match p with   
  | Pbigarray_unknown -> p1 = Pbigarray_unknown
  | Pbigarray_float32 -> p1 = Pbigarray_float32
  | Pbigarray_float64 -> p1 =  Pbigarray_float64
  | Pbigarray_sint8 -> p1 = Pbigarray_sint8
  | Pbigarray_uint8 -> p1 = Pbigarray_uint8
  | Pbigarray_sint16 -> p1 = Pbigarray_sint16 
  | Pbigarray_uint16 -> p1 = Pbigarray_uint16
  | Pbigarray_int32  -> p1 = Pbigarray_int32
  | Pbigarray_int64 -> p1 = Pbigarray_int64
  | Pbigarray_caml_int -> p1 = Pbigarray_caml_int
  | Pbigarray_native_int -> p1 = Pbigarray_native_int
  | Pbigarray_complex32  -> p1 = Pbigarray_complex32
  | Pbigarray_complex64 -> p1 = Pbigarray_complex64

let eq_bigarray_layout (p : Lam.bigarray_layout) (p1 : Lam.bigarray_layout) = 
  match p with 
  | Pbigarray_unknown_layout -> p1 = Pbigarray_unknown_layout
  | Pbigarray_c_layout -> p1 = Pbigarray_c_layout
  | Pbigarray_fortran_layout -> p1 = Pbigarray_fortran_layout

let eq_compile_time_constant ( p : Lam.compile_time_constant) (p1 : Lam.compile_time_constant) = 
  match p with 
  | Big_endian -> p1 = Big_endian
  | Word_size -> p1 = Word_size 
  | Ostype_unix -> p1 = Ostype_unix
  | Ostype_win32 -> p1 = Ostype_win32
  | Ostype_cygwin -> p1 = Ostype_cygwin 

let eq_record_representation ( p : Types.record_representation) ( p1 : Types.record_representation) = 
  match p with 
  | Record_float -> p1 = Record_float
  | Record_regular -> p1 = Record_regular
(* compared two lambdas in case analysis, note that we only compare some small lambdas
    Actually this patten is quite common in GADT, people have to write duplicated code 
    due to the type system restriction
*)
let rec 
  eq_lambda (l1 : Lam.t) (l2 : Lam.t) =
  match l1 with 
  | Lglobal_module i1 -> 
    begin match l2 with  Lglobal_module i2 -> Ident.same i1  i2  | _ -> false end
  | Lvar i1 -> 
    begin match l2 with  Lvar i2 ->  Ident.same i1 i2 | _ -> false end 
  | Lconst c1 -> 
    begin match l2 with  Lconst c2 -> c1 = c2 (* FIXME *) | _ -> false end 
  | Lapply {fn = l1; args = args1; _} -> 
    begin match l2 with Lapply {fn = l2; args = args2; _} ->
    eq_lambda l1 l2  && Ext_list.for_all2_no_exn eq_lambda args1 args2
    |_ -> false end 
  | Lifthenelse (a,b,c) -> 
    begin match l2 with  Lifthenelse (a0,b0,c0) ->
    eq_lambda a a0 && eq_lambda b b0 && eq_lambda c c0
    | _ -> false end 
  | Lsequence (a,b) -> 
    begin match l2 with Lsequence (a0,b0) ->
    eq_lambda a a0 && eq_lambda b b0
    | _ -> false end 
  | Lwhile (p,b) -> 
    begin match l2 with  Lwhile (p0,b0) -> eq_lambda p p0 && eq_lambda b b0
    | _ -> false end   
  | Lassign(v0,l0) -> 
    begin match l2 with  Lassign(v1,l1) -> Ident.same v0 v1 && eq_lambda l0 l1
    | _ -> false end 
  | Lstaticraise(id,ls) -> 
    begin match l2 with  Lstaticraise(id1,ls1) -> 
    (id : int) = id1 && Ext_list.for_all2_no_exn eq_lambda ls ls1 
    | _ -> false end 
  | Lprim {primitive = p; args = ls; } -> 
    begin match l2 with 
    Lprim {primitive = p1; args = ls1} ->
    eq_primitive p p1 && Ext_list.for_all2_no_exn eq_lambda ls ls1
    | _ -> false end 
  | Lfunction _  
  | Llet (_,_,_,_)
  | Lletrec _
  | Lswitch _ 
  | Lstringswitch _ 
  | Lstaticcatch _ 
  | Ltrywith _ 
  | Lfor (_,_,_,_,_) 
  | Lsend _
  | Lifused _ -> false    

  
and eq_primitive ( lhs : Lam.primitive) (rhs : Lam.primitive) = 
  match lhs with 
  | Pcreate_extension a -> begin match rhs with Pcreate_extension b -> a = (b : string) | _ -> false end
  (* | Pcreate_exception a -> begin match rhs with Pcreate_exception b -> a = (b : string) | _ -> false end *)
  | Pwrap_exn -> rhs = Pwrap_exn
  | Pbytes_to_string ->  rhs = Pbytes_to_string 
  | Pbytes_of_string ->  rhs = Pbytes_of_string
  | Praise -> rhs = Praise
  | Psequand -> rhs = Psequand
  | Psequor -> rhs = Psequor 
  | Pnot -> rhs = Pnot 
  | Pnegint -> rhs = Pnegint
  | Paddint -> rhs = Paddint 
  | Psubint -> rhs = Psubint
  | Pmulint -> rhs = Pmulint
  | Pdivint -> rhs = Pdivint
  | Pmodint -> rhs = Pmodint 
  | Pandint -> rhs = Pandint
  | Porint  -> rhs = Porint
  | Pxorint -> rhs = Pxorint
  | Plslint -> rhs = Plslint
  | Plsrint -> rhs = Plsrint
  | Pasrint -> rhs = Pasrint      
  | Plazyforce -> rhs = Plazyforce
  | Pintoffloat -> rhs = Pintoffloat
  | Pfloatofint -> rhs = Pfloatofint
  | Pnegfloat -> rhs =  Pnegfloat
  | Pabsfloat -> rhs = Pabsfloat
  | Paddfloat -> rhs = Paddfloat
  | Psubfloat -> rhs = Psubfloat
  | Pmulfloat -> rhs = Pmulfloat
  | Pdivfloat -> rhs = Pdivfloat
  | Pjs_apply -> rhs = Pjs_apply
  | Pjs_runtime_apply -> rhs = Pjs_runtime_apply
  | Pstringlength ->  rhs = Pstringlength
  | Pstringrefu ->  rhs = Pstringrefu
  | Pstringrefs ->  rhs = Pstringrefs
  | Pstringadd  ->  rhs = Pstringadd   
  | Pbyteslength -> rhs = Pbyteslength
  | Pbytesrefu ->   rhs = Pbytesrefu
  | Pbytessetu ->   rhs = Pbytessetu
  | Pbytesrefs ->   rhs = Pbytesrefs
  | Pbytessets ->   rhs = Pbytessets  
  | Pundefined_to_opt -> rhs = Pundefined_to_opt
  | Pnull_to_opt -> rhs = Pnull_to_opt
  | Pnull_undefined_to_opt -> rhs = Pnull_undefined_to_opt  
  | Pis_null -> rhs = Pis_null
  | Pis_undefined -> rhs = Pis_undefined
  | Pis_null_undefined -> rhs = Pis_null_undefined
  | Pjs_boolean_to_bool -> rhs = Pjs_boolean_to_bool
  | Pjs_typeof -> rhs = Pjs_typeof
  | Pisint -> rhs = Pisint
  | Pisout -> rhs = Pisout
  | Pbittest -> rhs = Pbittest
  | Pdebugger -> rhs = Pdebugger    
  | Pinit_mod -> rhs = Pinit_mod
  | Pupdate_mod -> rhs = Pupdate_mod
  | Pbswap16 -> rhs = Pbswap16
  | Pjs_function_length -> rhs = Pjs_function_length
  | Pjs_string_of_small_array -> rhs = Pjs_string_of_small_array
  | Pjs_is_instance_array -> rhs = Pjs_is_instance_array
  | Pcaml_obj_length -> rhs = Pcaml_obj_length
  | Pcaml_obj_set_length -> rhs = Pcaml_obj_set_length
  | Pccall {prim_name = n0 ;  prim_native_name = nn0} ->  (match rhs with Pccall {prim_name = n1; prim_native_name = nn1} ->    n0 = n1 && nn0 = nn1 | _ -> false )    
  | Pfield (n0, _dbg_info0) ->  (match rhs with Pfield (n1, _dbg_info1) ->  n0 = n1  | _ -> false )    
  | Psetfield(i0, b0, _dbg_info0) -> (match rhs with Psetfield(i1, b1, _dbg_info1) ->  i0 = i1 && b0 = b1 | _ -> false)
  | Pglobal_exception ident -> (match rhs with Pglobal_exception ident2 ->  Ident.same ident ident2 | _ -> false )
  | Pmakeblock (i, _tag_info, mutable_flag) -> (match rhs with Pmakeblock(i1,_,mutable_flag1) ->  i = i1 && mutable_flag = mutable_flag1  | _ -> false)
  | Pfloatfield (i0,_dbg_info) -> (match rhs with Pfloatfield (i1,_) -> i0 = i1   | _ -> false)
  | Psetfloatfield (i0,_dbg_info) ->  (match rhs with Psetfloatfield(i1,_) -> i0 = i1  | _ -> false)
  | Pduprecord (record_repesentation0,i1) -> (match rhs with Pduprecord(record_repesentation1,i2) ->  eq_record_representation record_repesentation0 record_repesentation1 && i1 = i2    | _ -> false)
  | Pjs_call (prim_name, arg_types, ffi) ->  ( match rhs with Pjs_call(prim_name1, arg_types1,ffi1) -> prim_name = prim_name1 && arg_types = arg_types1 && ffi = ffi1 | _ -> false)
  | Pjs_object_create obj_create -> (match rhs with Pjs_object_create obj_create1 -> obj_create = obj_create1 | _ -> false )
  | Pintcomp comparison -> (match rhs with Pintcomp comparison1 -> eq_comparison comparison  comparison1  | _ -> false )    
  | Pfloatcomp comparison -> (match rhs with Pfloatcomp comparison1 -> eq_comparison comparison  comparison1 | _ -> false)
  | Pjscomp comparison ->  (match rhs with  Pjscomp comparison1 -> eq_comparison comparison  comparison1  | _ -> false )    
  | Poffsetint i0 ->   (match rhs with  Poffsetint i1 -> i0 = i1 | _ -> false )   
  | Poffsetref i0 ->  (match rhs with Poffsetref i1 -> i0 = i1   | _ -> false)
  | Pmakearray array_kind -> (match rhs with Pmakearray array_kind1 -> eq_array_kind array_kind array_kind1 | _ -> false  )
  | Parraylength  array_kind -> (match rhs with Parraylength array_kind1 -> eq_array_kind array_kind array_kind1 | _ -> false  )
  | Parrayrefu  array_kind -> (match rhs with Parrayrefu array_kind1 -> eq_array_kind array_kind array_kind1 | _ -> false  )
  | Parraysetu  array_kind -> (match rhs with Parraysetu array_kind1 -> eq_array_kind array_kind array_kind1 | _ -> false  ) 
  | Parrayrefs array_kind -> (match rhs with Parrayrefs array_kind1 -> eq_array_kind array_kind array_kind1 | _ -> false  )
  | Parraysets  array_kind -> (match rhs with Parraysets array_kind1 -> eq_array_kind array_kind array_kind1 | _ -> false  )  
  | Pbintofint  boxed_integer -> (match rhs with Pbintofint boxed_integer1 -> eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Pintofbint  boxed_integer -> (match rhs with Pintofbint boxed_integer1 -> eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Pnegbint  boxed_integer -> (match rhs with Pnegbint boxed_integer1 -> eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Paddbint  boxed_integer -> (match rhs with Paddbint boxed_integer1 -> eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Psubbint  boxed_integer -> (match rhs with Psubbint boxed_integer1 -> eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Pmulbint  boxed_integer -> (match rhs with Pmulbint boxed_integer1 -> eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Pdivbint  boxed_integer -> (match rhs with Pdivbint boxed_integer1 -> eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Pmodbint  boxed_integer -> (match rhs with Pmodbint boxed_integer1 -> eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Pandbint  boxed_integer -> (match rhs with Pandbint boxed_integer1 -> eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Porbint boxed_integer ->   (match rhs with Porbint  boxed_integer1 -> eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Pxorbint  boxed_integer -> (match rhs with Pxorbint boxed_integer1 -> eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Plslbint  boxed_integer -> (match rhs with Plslbint boxed_integer1 -> eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Plsrbint  boxed_integer -> (match rhs with Plsrbint boxed_integer1 -> eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Pasrbint  boxed_integer -> (match rhs with Pasrbint boxed_integer1 -> eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Pbbswap boxed_integer ->   (match rhs with Pbbswap boxed_integer1  -> eq_boxed_integer boxed_integer boxed_integer1 | _ -> false )
  | Pcvtbint  (boxed_integer, boxed_integer1) -> (match rhs with Pcvtbint (boxed_integer10, boxed_integer11) -> eq_boxed_integer boxed_integer boxed_integer10 && eq_boxed_integer boxed_integer1 boxed_integer11 | _ -> false )
  | Pbintcomp  (boxed_integer , comparison) -> (match rhs with Pbintcomp(boxed_integer1, comparison1) -> eq_boxed_integer boxed_integer boxed_integer1 && eq_comparison comparison comparison1 | _ -> false)  
  | Pbigarraydim dim -> (match rhs with Pbigarraydim dim1 -> dim = dim1 | _ -> false )
  | Pstring_load_16 str ->  (match  rhs with Pstring_load_16 str1 -> str = str1  | _ -> false )
  | Pstring_load_32 b -> (match rhs with Pstring_load_32 b1 -> b = b1 | _ -> false )    
  | Pstring_load_64 b -> (match rhs with Pstring_load_64 b1 -> b = b1 | _ -> false )    
  | Pstring_set_16 b -> (match rhs with Pstring_set_16 b1 -> b = b1 | _ -> false )    
  | Pstring_set_32 b -> (match rhs with Pstring_set_32 b1 -> b = b1 | _ -> false )    
  | Pstring_set_64 b -> (match rhs with Pstring_set_64 b1 -> b = b1 | _ -> false )      
  | Pbigstring_load_16 b -> (match rhs with Pbigstring_load_16 b1 -> b = b1 | _ -> false )      
  | Pbigstring_load_32 b -> (match rhs with Pbigstring_load_32 b1 -> b = b1 | _ -> false )      
  | Pbigstring_load_64 b -> (match rhs with Pbigstring_load_64 b1 -> b = b1 | _ -> false )      
  | Pbigstring_set_16 b -> (match rhs with Pbigstring_set_16 b1 -> b = b1 | _ -> false )      
  | Pbigstring_set_32 b -> (match rhs with Pbigstring_set_32 b1 -> b = b1 | _ -> false )      
  | Pbigstring_set_64 b -> (match rhs with Pbigstring_set_64 b1 -> b = b1 | _ -> false )      
  | Pctconst compile_time_constant -> (match rhs with Pctconst compile_time_constant1 -> eq_compile_time_constant compile_time_constant compile_time_constant1 | _ -> false)
  | Pjs_unsafe_downgrade ( s,_loc) -> (match rhs with Pjs_unsafe_downgrade (s1,_) -> s = s1 | _ -> false)  
  | Pjs_fn_make i -> (match rhs with Pjs_fn_make i1 -> i = i1 | _ -> false)
  | Pjs_fn_run i -> (match rhs with Pjs_fn_run i1 -> i = i1 | _ -> false)
  | Pjs_fn_method i -> (match rhs with Pjs_fn_method i1 -> i = i1 | _ ->  false )
  | Pjs_fn_runmethod i -> (match rhs with Pjs_fn_runmethod i1 -> i = i1 | _ -> false ) 

  | Pbigarrayref  _ 
  | Pbigarrayset _ 
  | Praw_js_code_exp _ 
  | Praw_js_code_stmt _ -> false (* TOO lazy, here comparison is only approximation*)
  


(* TODO:  We can relax this a bit later,
    but decide whether to inline it later in the call site
 *)
let safe_to_inline (lam : Lam.t) = 
  match lam with 
  | Lfunction _ ->  true
  | Lconst (Const_pointer _  | Const_immstring _ ) -> true
  | _ -> false
