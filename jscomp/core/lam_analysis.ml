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
let not_zero_constant ( x : Lam_constant.t) =  
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
    Ext_list.for_all args  no_side_effects && 
    (
      match primitive with 
      | Pccall {prim_name ; _} ->
        begin 
          match prim_name,args with 
          | ("caml_register_named_value"
            (* register to c runtime does not make sense  in ocaml *)
            (* | "caml_set_oo_id"  *) (* it does have side effect, just in creation path it happens not to have *)
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
      | Pjs_typeof
      | Pis_null
      | Pis_not_none
      | Psome
      | Psome_not_nest
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
      | Pfield_computed
      | Pval_from_option
      | Pval_from_option_not_nest
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
      | Pnegfloat 
      (* | Pabsfloat *)
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
      | Parraylength  
      | Parrayrefu 
      | Parrayrefs  
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
      (* | Pjs_is_instance_array *)
      | Pwrap_exn
      | Praw_js_function _
        -> true
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
      | Parraysets 
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
      | Parraysetu  
      | Poffsetref _ 
      | Praise
      | Plazyforce 
      | Psetfield _ 
      | Psetfield_computed
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
    Ext_list.for_all_snd bindings no_side_effects && no_side_effects body
  | Lwhile _ -> false (* conservative here, non-terminating loop does have side effect *)
  | Lfor _ -> false 
  | Lassign _ -> false (* actually it depends ... *)
  | Lsend _ -> false 
  | Lapply _ -> false (* we need purity analysis .. *)
  


(* 
    Estimate the size of lambda for better inlining 
    threshold is 1000 - so that we 
 *)
exception Too_big_to_inline

let really_big () = raise_notrace Too_big_to_inline

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
    | Lprim {primitive = Praise | Pis_not_none ; args =  [l ];  _} 
      -> size l
    | Lam.Lglobal_module _ -> 1       
    | Lprim {primitive = 
        Praw_js_code_stmt _ 
      | Praw_js_function _ 
      | Praw_js_code_exp _ } -> really_big ()
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

  with Too_big_to_inline ->  1000 
and size_constant x = 
  match x with 
  | Const_int _ | Const_char _ 

  | Const_float _  | Const_int32 _ | Const_int64 _ 
  | Const_nativeint _ 
  | Const_immstring _
  | Const_pointer _ 
  | Const_js_null | Const_js_undefined
  | Const_js_true | Const_js_false
    -> 1 
  | Const_unicode _  (* TODO: this seems to be not good heurisitives*)
  | Const_string _ ->  1
  | Const_some s -> size_constant s   
  | Const_block (_, _, str) 
    ->  List.fold_left (fun acc x -> acc + size_constant x ) 0 str
  | Const_float_array xs  -> List.length xs

and size_lams acc (lams : Lam.t list) = 
  List.fold_left (fun acc l -> acc  + size l ) acc lams
let args_all_const (args : Lam.t list) =
  Ext_list.for_all args (fun x -> match x with Lconst _ -> true | _ -> false) 
    
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
    -> (* -FIXME *)
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







(* TODO:  We can relax this a bit later,
    but decide whether to inline it later in the call site
 *)
let safe_to_inline (lam : Lam.t) = 
  match lam with 
  | Lfunction _ ->  true
  | Lconst 
    (Const_pointer _  
    | Const_immstring _ 
    | Const_js_true 
    | Const_js_false
    | Const_js_undefined
    ) -> true
  | _ -> false
