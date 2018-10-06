(* Copyright (C) 2018 - Authors of BuckleScript
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

type ident = Ident.t

type apply_status =
  | App_na
  | App_ml_full
  | App_js_full


module Types = struct
  type switch =
    { sw_numconsts: bool; (* TODO: refine its representation *)
      sw_consts: (int * t) list;
      sw_numblocks: bool;
      sw_blocks: (int * t) list;
      sw_failaction : t option}
  (* 
    Invariant: 
    length (sw_consts) <= sw_numconsts 
    when length (sw_consts) >= sw_numconsts -> true 
    Note that failaction would appear in both
     {[
       match x with
       | ..
         | ..
           | _ -> 2
     ]}
     since compiler would first test [x] is a const pointer
     or not then the [default] applies to each branch.

     In most cases: {[
       let sw =
         {sw_numconsts = cstr.cstr_consts; sw_consts = consts;
          sw_numblocks = cstr.cstr_nonconsts; sw_blocks = nonconsts;
          sw_failaction = None} in
     ]}

     but there are some edge cases (see https://caml.inria.fr/mantis/view.php?id=6033)
     one predicate used is
     {[
       (sw.sw_numconsts - List.length sw.sw_consts) +
       (sw.sw_numblocks - List.length sw.sw_blocks) > 1
     ]}
     if [= 1] with [some fail] -- called once
     if [= 0] could not have [some fail]
  *)
  and prim_info =
    { primitive : Lam_primitive.t ;
      args : t list ;
      loc : Location.t;
    }
  and apply_info =
    { fn : t ;
      args : t list ;
      loc : Location.t;
      status : apply_status
    }
  and function_info =
    { arity : int ;
      params : ident list ;
      body : t
    }
  and t =
    | Lvar of ident
    | Lglobal_module of ident
    | Lconst of Lam_constant.t
    | Lapply of apply_info
    | Lfunction of function_info
    | Llet of Lam_compat.let_kind * ident * t * t
    | Lletrec of (ident * t) list * t
    | Lprim of prim_info
    | Lswitch of t * switch
    | Lstringswitch of t * (string * t) list * t option
    | Lstaticraise of int * t list
    | Lstaticcatch of t * (int * ident list) * t
    | Ltrywith of t * ident * t
    | Lifthenelse of t * t * t
    | Lsequence of t * t
    | Lwhile of t * t
    | Lfor of ident * t * t * Asttypes.direction_flag * t
    | Lassign of ident * t
    | Lsend of Lam_compat.meth_kind * t * t * t list * Location.t
end

module X = struct
  type switch
    = Types.switch
    =
      { sw_numconsts: bool;
        sw_consts: (int * t) list;
        sw_numblocks: bool;
        sw_blocks: (int * t) list;
        sw_failaction : t option}
  and prim_info
    =  Types.prim_info
    =
      { primitive : Lam_primitive.t ;
        args : t list ;
        loc : Location.t;
      }
  and apply_info
    = Types.apply_info
    =
      { fn : t ;
        args : t list ;
        loc : Location.t;
        status : apply_status
      }

  and function_info
    = Types.function_info
    =
      { arity : int ;
        params : ident list ;
        body : t
      }
  and t
    = Types.t
    =
      | Lvar of ident
      | Lglobal_module of ident
      | Lconst of Lam_constant.t
      | Lapply of apply_info
      | Lfunction of function_info
      | Llet of Lam_compat.let_kind * ident * t * t
      | Lletrec of (ident * t) list * t
      | Lprim of prim_info
      | Lswitch of t * switch
      | Lstringswitch of t * (string * t) list * t option
      | Lstaticraise of int * t list
      | Lstaticcatch of t * (int * ident list) * t
      | Ltrywith of t * ident * t
      | Lifthenelse of t * t * t
      | Lsequence of t * t
      | Lwhile of t * t
      | Lfor of ident * t * t * Asttypes.direction_flag * t
      | Lassign of ident * t
      | Lsend of Lam_compat.meth_kind * t * t * t list * Location.t
end
include Types




(** apply [f] to direct successor which has type [Lam.t] *)

let inner_map 
   (l : t) (f : t -> X.t ) : X.t =
  match l  with
  | Lvar (_ : ident)
  | Lconst (_ : Lam_constant.t) ->
    ( (* Obj.magic *) l : X.t)
  | Lapply ({fn; args; loc; status} )  ->
    let fn = f fn in
    let args = Ext_list.map args f in
    Lapply { fn ; args; loc; status }
  | Lfunction({body; arity;  params } ) ->
    let body = f body in
    Lfunction {body; arity;  params}
  | Llet(str, id, arg, body) ->
    let arg = f arg in let body =  f body in
    Llet(str,id,arg,body)
  | Lletrec(decl, body) ->
    let body = f body in
    let decl = Ext_list.map_snd decl f in
    Lletrec(decl,body)
  | Lglobal_module _ -> (l : X.t)
  | Lprim {args; primitive ; loc}  ->
    let args = Ext_list.map args f in
    Lprim { args; primitive; loc}

  | Lswitch(arg, {sw_consts; sw_numconsts; sw_blocks; sw_numblocks; sw_failaction}) ->
    let arg = f arg in
    let sw_consts = Ext_list.map_snd  sw_consts f in
    let sw_blocks = Ext_list.map_snd  sw_blocks f in
    let sw_failaction = Ext_option.map sw_failaction f in
    Lswitch(arg, { sw_consts; sw_blocks; sw_failaction; sw_numblocks; sw_numconsts})
  | Lstringswitch (arg,cases,default) ->
    let arg = f arg  in
    let cases = Ext_list.map_snd  cases f in
    let default = Ext_option.map default f in
    Lstringswitch(arg,cases,default)
  | Lstaticraise (id,args) ->
    let args = Ext_list.map args f in
    Lstaticraise(id,args)
  | Lstaticcatch(e1, vars , e2) ->
    let e1 = f e1 in
    let e2 = f e2 in
    Lstaticcatch(e1, vars, e2)
  | Ltrywith(e1, exn, e2) ->
    let e1  = f e1 in
    let e2 =  f e2 in
    Ltrywith(e1,exn,e2)
  | Lifthenelse(e1, e2, e3) ->
    let e1 = f e1 in let e2 =  f e2 in let e3 =  f e3 in
    Lifthenelse(e1,e2,e3)
  | Lsequence(e1, e2) ->
    let e1 = f e1 in let e2 =  f e2 in
    Lsequence(e1,e2)
  | Lwhile(e1, e2) ->
    let e1 = f e1 in let e2 =  f e2 in
    Lwhile(e1,e2)
  | Lfor(v, e1, e2, dir, e3) ->
    let e1 = f e1 in let e2 =  f e2 in let e3 =  f e3 in
    Lfor(v,e1,e2,dir,e3)
  | Lassign(id, e) ->
    let e = f e in
    Lassign(id,e)
  | Lsend (k, met, obj, args, loc) ->
    let met = f met in
    let obj = f obj in
    let args = Ext_list.map args f in
    Lsend(k,met,obj,args,loc)








exception Not_simple_form

(**


   [is_eta_conversion_exn params inner_args outer_args]
   case 1:
   {{
    (fun params -> wrap (primitive (inner_args)) args
   }}
   when [inner_args] are the same as [params], it can be simplified as
   [wrap (primitive args)]

    where [wrap] used to be simple instructions
    Note that [external] functions are forced to do eta-conversion
    when combined with [|>] operator, we need to make sure beta-reduction
    is applied though since `[@bs.splice]` needs such guarantee.
    Since `[@bs.splice] is the tail position
*)
let rec is_eta_conversion_exn
    params inner_args outer_args : t list =
  match params, inner_args, outer_args with
  | x::xs, Lvar y::ys, r::rest
    when Ident.same x y ->
    r :: is_eta_conversion_exn xs ys rest
  | x::xs,
    (Lprim ({primitive = Pjs_fn_make _;
             args = [Lvar y] } as p ) ::ys),
    r :: rest when Ident.same x y ->
    Lprim ({p with args = [ r]}) ::
    is_eta_conversion_exn xs ys rest
  | [], [], [] -> []
  | _, _, _ -> raise_notrace Not_simple_form

(** FIXME: more robust inlining check later, we should inline it before we add stub code*)
let apply fn args loc status : t =
  match fn with
  | Lfunction {
               params;
               body = Lprim {primitive =
                               (Pundefined_to_opt |
                                Pnull_to_opt |
                                Pnull_undefined_to_opt |
                                Pis_null |
                                Pis_null_undefined |
                                Pjs_typeof ) as wrap;
                             args = [Lprim ({primitive; args = inner_args} as primitive_call)]
                            }
              } ->
    begin match is_eta_conversion_exn params inner_args args with
      | args
        ->
        Lprim {primitive = wrap ; args = [Lprim { primitive_call with args ; loc = loc }] ; loc }
      | exception Not_simple_form ->
        Lapply { fn; args; loc; status }
    end
  | Lfunction {
               params;
               body =Lprim ({primitive; args = inner_args}as primitive_call) }
    ->
    begin match is_eta_conversion_exn params inner_args args with
      | args
        ->
        Lprim { primitive_call with args ; loc = loc }
      | exception _ ->
        Lapply { fn; args;  loc;    status }
    end
  | Lfunction {
               params;
               body = Lsequence (Lprim ({primitive; args = inner_args}as primitive_call), (Lconst _ as const )) }
    ->
    begin match is_eta_conversion_exn params inner_args args with
      | args
        ->
        Lsequence(Lprim { primitive_call with args ; loc = loc }, const)
      | exception _ ->
        Lapply { fn; args;  loc;    status }
    end
  (* | Lfunction {params;body} when Ext_list.same_length params args ->
      Ext_list.fold_right2 (fun p arg acc ->
        Llet(Strict,p,arg,acc)
      ) params args body *) (* TODO: more rigirous analysis on [let_kind] *)
  | _ ->
    Lapply { fn; args;  loc  ; status }






let switch lam (lam_switch : switch) : t =
  match lam with
  | Lconst ((Const_pointer (i,_) |  (Const_int i)))
    ->
    Ext_list.assoc_by_int   lam_switch.sw_consts i lam_switch.sw_failaction
  | Lconst (Const_block (i,_,_)) ->
    Ext_list.assoc_by_int lam_switch.sw_blocks i lam_switch.sw_failaction 
  | _ ->
    Lswitch(lam,lam_switch)

let stringswitch (lam : t) cases default : t =
  match lam with
  | Lconst (Const_string a) ->
    Ext_list.assoc_by_string  cases a default
  | _ -> Lstringswitch(lam, cases, default)


let true_ : t =
  Lconst (Const_js_true)

let false_ : t =
  Lconst (Const_js_false)

let unit : t =
  Lconst (Const_pointer( 0, Pt_constructor "()"))




let rec seq (a : t) b : t =
   match a with
  | Lprim 
    {primitive = Pmakeblock(_); 
     args= x::xs} -> 
    seq (List.fold_left seq x xs) b 
  | _ -> 
  Lsequence (a, b)


let var id : t = Lvar id
let global_module id = Lglobal_module id
let const ct : t = Lconst ct
let function_ ~arity  ~params ~body : t =
  Lfunction { arity;  params ; body}

let let_ kind id e body :  t
  = Llet (kind,id,e,body)
let letrec bindings body : t =
  Lletrec(bindings,body)
let while_ a b : t  =
  Lwhile(a,b)

let try_  body id  handler : t =
  Ltrywith(body,id,handler)

let for_ v e1 e2 dir e3 : t  =
  Lfor(v,e1,e2,dir,e3)

let assign v l : t = Lassign(v,l)
let send u m o ll v : t =
  Lsend(u, m, o, ll, v)
let staticcatch  a b c : t = Lstaticcatch(a,b,c)
let staticraise a b : t = Lstaticraise(a,b)


module Lift = struct
  let int i : t =
    Lconst ((Const_int i))


  let int32 i : t =
    Lconst ((Const_int32 i))

  let bool b = if b then true_ else false_

  (* ATTENTION: [float, nativeint] constant propogaton is not done
     yet , due to cross platform problem
  *)
  let float b  : t =
    Lconst ((Const_float b))

  let nativeint b : t =
    Lconst ((Const_nativeint b))

  let int32 b : t =
    Lconst ((Const_int32 b))

  let int64 b : t =
    Lconst ((Const_int64 b))
  let string b : t =
    Lconst ((Const_string (b)))
  let char b : t =
    Lconst ((Const_char b))
end



let prim ~primitive:(prim : Lam_primitive.t) ~args loc  : t =
  let default () : t = Lprim { primitive = prim ;args; loc} in
  match args with
  | [Lconst a] ->
    begin match prim, a  with
      | Pnegint, ((Const_int a))
        -> Lift.int (- a)
      (* | Pfloatofint, ( (Const_int a)) *)
      (*   -> Lift.float (float_of_int a) *)
      | Pintoffloat, ( (Const_float a))
        ->
        Lift.int (int_of_float (float_of_string a))
      (* | Pnegfloat -> Lift.float (-. a) *)
      (* | Pabsfloat -> Lift.float (abs_float a) *)
      | Pstringlength, Const_string a
        ->
        Lift.int (String.length a)
      (* | Pnegbint Pnativeint, ( (Const_nativeint i)) *)
      (*   ->   *)
      (*   Lift.nativeint (Nativeint.neg i) *)
      | Pnegbint Pint32, Const_int32 a
        ->
        Lift.int32 (Int32.neg a)
      | Pnegbint Pint64, Const_int64 a
        ->
        Lift.int64 (Int64.neg a)
      | Pnot, Const_js_true -> false_
      | Pnot, Const_js_false -> true_
      | Pnot , Const_pointer (a,_)
        -> Lift.bool (a = 0 )
      | _ -> default ()
    end


  | [Lconst a ; Lconst b] ->
    begin match prim, a, b  with
      | Pbintcomp(_, cmp),  (Const_int32 a),  (Const_int32 b)
        -> Lift.bool (Lam_compat.cmp_int32 cmp a b)
      | Pbintcomp(_, cmp),  (Const_int64 a),  (Const_int64 b)
        -> Lift.bool (Lam_compat.cmp_int64  cmp a b)
      | Pbintcomp(_, cmp),  (Const_nativeint a),  (Const_nativeint b)
        -> Lift.bool (Lam_compat.cmp_nativeint  cmp a b)
      | Pfloatcomp  cmp,  (Const_float a),  (Const_float b)
        -> (** FIXME: could raise? *)
          Lift.bool (Lam_compat.cmp_float  cmp (float_of_string a) (float_of_string b))
      | Pintcomp cmp ,
        ( (Const_int a) | Const_pointer (a,_)),
        ( (Const_int b) | Const_pointer (b,_))
        -> Lift.bool (Lam_compat.cmp_int cmp a b)
      | (Paddint
        | Psubint
        | Pmulint
        | Pdivint
        | Pmodint
        | Pandint
        | Porint
        | Pxorint
        | Plslint
        | Plsrint
        | Pasrint), (Const_int a),   (Const_int b)
        ->
        (* WE SHOULD keep it as [int], to preserve types *)
        let aa,bb = Int32.of_int a, Int32.of_int  b in
        let int_ v = Lift.int (Int32.to_int v ) in
        begin match prim with
          | Paddint -> int_ (Int32.add aa bb)
          | Psubint -> int_ (Int32.sub aa bb)
          | Pmulint -> int_ (Int32.mul aa  bb)
          | Pdivint ->
            if bb = 0l then default ()
            else int_ (Int32.div aa bb)
          | Pmodint ->
            if bb = 0l then default ()
            else int_ (Int32.rem aa bb)
          | Pandint -> int_ (Int32.logand aa bb)
          | Porint -> int_ (Int32.logor aa bb)
          | Pxorint -> int_ (Int32.logxor aa bb)
          | Plslint -> int_ (Int32.shift_left  aa b )
          | Plsrint -> int_ (Int32.shift_right_logical aa  b)
          | Pasrint -> int_ (Int32.shift_right aa b)
          | _ -> default ()
        end
      | (Paddbint Pint32
        | Psubbint Pint32
        | Pmulbint Pint32
        | Pdivbint Pint32
        | Pmodbint Pint32
        | Pandbint Pint32
        | Porbint Pint32
        | Pxorbint Pint32
        ),  (Const_int32 aa),   (Const_int32 bb)
        ->
        begin match prim with
          | Paddbint _  -> Lift.int32 (Int32.add aa bb)
          | Psubbint _  -> Lift.int32 (Int32.sub aa bb)
          | Pmulbint _ -> Lift.int32 (Int32.mul aa  bb)
          | Pdivbint _ ->  (try Lift.int32 (Int32.div aa  bb) with _  -> default ())
          | Pmodbint _ -> (try Lift.int32 (Int32.rem aa  bb) with _ -> default ())
          | Pandbint _ -> Lift.int32 (Int32.logand aa bb)
          | Porbint _ -> Lift.int32 (Int32.logor aa bb)
          | Pxorbint _ -> Lift.int32 (Int32.logxor aa bb)
          | _ -> default ()
        end
      | Plslbint Pint32,  (Const_int32 aa),  (Const_int b)
        -> Lift.int32 (Int32.shift_left  aa b )
      | Plsrbint Pint32,  (Const_int32 aa),  (Const_int b)
        -> Lift.int32 (Int32.shift_right_logical  aa b )
      | Pasrbint Pint32,  (Const_int32 aa),  (Const_int b)
        -> Lift.int32 (Int32.shift_right  aa b )

      | (Paddbint Pint64
        | Psubbint Pint64
        | Pmulbint Pint64
        | Pdivbint Pint64
        | Pmodbint Pint64
        | Pandbint Pint64
        | Porbint Pint64
        | Pxorbint Pint64
        ),  (Const_int64 aa),   (Const_int64 bb)
        ->
        begin match prim with
          | Paddbint _  -> Lift.int64 (Int64.add aa bb)
          | Psubbint _  -> Lift.int64 (Int64.sub aa bb)
          | Pmulbint _ -> Lift.int64 (Int64.mul aa  bb)
          | Pdivbint _ -> (try Lift.int64 (Int64.div aa  bb) with _ -> default ())
          | Pmodbint _ -> (try Lift.int64 (Int64.rem aa  bb) with _ -> default ())
          | Pandbint _ -> Lift.int64 (Int64.logand aa bb)
          | Porbint _ -> Lift.int64 (Int64.logor aa bb)
          | Pxorbint _ -> Lift.int64 (Int64.logxor aa bb)
          | _ -> default ()
        end
      | Plslbint Pint64,  (Const_int64 aa),  (Const_int b)
        -> Lift.int64 (Int64.shift_left  aa b )
      | Plsrbint Pint64,  (Const_int64 aa),  (Const_int b)
        -> Lift.int64 (Int64.shift_right_logical  aa b )
      | Pasrbint Pint64,  (Const_int64 aa),  (Const_int b)
        -> Lift.int64 (Int64.shift_right  aa b )

      | Psequand, Const_js_false, 
        (Const_js_true | Const_js_false) ->
        false_
      | Psequand, Const_js_true, Const_js_true ->
        true_
      | Psequand, Const_js_true, Const_js_false ->
        false_
      | Psequor, Const_js_true, (Const_js_true | Const_js_false) ->
        true_
      | Psequor, Const_js_false, Const_js_true -> true_
      | Psequor, Const_js_false, Const_js_false -> false_        
      | Pstringadd, (Const_string (a)),
        (Const_string (b))
        ->
        Lift.string (a ^ b)
      | (Pstringrefs | Pstringrefu), (Const_string(a)),
        ((Const_int b)| Const_pointer (b,_))
        ->
        begin try Lift.char (String.get a b)
          with  _ -> default ()
        end
      | _ -> default ()
    end

  | _ -> default ()

let not_ loc x  : t =
  prim ~primitive:Pnot ~args:[x] loc


let has_boolean_type (x : t) = 
  match x with 
  | Lprim {primitive =
    Pnot | Psequand |
    Psequor 
    | Pisout 
    | Pintcomp _ 
    | Pis_not_none
    | Pfloatcomp _; loc}
  | Lprim {primitive = 
    Pccall {prim_name = "caml_string_equal" | "caml_string_notequal"};
    loc
    }
   -> Some loc
  | _ -> None

(** [complete_range sw_consts 0 7]
    is complete with [0,1,.. 7]
*)  
let rec complete_range  (sw_consts : (int * _) list) ~(start : int) ~finish=   
  match sw_consts with 
  | [] -> finish < start
  | (i,_)::rest 
    -> 
      start <= finish &&
      i = start &&
      complete_range  rest ~start:(start + 1) ~finish


let if_ (a : t) (b : t) c =
  match a with
  | Lconst v ->
    begin match v with
      | Const_pointer (x, _)  | (Const_int x)
        ->
        if x <> 0 then b else c
      | (Const_char x) ->
        if Char.code x <> 0 then b else c
      | (Const_int32 x) ->
        if x <> 0l then b else c
      |  (Const_int64 x) ->
        if x <> 0L then b else c
      | (Const_nativeint x) ->
        if x <> 0n then b else c
      | Const_js_false
      | Const_js_null
      | Const_js_undefined -> c
      | Const_js_true
      | Const_string _
      | Const_float _
      | Const_unicode _
      | Const_block _
      | Const_some _ 
      | Const_float_array _
      | Const_immstring _ -> b
    end
  
  | _ -> 
    begin match  b, c with 
      | Lconst(Const_js_true), Lconst(Const_js_false)
        -> 
        if has_boolean_type a != None then a 
        else Lifthenelse (a,b,c)
      | Lconst(Const_js_false), Lconst(Const_js_true)
        ->  
        (match  has_boolean_type a with
         | Some loc ->  not_ loc a 
         | None -> Lifthenelse (a,b,c))     
      | _ -> 
        (match a with 
         | Lprim {primitive = Pisout; args = [Lconst(Const_int range); Lvar xx] } 
           -> 
           begin match c with 
             | Lswitch ( Lvar yy as switch_arg, 
                         ({sw_blocks = []; sw_numblocks = true; sw_consts ;
                           sw_numconsts; sw_failaction = None} as body)
                       )
               when Ident.same xx yy 
                 && complete_range sw_consts ~start:0 ~finish:range
               ->  
               Lswitch(switch_arg, 
                       { body with sw_failaction = Some b; sw_numconsts = false })
             |  _ -> Lifthenelse(a,b,c)      
           end
         | _ ->  Lifthenelse (a,b,c))
    end 


(** TODO: the smart constructor is not exploited yet*)
(** [l || r ] *)
let sequor l r = if_ l true_ r

(** [l && r ] *)
let sequand l r = if_ l r false_  

(******************************************************************)
(** only [handle_bs_non_obj_ffi] will be used outside *)
(**
   [no_auto_uncurried_arg_types xs]
   check if the FFI have [@@bs.uncurry] attribute.
   if it does not we wrap it in a nomral way otherwise
*)
let rec no_auto_uncurried_arg_types
    (xs : External_arg_spec.t list)  =
  match xs with
  | [] -> true
  | {arg_type = Fn_uncurry_arity _ } :: _ ->
    false
  | _ :: xs -> no_auto_uncurried_arg_types xs


let result_wrap loc (result_type : External_ffi_types.return_wrapper) result  =
  match result_type with
  | Return_replaced_with_unit
    -> seq result unit
  | Return_null_to_opt -> prim ~primitive:Pnull_to_opt ~args:[result] loc
  | Return_null_undefined_to_opt -> prim ~primitive:Pnull_undefined_to_opt ~args:[result] loc
  | Return_undefined_to_opt -> prim ~primitive:Pundefined_to_opt ~args:[result] loc
  | Return_unset
  | Return_identity ->
    result
(* TODO: sort out the order here
   consolidate {!Lam_compile_external_call.assemble_args_splice}
*)
let rec transform_uncurried_arg_type loc (arg_types : External_arg_spec.t list)
    (args : t list ) =
  match arg_types,args with
  | { arg_type = Fn_uncurry_arity n ; arg_label } :: xs,
    y::ys ->
    let (o_arg_types, o_args) =
      transform_uncurried_arg_type loc xs ys in
    { External_arg_spec.arg_type = Nothing ; arg_label } :: o_arg_types ,
    prim ~primitive:(Pjs_fn_make n) ~args:[y] loc :: o_args
  |  x  ::xs, y::ys ->
    begin match x with
      | {arg_type = Arg_cst _ }  ->
        let o_arg_types, o_args = transform_uncurried_arg_type loc xs args in
        x :: o_arg_types , o_args
      | _ ->
        let o_arg_types, o_args = transform_uncurried_arg_type loc xs ys in
        x :: o_arg_types , y:: o_args
    end
  | [] , []
  | _::_, []
  | [], _::_ as ok -> ok


let handle_bs_non_obj_ffi
    (arg_types : External_arg_spec.t list)
    (result_type : External_ffi_types.return_wrapper)
    ffi
    args
    loc
    prim_name =
  if no_auto_uncurried_arg_types arg_types then
    result_wrap loc result_type @@ prim ~primitive:(Pjs_call(prim_name, arg_types, ffi))
      ~args loc
  else
    let n_arg_types, n_args =
      transform_uncurried_arg_type loc  arg_types args in
    result_wrap loc result_type @@
    prim ~primitive:(Pjs_call (prim_name, n_arg_types, ffi))
      ~args:n_args loc

