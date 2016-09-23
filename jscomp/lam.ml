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

type array_kind = Lambda.array_kind
type boxed_integer = Lambda.boxed_integer
type comparison = Lambda.comparison 
type bigarray_kind = Lambda.bigarray_kind
type bigarray_layout = Lambda.bigarray_layout
type compile_time_constant = Lambda.compile_time_constant

type tag_info = Lambda.tag_info
type mutable_flag = Asttypes.mutable_flag
type field_dbg_info = Lambda.field_dbg_info 
type set_field_dbg_info = Lambda.set_field_dbg_info

type ident = Ident.t

type function_arities = 
  | Determin of bool * (int * Ident.t list option) list  * bool
  | NA 
               
type primitive = 
  | Pbytes_to_string
  | Pbytes_of_string
  (* Globals *)
  | Pgetglobal of ident
  | Psetglobal of ident
  | Pglobal_exception of ident       
  (* Operations on heap blocks *)
  | Pmakeblock of int * tag_info * mutable_flag
  | Pfield of int * field_dbg_info
  | Psetfield of int * bool * set_field_dbg_info
  (* could have field info at least for record *)
  | Pfloatfield of int * field_dbg_info
  | Psetfloatfield of int * set_field_dbg_info
  | Pduprecord of Types.record_representation * int
  (* Force lazy values *)
  | Plazyforce
  (* External call *)
  | Pccall of  Primitive.description
  (* Exceptions *)
  | Praise
  (* Boolean operations *)
  | Psequand | Psequor | Pnot
  (* Integer operations *)
  | Pnegint | Paddint | Psubint | Pmulint | Pdivint | Pmodint
  | Pandint | Porint | Pxorint
  | Plslint | Plsrint | Pasrint
  | Pintcomp of comparison
  | Poffsetint of int
  | Poffsetref of int
  (* Float operations *)
  | Pintoffloat | Pfloatofint
  | Pnegfloat | Pabsfloat
  | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat
  | Pfloatcomp of comparison
  (* String operations *)
  | Pstringlength 
  | Pstringrefu 
  | Pstringrefs
  | Pstringadd  
  | Pbyteslength
  | Pbytesrefu
  | Pbytessetu 
  | Pbytesrefs
  | Pbytessets
  (* Array operations *)
  | Pmakearray of array_kind
  | Parraylength of array_kind
  | Parrayrefu of array_kind
  | Parraysetu of array_kind
  | Parrayrefs of array_kind
  | Parraysets of array_kind
  (* Test if the argument is a block or an immediate integer *)
  | Pisint
  (* Test if the (integer) argument is outside an interval *)
  | Pisout
  (* Bitvect operations *)
  | Pbittest
  (* Operations on boxed integers (Nativeint.t, Int32.t, Int64.t) *)
  | Pbintofint of boxed_integer
  | Pintofbint of boxed_integer
  | Pcvtbint of boxed_integer (*source*) * boxed_integer (*destination*)
  | Pnegbint of boxed_integer
  | Paddbint of boxed_integer
  | Psubbint of boxed_integer
  | Pmulbint of boxed_integer
  | Pdivbint of boxed_integer
  | Pmodbint of boxed_integer
  | Pandbint of boxed_integer
  | Porbint of boxed_integer
  | Pxorbint of boxed_integer
  | Plslbint of boxed_integer
  | Plsrbint of boxed_integer
  | Pasrbint of boxed_integer
  | Pbintcomp of boxed_integer * comparison
  (* Operations on big arrays: (unsafe, #dimensions, kind, layout) *)
  | Pbigarrayref of bool * int * bigarray_kind * bigarray_layout
  | Pbigarrayset of bool * int * bigarray_kind * bigarray_layout
  (* size of the nth dimension of a big array *)
  | Pbigarraydim of int
  (* load/set 16,32,64 bits from a string: (unsafe)*)
  | Pstring_load_16 of bool
  | Pstring_load_32 of bool
  | Pstring_load_64 of bool
  | Pstring_set_16 of bool
  | Pstring_set_32 of bool
  | Pstring_set_64 of bool
  (* load/set 16,32,64 bits from a
     (char, int8_unsigned_elt, c_layout) Bigarray.Array1.t : (unsafe) *)
  | Pbigstring_load_16 of bool
  | Pbigstring_load_32 of bool
  | Pbigstring_load_64 of bool
  | Pbigstring_set_16 of bool
  | Pbigstring_set_32 of bool
  | Pbigstring_set_64 of bool
  (* Compile time constants *)
  | Pctconst of compile_time_constant
  (* byte swap *)
  | Pbswap16
  | Pbbswap of boxed_integer
  (* Integer to external pointer *)

  | Pdebugger 
  | Pjs_unsafe_downgrade of string * Location.t
  | Pinit_mod
  | Pupdate_mod
  | Pjs_fn_make of int 
  | Pjs_fn_run of int 
  | Pjs_fn_method of int 
  | Pjs_fn_runmethod of int

type switch = 
  { sw_numconsts: int;
    sw_consts: (int * t) list;
    sw_numblocks: int;
    sw_blocks: (int * t) list;
    sw_failaction : t option}
and prim_info = 
  { primitive : primitive ; 
    args : t list ;
    loc : Location.t;
  }
and apply_status =
  | App_na
  | App_ml_full
  | App_js_full    
and apply_info = 
  { fn : t ; 
    args : t list ; 
    loc : Location.t;
    status : apply_status
  }
and function_info = 
  { arity : int ; 
   kind : Lambda.function_kind ; 
   params : ident list ;
   body : t 
  }
and t = 
  | Lvar of ident
  | Lconst of Lambda.structured_constant
  | Lapply of apply_info
  | Lfunction of function_info
  | Llet of Lambda.let_kind * ident * t * t
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
  | Lsend of Lambda.meth_kind * t * t * t list * Location.t
  | Lifused of ident * t
  (* | Levent of t * Lambda.lambda_event 
     [Levent] in the branch hurt pattern match, 
     we should use record for trivial debugger info
  *)



module Prim = struct 
  type t = primitive
  let mk name arity = 
    Pccall {prim_name = name ; 
            prim_native_name = "" ;
            prim_alloc = false;
            prim_native_float = false;
            prim_arity = arity;

           }
  let js_is_nil : t = 
    mk "js_is_nil" 1 
  let js_is_undef : t = 
    mk "js_is_undef" 1 
  let js_is_nil_undef : t  = 
    mk "js_is_nil_undef" 1 
end





type binop = t -> t -> t 

type triop = t -> t -> t -> t 

type unop = t -> t 


let var id : t = Lvar id
let const ct : t = Lconst ct 
let apply fn args loc status : t = 
  Lapply { fn; args;  loc  ;
           status }
let function_ ~arity ~kind ~params ~body : t = 
  Lfunction { arity; kind; params ; body}

let let_ kind id e body :  t 
  = Llet (kind,id,e,body)
let letrec bindings body : t = 
  Lletrec(bindings,body)

let if_ (a : t) (b : t) c = 
  match a with
  | Lconst v ->
    begin match v with
    | Const_pointer (x, _)  | Const_base(Const_int x)
      ->
      if x <> 0 then b else c
    | Const_base (Const_char x) ->
      if Char.code x <> 0 then b else c
    | Const_base (Const_int32 x) ->
      if x <> 0l then b else c
    | Const_base (Const_int64 x) ->
      if x <> 0L then b else c
    | Const_base (Const_nativeint x) ->
      if x <> 0n then b else c
    | Const_base (Const_string _ | Const_float _ ) -> b
    | Const_block _
    | Const_float_array _
    | Const_immstring _ -> b
    end
  | _ ->  Lifthenelse (a,b,c)

let switch lam (lam_switch : switch) : t =
  match lam with
  | Lconst ((Const_pointer (i,_) | Const_base (Const_int i)))
    ->
    begin try List.assoc i lam_switch.sw_consts
      with  Not_found ->
      match lam_switch.sw_failaction with
      | Some x -> x
      | None -> assert false
    end
  | Lconst (Const_block (i,_,_)) ->
    begin try List.assoc i lam_switch.sw_blocks
      with  Not_found ->
      match lam_switch.sw_failaction with
      | Some x -> x
      | None -> assert false
    end
  | _ -> 
    Lswitch(lam,lam_switch)

let stringswitch (lam : t) cases default : t = 
  match lam with
  | Lconst (Const_base (Const_string (a,_))) ->
    begin
      try List.assoc a cases with Not_found ->
        begin
          match default with
          | Some x -> x
          | None -> assert false
        end
    end
  | _ -> Lstringswitch(lam, cases, default)


let true_ : t =
  Lconst (Const_pointer ( 1, Pt_constructor "true")) 

let false_ : t =
  Lconst (Const_pointer( 0, Pt_constructor "false"))

let unit : t = 
  Lconst (Const_pointer( 0, Pt_constructor "()"))

let assert_false_unit : t = 
  Lconst (Const_pointer( 0, Pt_constructor "impossible branch"))

(** [l || r ] *)
let sequor l r = if_ l true_ r 

(** [l && r ] *)
let sequand l r = if_ l r false_

let seq a b : t = 
  Lsequence (a, b)

let while_ a b : t  = 
  Lwhile(a,b)

let try_  body id  handler : t = 
  Ltrywith(body,id,handler)

let for_ v e1 e2 dir e3 : t  = 
  Lfor(v,e1,e2,dir,e3)

let event l (_event : Lambda.lambda_event) = l 

let ifused v l : t  = 
  Lifused (v,l)

let assign v l : t = Lassign(v,l)

let send u m o ll v : t = 
  Lsend(u, m, o, ll, v)

let staticcatch  a b c : t = Lstaticcatch(a,b,c)

let staticraise a b : t = Lstaticraise(a,b)

let comparison (cmp : Lambda.comparison) a b : bool = 
  match cmp with 
  | Ceq -> a = b 
  | Cneq -> a <> b 
  | Cgt -> a > b 
  | Cle -> a <= b 
  | Clt -> a < b 
  | Cge -> a >= b 

module Lift = struct 
  let int i : t =
    Lconst (Const_base (Const_int i))


  let int32 i : t =
    Lconst (Const_base (Const_int32 i))

  let bool b = if b then true_ else false_

  (* ATTENTION: [float, nativeint] constant propogaton is not done
     yet , due to cross platform problem
  *) 
  let float b  : t = 
    Lconst (Const_base (Const_float b))

  let nativeint b : t = 
    Lconst (Const_base (Const_nativeint b))

  let int32 b : t = 
    Lconst (Const_base (Const_int32 b))

  let int64 b : t =
    Lconst (Const_base (Const_int64 b))
  let string b : t =
    Lconst (Const_base (Const_string (b, None)))
  let char b : t =
    Lconst (Const_base (Const_char b))    
end

let prim ~primitive:(prim : Prim.t) ~args:(ll : t list) loc  : t = 
  let default () : t = Lprim { primitive = prim ;args =  ll ; loc} in 
  match ll with 
  | [Lconst a] -> 
    begin match prim, a  with 
      | Pnegint, (Const_base (Const_int a))
        -> Lift.int (- a)
      (* | Pfloatofint, (Const_base (Const_int a)) *)
      (*   -> Lift.float (float_of_int a) *)
      | Pintoffloat, (Const_base (Const_float a))
        -> 
        Lift.int (int_of_float (float_of_string a))
        (* | Pnegfloat -> Lift.float (-. a) *)
        (* | Pabsfloat -> Lift.float (abs_float a) *)
      | Pstringlength, (Const_base (Const_string (a,_)) ) 
        -> 
        Lift.int (String.length a)
      (* | Pnegbint Pnativeint, (Const_base (Const_nativeint i)) *)
      (*   ->   *)
      (*   Lift.nativeint (Nativeint.neg i) *)
      | Pnegbint Pint32, (Const_base (Const_int32 a))
        -> 
        Lift.int32 (Int32.neg a)
      | Pnegbint Pint64, (Const_base (Const_int64 a))
        -> 
        Lift.int64 (Int64.neg a)
      | Pnot , Const_pointer (a,_) 
        -> Lift.bool (a = 0 )
      | _ -> default ()
    end


  | [Lconst a ; Lconst b] -> 
    begin match prim, a, b  with 
      | Pbintcomp(_, cmp), Const_base (Const_int32 a), Const_base (Const_int32 b)
        -> Lift.bool (comparison cmp a b)
      | Pbintcomp(_, cmp), Const_base (Const_int64 a), Const_base (Const_int64 b)
        -> Lift.bool (comparison cmp a b)
      | Pbintcomp(_, cmp), Const_base (Const_nativeint a), Const_base (Const_nativeint b)
        -> Lift.bool (comparison cmp a b)
      | Pfloatcomp  cmp, Const_base (Const_nativeint a), Const_base (Const_nativeint b)
        -> Lift.bool (comparison cmp a b)
      | Pintcomp cmp ,
        (Const_base (Const_int a) | Const_pointer (a,_)),
        (Const_base (Const_int b) | Const_pointer (b,_))
        -> Lift.bool (comparison cmp a b)
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
        | Pasrint),Const_base (Const_int a),  Const_base (Const_int b)
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
        ), Const_base (Const_int32 aa),  Const_base (Const_int32 bb)
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
      | Plslbint Pint32, Const_base (Const_int32 aa), Const_base (Const_int b)
        -> Lift.int32 (Int32.shift_left  aa b )
      | Plsrbint Pint32, Const_base (Const_int32 aa), Const_base (Const_int b)
        -> Lift.int32 (Int32.shift_right_logical  aa b )
      | Pasrbint Pint32, Const_base (Const_int32 aa), Const_base (Const_int b)
        -> Lift.int32 (Int32.shift_right  aa b )

      | (Paddbint Pint64
        | Psubbint Pint64
        | Pmulbint Pint64
        | Pdivbint Pint64
        | Pmodbint Pint64
        | Pandbint Pint64
        | Porbint Pint64
        | Pxorbint Pint64
        ), Const_base (Const_int64 aa),  Const_base (Const_int64 bb)
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
      | Plslbint Pint64, Const_base (Const_int64 aa), Const_base (Const_int b)
        -> Lift.int64 (Int64.shift_left  aa b )
      | Plsrbint Pint64, Const_base (Const_int64 aa), Const_base (Const_int b)
        -> Lift.int64 (Int64.shift_right_logical  aa b )
      | Pasrbint Pint64, Const_base (Const_int64 aa), Const_base (Const_int b)
        -> Lift.int64 (Int64.shift_right  aa b )
      | Psequand, Const_pointer (a, _), Const_pointer( b, _)
        -> 
        Lift.bool (a = 1 && b = 1)
      | Psequor, Const_pointer (a, _), Const_pointer( b, _)
        -> 
        Lift.bool (a = 1 || b = 1)
      | Pstringadd, Const_base(Const_string (a, None)),
        Const_base (Const_string (b,None))
        ->
        Lift.string (a ^ b)
      | (Pstringrefs | Pstringrefu), Const_base(Const_string(a,None)),
        (Const_base(Const_int b)| Const_pointer (b,_))
        ->
        begin try Lift.char (String.get a b)
          with  _ -> default ()
        end                       
      | _ -> default ()
    end

  | _ -> default ()


let not_ loc x  : t = 
  prim Pnot [x] loc

let lam_prim ~primitive:( p : Lambda.primitive) ~args loc  : t = 
  match p with 
  | Pint_as_pointer 
  | Pidentity ->  
    begin match args with [x] -> x | _ -> assert false end
  | Pbytes_to_string 
    -> prim ~primitive:Pbytes_to_string ~args loc
  | Pbytes_of_string -> prim ~primitive:Pbytes_of_string ~args loc
  | Pignore -> (* Pignore means return unit, it is not an nop *)
    begin match args with [x] -> seq x unit | _ -> assert false end
  | Prevapply 
    -> 
    begin match args with 
    | [x ; Lapply{fn; args}]
      -> apply fn (args @[x]) loc App_na
    | [x; f] ->  apply f [x] loc App_na
    | _ -> assert false 
    end

  | Pdirapply ->
    begin match args with 
    | [Lapply{fn ; args }; x ] 
      -> 
        apply fn (args @ [x]) loc App_na
    | [f;x] -> apply f [x] loc App_na
    | _ -> assert false 
    end
  | Ploc loc -> assert false (* already compiled away here*)
  | Pgetglobal id ->
    if Ident.is_predef_exn id then
      prim ~primitive:(Pglobal_exception id) ~args loc       
    else       
      prim ~primitive:(Pgetglobal id) ~args loc
  | Psetglobal id -> prim ~primitive:(Psetglobal id) ~args loc
  | Pmakeblock (tag,info, mutable_flag) 
    -> prim ~primitive:(Pmakeblock (tag,info,mutable_flag)) ~args loc
  | Pfield (id,info) 
    -> prim ~primitive:(Pfield (id,info)) ~args loc

  | Psetfield (id,b,info)
    -> prim ~primitive:(Psetfield (id,b,info)) ~args loc

  | Pfloatfield (id,info)
    -> prim ~primitive:(Pfloatfield (id,info)) ~args loc
  | Psetfloatfield (id,info) 
    -> prim ~primitive:(Psetfloatfield (id,info)) ~args loc
  | Pduprecord (repr,i) 
    -> prim ~primitive:(Pduprecord(repr,i)) ~args loc
  | Plazyforce -> prim ~primitive:Plazyforce ~args loc

  | Pccall a -> 
    let prim_name = a.prim_name in
    if Pervasives.not @@ Ext_string.starts_with prim_name "js_" then 
      prim ~primitive:(Pccall a ) ~args loc else 
    if prim_name =  Literals.js_debugger then 
      prim ~primitive:Pdebugger ~args loc else 
    if prim_name =  Literals.js_fn_run || prim_name = Literals.js_method_run then
      prim ~primitive:(Pjs_fn_run (int_of_string a.prim_native_name)) ~args loc else 
    if prim_name = Literals.js_fn_mk then 
      prim ~primitive:(Pjs_fn_make (int_of_string a.prim_native_name)) ~args loc else                
    if prim_name = Literals.js_fn_method then 
      prim ~primitive:(Pjs_fn_method (int_of_string a.prim_native_name)) ~args loc else
    if prim_name = Literals.js_fn_runmethod then 
      prim ~primitive:(Pjs_fn_runmethod (int_of_string a.prim_native_name)) ~args loc 
    else
      prim ~primitive:(Pccall a) ~args loc
  | Praise _ ->
    if Js_config.get_no_any_assert () then 
      begin match args with 
        | [Lprim {primitive = Pmakeblock (0, _, _) ; 
                  args = [ 
                    Lprim {primitive = Pglobal_exception ({name = "Assert_failure"} as id); args =  []}; 
                    _
                  ]
                 } ] when Ident.global id
          -> assert_false_unit
        | _ -> prim ~primitive:Praise ~args loc 
      end
    else prim ~primitive:Praise ~args loc 
  | Psequand -> prim ~primitive:Psequand ~args loc
  | Psequor -> prim ~primitive:Psequor ~args loc
  | Pnot -> prim ~primitive:Pnot ~args loc 
  | Pnegint -> prim ~primitive:Pnegint ~args  loc 
  | Paddint -> prim ~primitive:Paddint ~args loc 
  | Psubint -> prim ~primitive:Psubint ~args loc 
  | Pmulint -> prim ~primitive:Pmulint ~args loc 
  | Pdivint -> prim ~primitive:Pdivint ~args loc 
  | Pmodint -> prim ~primitive:Pmodint ~args loc 
  | Pandint -> prim ~primitive:Pandint ~args loc 
  | Porint -> prim ~primitive:Porint ~args loc 
  | Pxorint -> prim ~primitive:Pxorint ~args loc 
  | Plslint -> prim ~primitive:Plslint ~args loc 
  | Plsrint -> prim ~primitive:Plsrint ~args loc 
  | Pasrint -> prim ~primitive:Pasrint ~args loc 
  | Pstringlength -> prim ~primitive:Pstringlength ~args loc 
  | Pstringrefu -> prim ~primitive:Pstringrefu ~args loc 
  | Pstringsetu 
  | Pstringsets -> assert false
  | Pstringrefs -> prim ~primitive:Pstringrefs ~args loc 

  | Pbyteslength -> prim ~primitive:Pbyteslength ~args loc 
  | Pbytesrefu -> prim ~primitive:Pbytesrefu ~args loc
  | Pbytessetu -> prim ~primitive:Pbytessetu ~args  loc 
  | Pbytesrefs -> prim ~primitive:Pbytesrefs ~args loc 
  | Pbytessets -> prim ~primitive:Pbytessets ~args loc 
  | Pisint -> prim ~primitive:Pisint ~args loc 
  | Pisout -> prim ~primitive:Pisout ~args loc 
  | Pbittest -> prim ~primitive:Pbittest ~args loc 
  | Pintoffloat -> prim ~primitive:Pintoffloat ~args loc
  | Pfloatofint -> prim ~primitive:Pfloatofint ~args loc 
  | Pnegfloat -> prim ~primitive:Pnegfloat ~args loc 
  | Pabsfloat -> prim ~primitive:Pabsfloat ~args loc 
  | Paddfloat -> prim ~primitive:Paddfloat ~args loc 
  | Psubfloat -> prim ~primitive:Psubfloat ~args loc 
  | Pmulfloat -> prim ~primitive:Pmulfloat ~args loc 
  | Pdivfloat -> prim ~primitive:Pdivfloat ~args loc 

  | Pbswap16 -> prim ~primitive:Pbswap16 ~args loc 
  | Pintcomp x -> prim ~primitive:(Pintcomp x)  ~args loc 
  | Poffsetint x -> prim ~primitive:(Poffsetint x) ~args loc 
  | Poffsetref x -> prim ~primitive:(Poffsetref x) ~args  loc
  | Pfloatcomp x -> prim ~primitive:(Pfloatcomp x) ~args loc 
  | Pmakearray x -> prim ~primitive:(Pmakearray x) ~args  loc 
  | Parraylength x -> prim ~primitive:(Parraylength x) ~args loc
  | Parrayrefu x -> prim ~primitive:(Parrayrefu x) ~args loc
  | Parraysetu x -> prim ~primitive:(Parraysetu x) ~args loc 
  | Parrayrefs x -> prim ~primitive:(Parrayrefs x) ~args loc 
  | Parraysets x -> prim ~primitive:(Parraysets x) ~args loc 
  | Pbintofint x -> prim ~primitive:(Pbintofint x) ~args loc 
  | Pintofbint x -> prim ~primitive:(Pintofbint x) ~args loc 
  | Pnegbint x -> prim ~primitive:(Pnegbint x) ~args loc 
  | Paddbint x -> prim ~primitive:(Paddbint x) ~args loc 
  | Psubbint x -> prim ~primitive:(Psubbint x) ~args loc 
  | Pmulbint x -> prim ~primitive:(Pmulbint x) ~args loc 
  | Pdivbint x -> prim ~primitive:(Pdivbint x) ~args loc 
  | Pmodbint x -> prim ~primitive:(Pmodbint x) ~args loc 
  | Pandbint x -> prim ~primitive:(Pandbint x) ~args loc 
  | Porbint x -> prim ~primitive:(Porbint x) ~args loc 
  | Pxorbint x -> prim ~primitive:(Pxorbint x) ~args loc 
  | Plslbint x -> prim ~primitive:(Plslbint x) ~args loc 
  | Plsrbint x -> prim ~primitive:(Plsrbint x) ~args loc 
  | Pasrbint x -> prim ~primitive:(Pasrbint x) ~args loc 
  | Pbigarraydim x -> prim ~primitive:(Pbigarraydim x) ~args loc 
  | Pstring_load_16 x -> prim ~primitive:(Pstring_load_16 x) ~args loc 
  | Pstring_load_32 x -> prim ~primitive:(Pstring_load_32 x) ~args loc 
  | Pstring_load_64 x -> prim ~primitive:(Pstring_load_64 x) ~args loc 
  | Pstring_set_16 x -> prim ~primitive:(Pstring_set_16 x) ~args loc 
  | Pstring_set_32 x -> prim ~primitive:(Pstring_set_32 x) ~args loc 
  | Pstring_set_64 x -> prim ~primitive:(Pstring_set_64 x) ~args loc 
  | Pbigstring_load_16 x -> prim ~primitive:(Pbigstring_load_16 x) ~args loc 
  | Pbigstring_load_32 x -> prim ~primitive:(Pbigstring_load_32 x) ~args loc 
  | Pbigstring_load_64 x -> prim ~primitive:(Pbigstring_load_64 x) ~args loc 
  | Pbigstring_set_16 x -> prim ~primitive:(Pbigstring_set_16 x) ~args loc 
  | Pbigstring_set_32 x -> prim ~primitive:(Pbigstring_set_32 x) ~args loc 
  | Pbigstring_set_64 x -> prim ~primitive:(Pbigstring_set_64 x) ~args loc 
  | Pctconst x ->
    begin match x with
      | Word_size ->
        Lift.int 32 (* TODO: documentation*)        
      | _ -> prim ~primitive:(Pctconst x) ~args loc          
    end

  | Pbbswap x -> prim ~primitive:(Pbbswap x) ~args loc 
  | Pcvtbint (a,b) -> prim ~primitive:(Pcvtbint (a,b)) ~args loc 
  | Pbintcomp (a,b) -> prim ~primitive:(Pbintcomp (a,b)) ~args loc 
  | Pbigarrayref (a,b,c,d) -> prim ~primitive:(Pbigarrayref (a,b,c,d)) ~args loc 
  | Pbigarrayset (a,b,c,d) -> prim ~primitive:(Pbigarrayset (a,b,c,d)) ~args loc 


let rec convert (lam : Lambda.lambda) : t = 
  match lam with 
  | Lvar x -> Lvar x 
  | Lconst x -> 
    Lconst x 
  | Lapply (fn,args,loc) 
    ->  
    begin match fn with 
    | Lprim (
         Pfield (id, _),
         [
          Lprim (
            Pgetglobal { name = "CamlinternalMod" },
            _,_
          )
        ],loc
      ) -> (* replace all {!CamlinternalMod} function *)
      let args = List.map convert args in
      begin match Ocaml_stdlib_slots.camlinternalMod.(id), args  with
      | "init_mod" ,  [_loc ; shape]  -> 
          begin match shape with 
            | Lconst (Const_block (0, _, [Const_block (0, _, [])])) 
              -> unit  (* see {!Translmod.init_shape}*)
            | _ ->  prim ~primitive:Pinit_mod ~args loc 
          end
      | "update_mod", [shape ;  _obj1; _obj2] -> 
            (* here array access will have side effect .. *)
            begin match shape with 
            | Lconst (Const_block (0, _, [Const_block (0, _, [])]))
              -> unit (* see {!Translmod.init_shape}*)
            | _ -> prim ~primitive:Pupdate_mod ~args loc
            end
      | _ -> assert false
      end

    | Lprim ( Pfield (id, _),
              [Lprim (Pgetglobal ({name  = "Pervasives"} ), _,_)],loc              
            )
      ->
      let args = List.map convert args in
      begin match Ocaml_stdlib_slots.pervasives.(id) , args  with
        | "^", [ l; r ] 
          ->
          prim ~primitive:Pstringadd ~args:[l;r] loc 
        | _ ->  apply (convert fn) args loc  App_na
      end
    | _ -> 
        apply (convert fn) (List.map convert args) 
          loc App_na
    end
  | Lfunction (kind,  params,body)
    ->  function_ 
          ~arity:(List.length params) ~kind ~params 
          ~body:(convert body)
  | Llet (kind,id,e,body) 
    -> Llet(kind,id,convert e, convert body)
  | Lletrec (bindings,body)
    -> 
    Lletrec (List.map (fun (id, e) -> id, convert e) bindings, convert body)
  | Lprim (primitive,args, loc) 
    -> convert_primitive loc primitive args 
    (* Lprim {primitive ; args = List.map convert args } *)
  | Lswitch (e,s) -> 
    Lswitch (convert e, convert_switch s)
  | Lstringswitch (e, cases, default,_) -> 
    Lstringswitch (convert e, List.map (fun (x, b) -> x, convert b ) cases, 
                   match default with 
                   | None -> None
                   | Some x -> Some (convert x)
                  )    

  | Lstaticraise (id, args) -> 
    Lstaticraise (id, List.map convert args)
  | Lstaticcatch (b, (i, ids), handler) -> 
    Lstaticcatch (convert b, (i,ids), convert handler)
  | Ltrywith (b, id, handler) -> 
    Ltrywith (convert b, id, convert handler)
  | Lifthenelse (b,then_,else_) -> 
    Lifthenelse (convert b, convert then_, convert else_)
  | Lsequence (a,b) 
    -> Lsequence (convert a, convert b)
  | Lwhile (b,body) -> 
    Lwhile (convert b, convert body)
  | Lfor (id, from_, to_, dir, loop) -> 
    Lfor (id, convert from_, convert to_, dir, convert loop)
  | Lassign (id, body) -> 
    Lassign (id, convert body)    
  | Lsend (kind, a,b,ls, loc) -> 
    (* Format.fprintf Format.err_formatter "%a@." Printlambda.lambda b ; *)
    begin match convert b with 
      | Lprim {primitive =  Pccall {prim_name };  args; loc}
        when prim_name = Literals.js_unsafe_downgrade
        -> 
        begin match kind, ls with 
          | Public (Some name), [] -> 
            prim ~primitive:(Pjs_unsafe_downgrade (name,loc)) 
              ~args loc 
          | _ -> assert false 
        end
      | b ->     
        (* Format.fprintf Format.err_formatter "weird: %d@." (Obj.tag (Obj.repr b));  *)
        Lsend(kind, convert a,  b, List.map convert ls, loc )
    end
  | Levent (e, event) -> convert e 
  | Lifused (id, e) -> 
    Lifused(id, convert e) (* TODO: remove it ASAP *)
and convert_primitive loc (primitive : Lambda.primitive) args = 
  lam_prim ~primitive ~args:(List.map convert args) loc
and convert_switch (s : Lambda.lambda_switch) : switch = 
  { sw_numconsts = s.sw_numconsts ; 
    sw_consts = List.map (fun (i, lam) -> i, convert lam) s.sw_consts;
    sw_numblocks = s.sw_numblocks;
    sw_blocks = List.map (fun (i,lam) -> i, convert lam ) s.sw_blocks;
    sw_failaction = 
      match s.sw_failaction with 
      | None -> None 
      | Some a -> Some (convert a)
  }  
