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

type primitive = Lambda.primitive
type switch = 
  { sw_numconsts: int;
    sw_consts: (int * t) list;
    sw_numblocks: int;
    sw_blocks: (int * t) list;
    sw_failaction : t option}
and t = 
  | Lvar of Ident.t
  | Lconst of Lambda.structured_constant
  | Lapply of t * t list * Lambda.apply_info
  | Lfunction of int * Lambda.function_kind  * Ident.t list * t
  | Llet of Lambda.let_kind * Ident.t * t * t
  | Lletrec of (Ident.t * t) list * t
  | Lprim of primitive * t list * int 
  | Lswitch of t * switch
  | Lstringswitch of t * (string * t) list * t option
  | Lstaticraise of int * t list
  | Lstaticcatch of t * (int * Ident.t list) * t
  | Ltrywith of t * Ident.t * t
  | Lifthenelse of t * t * t
  | Lsequence of t * t
  | Lwhile of t * t
  | Lfor of Ident.t * t * t * Asttypes.direction_flag * t
  | Lassign of Ident.t * t
  | Lsend of Lambda.meth_kind * t * t * t list * Location.t
  | Levent of t * Lambda.lambda_event
  | Lifused of Ident.t * t


module Prim = struct 
  type t = primitive
  let js_is_nil : t = 
    Pccall{ prim_name = "js_is_nil";
                   prim_arity = 1 ;
                   prim_alloc = false;
                   prim_native_name = "js_is_nil";
                   prim_native_float = false;
                   prim_attributes = [];
                   prim_ty = None
                 }

  let js_is_undef : t = 
    Pccall{ prim_name = "js_is_undef";
                   prim_arity = 1 ;
                   prim_alloc = false;
                   prim_native_name = "js_is_undef";
                   prim_native_float = false;
                   prim_attributes = [];
                   prim_ty = None
                 }

  let js_is_nil_undef : t  = 
    Pccall{ prim_name = "js_is_nil_undef";
                   prim_arity = 1 ;
                   prim_alloc = false;
                   prim_native_name = "js_is_nil_undef";
                   prim_native_float = false;
                   prim_attributes = [];
                   prim_ty = None
                 }

end






type binop = t -> t -> t 

type triop = t -> t -> t -> t 

type unop = t -> t 


let var id : t = Lvar id
let const ct : t = Lconst ct 
let apply fn args info : t = Lapply(fn,args, info)
let function_ arity kind ids body : t = 
  Lfunction(arity, kind, ids, body)

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

let switch lam lam_switch : t = 
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

let lift_int i : t =
  Lconst (Const_base (Const_int i))


let int32 i : t =
  Lconst (Const_base (Const_int32 i))

let lift_bool b = if b then true_ else false_

(* ATTENTION: [float, nativeint] constant propogaton is not done
   yet , due to cross platform problem
*) 
let lift_float b  : t = 
  Lconst (Const_base (Const_float b))

let lift_nativeint b : t = 
  Lconst (Const_base (Const_nativeint b))

let lift_int32 b : t = 
  Lconst (Const_base (Const_int32 b))

let lift_int64 b : t =
  Lconst (Const_base (Const_int64 b))

let prim (prim : Prim.t) (ll : t list) len : t = 
  let default () : t = Lprim(prim,ll, len) in 
  match ll with 
  | [Lconst a] -> 
    begin match prim, a  with 
      | Pnegint, (Const_base (Const_int a))
        -> lift_int (- a)
      (* | Pfloatofint, (Const_base (Const_int a)) *)
      (*   -> lift_float (float_of_int a) *)
      | Pintoffloat, (Const_base (Const_float a))
        -> 
        lift_int (int_of_float (float_of_string a))
        (* | Pnegfloat -> lift_float (-. a) *)
        (* | Pabsfloat -> lift_float (abs_float a) *)
      | Pstringlength, (Const_base (Const_string (a,_)) ) 
        -> 
        lift_int (String.length a)
      (* | Pnegbint Pnativeint, (Const_base (Const_nativeint i)) *)
      (*   ->   *)
      (*   lift_nativeint (Nativeint.neg i) *)
      | Pnegbint Pint32, (Const_base (Const_int32 a))
        -> 
        lift_int32 (Int32.neg a)
      | Pnegbint Pint64, (Const_base (Const_int64 a))
        -> 
        lift_int64 (Int64.neg a)
      | Pnot , Const_pointer (a,_) 
        -> lift_bool (a = 0 )

      | _ -> default ()
    end


  | [Lconst a ; Lconst b] -> 
    begin match prim, a, b  with 
      | Pbintcomp(_, cmp), Const_base (Const_int32 a), Const_base (Const_int32 b)
        -> lift_bool (comparison cmp a b)
      | Pbintcomp(_, cmp), Const_base (Const_int64 a), Const_base (Const_int64 b)
        -> lift_bool (comparison cmp a b)
      | Pbintcomp(_, cmp), Const_base (Const_nativeint a), Const_base (Const_nativeint b)
        -> lift_bool (comparison cmp a b)
      | Pfloatcomp  cmp, Const_base (Const_nativeint a), Const_base (Const_nativeint b)
        -> lift_bool (comparison cmp a b)
      | Pintcomp cmp , Const_base (Const_int a), Const_base (Const_int b)
        -> lift_bool (comparison cmp a b)

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
            let int_ v = lift_int (Int32.to_int v ) in 
            begin match prim with 
              | Paddint -> int_ (Int32.add aa bb)
              | Psubint -> int_ (Int32.sub aa bb)
              | Pmulint -> int_ (Int32.mul aa  bb)
              | Pdivint -> (try int_ (Int32.div aa  bb) with _ -> default ())
              | Pmodint -> int_ (Int32.rem aa  bb)
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
          | Paddbint _  -> lift_int32 (Int32.add aa bb)
          | Psubbint _  -> lift_int32 (Int32.sub aa bb)
          | Pmulbint _ -> lift_int32 (Int32.mul aa  bb)
          | Pdivbint _ ->  (try lift_int32 (Int32.div aa  bb) with _  -> default ())
          | Pmodbint _ -> lift_int32 (Int32.rem aa  bb)
          | Pandbint _ -> lift_int32 (Int32.logand aa bb)
          | Porbint _ -> lift_int32 (Int32.logor aa bb)
          | Pxorbint _ -> lift_int32 (Int32.logxor aa bb)
          | _ -> default ()
        end
      | Plslbint Pint32, Const_base (Const_int32 aa), Const_base (Const_int b)
        -> lift_int32 (Int32.shift_left  aa b )
      | Plsrbint Pint32, Const_base (Const_int32 aa), Const_base (Const_int b)
        -> lift_int32 (Int32.shift_right_logical  aa b )
      | Pasrbint Pint32, Const_base (Const_int32 aa), Const_base (Const_int b)
        -> lift_int32 (Int32.shift_right  aa b )

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
          | Paddbint _  -> lift_int64 (Int64.add aa bb)
          | Psubbint _  -> lift_int64 (Int64.sub aa bb)
          | Pmulbint _ -> lift_int64 (Int64.mul aa  bb)
          | Pdivbint _ -> (try lift_int64 (Int64.div aa  bb) with _ -> default ())
          | Pmodbint _ -> lift_int64 (Int64.rem aa  bb)
          | Pandbint _ -> lift_int64 (Int64.logand aa bb)
          | Porbint _ -> lift_int64 (Int64.logor aa bb)
          | Pxorbint _ -> lift_int64 (Int64.logxor aa bb)
          | _ -> default ()
        end
      | Plslbint Pint64, Const_base (Const_int64 aa), Const_base (Const_int b)
        -> lift_int64 (Int64.shift_left  aa b )
      | Plsrbint Pint64, Const_base (Const_int64 aa), Const_base (Const_int b)
        -> lift_int64 (Int64.shift_right_logical  aa b )
      | Pasrbint Pint64, Const_base (Const_int64 aa), Const_base (Const_int b)
        -> lift_int64 (Int64.shift_right  aa b )
      | Psequand, Const_pointer (a, _), Const_pointer( b, _)
        -> 
        lift_bool (a = 1 && b = 1)
      | Psequor, Const_pointer (a, _), Const_pointer( b, _)
        -> 
        lift_bool (a = 1 || b = 1)
      | _ -> default ()
    end

  | _ -> default ()


let not x : t = 
  prim Pnot [x] 1 


let rec convert (lam : Lambda.lambda) : t = 
  match lam with 
  | Lvar x -> Lvar x 
  | Lconst x -> 
    Lconst x 
  | Lapply (fn,args,info) 
    -> Lapply(convert fn,List.map convert args,info)
  | Lfunction (kind,ids,body)
    ->  function_ (List.length ids) kind ids (convert body)
  | Llet (kind,id,e,body) 
    -> Llet(kind,id,convert e, convert body)
  | Lletrec (bindings,body)
    -> 
    Lletrec (List.map (fun (id, e) -> id, convert e) bindings, convert body)
  | Lprim (prim,args) 
    -> 
    Lprim (prim,List.map convert args, List.length args)
  | Lswitch (e,s) -> 
    Lswitch (convert e, convert_switch s)
  | Lstringswitch (e, cases, default) -> 
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
    Lsend(kind, convert a, convert b, List.map convert ls, loc )

  | Levent (e, event) -> 
    Levent (convert e, event)
  | Lifused (id, e) -> Lifused(id, convert e)

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
