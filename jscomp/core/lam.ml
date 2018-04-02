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
  (*TODO: only [Pfloatarray] makes sense *)
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

type function_kind
  = Curried
(* | Tupled *)


type let_kind = Lambda.let_kind
= Strict
| Alias
| StrictOpt
| Variable

type meth_kind = Lambda.meth_kind
= Self
| Public of string option
| Cached

type constant =
  | Const_js_null
  | Const_js_undefined
  | Const_js_true
  | Const_js_false
  | Const_int of int
  | Const_char of char
  | Const_string of string  (* use record later *)
  | Const_unicode of string
  | Const_float of string
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint
  | Const_pointer of int * Lambda.pointer_info
  | Const_block of int * Lambda.tag_info * constant list
  | Const_float_array of string list
  | Const_immstring of string

type primitive =
  | Pbytes_to_string
  | Pbytes_of_string
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
  | Pjs_call of
      string *  (* prim_name *)
      External_arg_spec.t list * (* arg_types *)
      External_ffi_types.attr  (* ffi *)
  | Pjs_object_create of External_ffi_types.obj_create
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
  | Pjscomp of comparison
  | Pjs_apply (*[f;arg0;arg1; arg2; ... argN]*)
  | Pjs_runtime_apply (* [f; [...]] *)
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
  | Praw_js_code_exp of string
  | Praw_js_code_stmt of string
  | Pjs_fn_make of int
  | Pjs_fn_run of int
  | Pjs_fn_method of int
  | Pjs_fn_runmethod of int

  | Pundefined_to_opt
  | Pnull_to_opt
  | Pnull_undefined_to_opt
  | Pis_null
  | Pis_undefined
  | Pis_null_undefined
  | Pjs_typeof
  | Pjs_function_length

  | Pjs_string_of_small_array
  (* | Pjs_is_instance_array *)
  | Pcaml_obj_length
  | Pcaml_obj_set_length
  | Pwrap_exn (* convert either JS exception or OCaml exception into OCaml format *)

  (* | Pcreate_exception of string  *)
  | Pcreate_extension of string

type apply_status =
  | App_na
  | App_ml_full
  | App_js_full


module Types = struct
  type switch =
    { sw_numconsts: int;
      sw_consts: (int * t) list;
      sw_numblocks: int;
      sw_blocks: (int * t) list;
      sw_failaction : t option}
  (* Note that failaction would appear in both
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

     but there are some edge cases (see MPR#6033)
     one predicate used is
     {[
       (sw.sw_numconsts - List.length sw.sw_consts) +
       (sw.sw_numblocks - List.length sw.sw_blocks) > 1
     ]}
     if [= 1] with [some fail] -- called once
     if [= 0] could not have [some fail]
  *)
  and prim_info =
    { primitive : primitive ;
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
      function_kind : function_kind ;
      params : ident list ;
      body : t
    }
  and t =
    | Lvar of ident
    | Lglobal_module of ident
    | Lconst of constant
    | Lapply of apply_info
    | Lfunction of function_info
    | Llet of let_kind * ident * t * t
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
    | Lsend of meth_kind * t * t * t list * Location.t
    | Lifused of ident * t
end

module X = struct
  type switch
    = Types.switch
    =
      { sw_numconsts: int;
        sw_consts: (int * t) list;
        sw_numblocks: int;
        sw_blocks: (int * t) list;
        sw_failaction : t option}
  and prim_info
    =  Types.prim_info
    =
      { primitive : primitive ;
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
        function_kind : function_kind ;
        params : ident list ;
        body : t
      }
  and t
    = Types.t
    =
      | Lvar of ident
      | Lglobal_module of ident
      | Lconst of constant
      | Lapply of apply_info
      | Lfunction of function_info
      | Llet of let_kind * ident * t * t
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
      | Lsend of meth_kind * t * t * t list * Location.t
      | Lifused of ident * t
end
include Types
(** apply [f] to direct successor which has type [Lam.t] *)
let inner_map (f : t -> X.t ) (l : t) : X.t =
  match l  with
  | Lvar (_ : ident)
  | Lconst (_ : constant) ->
    ( (* Obj.magic *) l : X.t)
  | Lapply ({fn; args; loc; status} )  ->
    let fn = f fn in
    let args = Ext_list.map f args in
    Lapply { fn ; args; loc; status }
  | Lfunction({body; arity; function_kind; params } ) ->
    let body = f body in
    Lfunction {body; arity; function_kind ; params}
  | Llet(str, id, arg, body) ->
    let arg = f arg in let body =  f body in
    Llet(str,id,arg,body)
  | Lletrec(decl, body) ->
    let body = f body in
    let decl = Ext_list.map (fun (id, exp) -> id, f exp) decl in
    Lletrec(decl,body)
  | Lglobal_module _ -> (l : X.t)
  | Lprim {args; primitive ; loc}  ->
    let args = Ext_list.map f args in
    Lprim { args; primitive; loc}

  | Lswitch(arg, {sw_consts; sw_numconsts; sw_blocks; sw_numblocks; sw_failaction}) ->
    let arg = f arg in
    let sw_consts = Ext_list.map (fun (key, case) -> key , f case) sw_consts in
    let sw_blocks = Ext_list.map (fun (key, case) -> key, f case) sw_blocks in
    let sw_failaction = begin match sw_failaction with
      | None -> None
      | Some a -> Some (f a)
    end in
    Lswitch(arg, { sw_consts; sw_blocks; sw_failaction; sw_numblocks; sw_numconsts})
  | Lstringswitch (arg,cases,default) ->
    let arg = f arg  in
    let cases = Ext_list.map (fun (k,act) -> k,f act) cases  in
    let default = begin match default with
      | None -> None
      | Some a -> Some (f a)
    end in
    Lstringswitch(arg,cases,default)
  | Lstaticraise (id,args) ->
    let args = Ext_list.map f args in
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
    let args = Ext_list.map f args in
    Lsend(k,met,obj,args,loc)

  | Lifused (v, e) ->
    let e = f e in
    Lifused(v,e)

let inner_iter (f : t -> unit ) (l : t) : unit =
  match l  with
  | Lvar (_ : ident)
  | Lconst (_ : constant) -> ()
  | Lapply ({fn; args; loc; status} )  ->
    f fn;
    List.iter f args
  | Lfunction({body; arity; function_kind; params } ) ->
    f body
  | Llet(str, id, arg, body) ->
    f arg ;
    f body;
  | Lletrec(decl, body) ->
    f body;
    List.iter (fun (id, exp) ->  f exp) decl
  | Lglobal_module (_ )
    ->  ()
  | Lprim {args; primitive ; loc}  ->
    List.iter f args;
  | Lswitch(arg, {sw_consts; sw_numconsts; sw_blocks; sw_numblocks; sw_failaction}) ->
    f arg;
    List.iter (fun (key, case) -> f case) sw_consts;
    List.iter (fun (key, case) ->  f case) sw_blocks ;
    begin match sw_failaction with
      | None -> ()
      | Some a ->  f a
    end
  | Lstringswitch (arg,cases,default) ->
    f arg;
    List.iter (fun (k,act) -> f act) cases  ;
    begin match default with
      | None -> ()
      | Some a -> f a
    end
  | Lstaticraise (id,args) ->
    List.iter f args;
  | Lstaticcatch(e1, vars , e2) ->
    f e1;
    f e2
  | Ltrywith(e1, exn, e2) ->
    f e1;
    f e2
  | Lifthenelse(e1, e2, e3) ->
    f e1;  f e2 ;  f e3
  | Lsequence(e1, e2) ->
    f e1 ;  f e2
  | Lwhile(e1, e2) ->
    f e1 ;  f e2
  | Lfor(v, e1, e2, dir, e3) ->
    f e1 ;  f e2;  f e3
  | Lassign(id, e) ->
    f e
  | Lsend (k, met, obj, args, loc) ->
    f met; f obj; List.iter f args
  | Lifused (v, e) ->
    f e


(*
let add_list lst set =
    List.fold_left (fun acc x -> Ident_set.add x acc) set lst
let free_variables l =
  let rec free bounded acc (l : t) =
      match (l : t) with
      | Lvar id ->
        if Ident_set.mem id bounded then acc
        else Ident_set.add id acc
      | Lconst _ -> acc
      | Lapply{fn; args; _} ->
        let acc = free bounded  acc fn in
        List.fold_left (fun acc arg -> free bounded acc arg) acc args
      | Lfunction{body;params} ->
        let bounded = add_list params bounded in
        free bounded acc  body
      | Llet(str, id, arg, body) ->
        let acc = free bounded acc  arg in
        let bounded =  Ident_set.add id bounded in
        free bounded acc body
      | Lletrec(decl, body) ->
        let bounded =
          List.fold_left (fun acc (x,_) -> Ident_set.add x acc) bounded decl
        in
        let acc = List.fold_left (fun acc (_,exp) -> free bounded acc exp ) acc decl in
        free bounded acc body
      | Lprim {args; _} ->
        List.fold_left (fun acc arg -> free bounded acc arg) acc args
      | Lswitch(arg, {sw_consts; sw_blocks; sw_failaction}) ->
        let acc = free bounded acc arg in
        let acc = List.fold_left
          (fun acc (key, case) -> free  bounded acc case) acc sw_consts in
        let acc =
          List.fold_left
          (fun acc (key, case) -> free bounded acc  case) acc sw_blocks in
        begin match sw_failaction with
          | None -> acc
          | Some a -> free bounded acc a
        end
      | Lstringswitch (arg,cases,default) ->
        let acc = free bounded acc arg  in
        let acc = List.fold_left (fun acc  (_,act) -> free bounded acc act) acc cases  in
        begin match default with
          | None -> acc
          | Some a -> free bounded acc a
        end
      | Lstaticraise (_,args) ->
        List.fold_left (fun acc arg -> free bounded acc arg) acc args
      | Lstaticcatch(e1, (_,vars), e2) ->
        let acc = free  bounded acc e1 in
        let bounded = add_list vars bounded in
        free bounded acc e2
      | Ltrywith(e1, exn, e2) ->
        let acc = free  bounded acc e1 in
        let bounded = Ident_set.add exn bounded in
        free  bounded acc e2
      | Lifthenelse(e1, e2, e3) ->
        let acc = free  bounded acc e1 in
        let acc = free  bounded acc e2 in
        free bounded acc e3
      | Lwhile(e1, e2)
      | Lsequence(e1, e2) ->
        let acc = free bounded acc e1 in
        free bounded acc e2
      | Lfor(v, e1, e2, dir, e3) ->

        let acc = free  bounded acc e1 in
        let acc = free  bounded acc e2 in
        let bounded = Ident_set.add v bounded in
        free bounded acc e3
      | Lassign(id, e) ->
        let acc = free bounded acc  e in
        if Ident_set.mem id bounded then acc
        else Ident_set.add id acc
      | Lsend (k, met, obj, args, _) ->
        let acc = free bounded acc met in
        let acc = free bounded acc obj in
        List.fold_left (fun ac arg -> free bounded acc arg) acc args
      | Lifused (v, e) ->
        free bounded acc e
  in free Ident_set.empty Ident_set.empty l
*)

(**
        [hit_any_variables fv l]
        check the lambda expression [l] if has some free
        variables captured by [fv].
        Note it does not do any checking like below
        [Llet(str,id,arg,body)]
        it only check [arg] or [body] is hit or not, there
        is a case that [id] is hit in [arg] but also exists
        in [fv], this is ignored.
*)
let hit_any_variables (fv : Ident_set.t) l : bool  =
  let rec hit (l : t) =
    begin
      match (l : t) with
      | Lvar id -> Ident_set.mem id fv
      | Lassign(id, e) ->
        Ident_set.mem id fv || hit e
      | Lstaticcatch(e1, (_,vars), e2) ->
        hit e1 || hit e2
      | Ltrywith(e1, exn, e2) ->
        hit e1 || hit e2
      | Lfunction{body;params} ->
        hit body;
      | Llet(str, id, arg, body) ->
        hit arg || hit body
      | Lletrec(decl, body) ->
        hit body ||
        List.exists (fun (id, exp) -> hit exp) decl
      | Lfor(v, e1, e2, dir, e3) ->
        hit e1 || hit e2 || hit e3
      | Lconst _ -> false
      | Lapply{fn; args; _} ->
        hit fn || List.exists hit args
      | Lglobal_module _  (* global persistent module, play safe *)
        -> false
      | Lprim {args; _} ->
        List.exists hit args
      | Lswitch(arg, sw) ->
        hit arg ||
        List.exists (fun (key, case) -> hit case) sw.sw_consts ||
        List.exists (fun (key, case) -> hit case) sw.sw_blocks ||
        begin match sw.sw_failaction with
          | None -> false
          | Some a -> hit a
        end
      | Lstringswitch (arg,cases,default) ->
        hit arg ||
        List.exists (fun (_,act) -> hit act) cases ||
        begin match default with
          | None -> false
          | Some a -> hit a
        end
      | Lstaticraise (_,args) ->
        List.exists hit args
      | Lifthenelse(e1, e2, e3) ->
        hit e1 || hit e2 || hit e3
      | Lsequence(e1, e2) ->
        hit e1 || hit e2
      | Lwhile(e1, e2) ->
        hit e1 || hit e2
      | Lsend (k, met, obj, args, _) ->
        hit met || hit obj || List.exists hit args
      | Lifused (v, e) ->
        hit e
    end;
  in hit l

(** A conservative approach to avoid packing exceptions
    for lambda expression like {[
      try { ... }catch(id){body}
    ]}
    we approximate that if [id] is destructed or not.
    If it is destructed, we need pack it in case it is JS exception.
    Note it is not guaranteed that exception raised(or re-raised) is a structured
    ocaml exception but it is guaranteed that if such exception is processed it would
    still be an ocaml exception.
    for example {[
      match x with
      | exception e -> raise e
    ]}
    it will re-raise an exception as it is (we are not packing it anywhere)

    It is hard to judge an exception is destructed or escaped, any potential
    alias(or if it is passed as an argument) would cause it to be leaked
*)
let exception_id_escaped (fv : Ident.t) l : bool  =
  let rec hit (l : t) =
    begin
      match (l : t) with
      | Lprim {primitive = Pintcomp _ ;
               args = ([x;y ])  } ->
        begin match x,y with
          | Lvar _, Lvar _ -> false
          | Lvar _, _ -> hit y
          | _, Lvar _ -> hit x
          | _, _  -> hit x || hit y
        end
      | Lprim {primitive = Praise ; args = [Lvar _]} -> false
      | Lprim {primitive ; args; _} ->
        List.exists hit args
      | Lvar id ->
        Ext_log.dwarn __LOC__ "[HIT]%s/%d@." id.name id.stamp ;
        Ident.same id fv
      | Lassign(id, e) ->
        Ident.same id fv || hit e
      | Lstaticcatch(e1, (_,vars), e2) ->
        hit e1 || hit e2
      | Ltrywith(e1, exn, e2) ->
        hit e1 || hit e2
      | Lfunction{body;params} ->
        hit body;
      | Llet(str, id, arg, body) ->
        hit arg || hit body
      | Lletrec(decl, body) ->
        hit body ||
        List.exists (fun (id, exp) -> hit exp) decl
      | Lfor(v, e1, e2, dir, e3) ->
        hit e1 || hit e2 || hit e3
      | Lconst _ -> false
      | Lapply{fn; args; _} ->
        hit fn || List.exists hit args
      | Lglobal_module _  (* global persistent module, play safe *)
        -> false
      | Lswitch(arg, sw) ->
        hit arg ||
        List.exists (fun (key, case) -> hit case) sw.sw_consts ||
        List.exists (fun (key, case) -> hit case) sw.sw_blocks ||
        begin match sw.sw_failaction with
          | None -> false
          | Some a -> hit a
        end
      | Lstringswitch (arg,cases,default) ->
        hit arg ||
        List.exists (fun (_,act) -> hit act) cases ||
        begin match default with
          | None -> false
          | Some a -> hit a
        end
      | Lstaticraise (_,args) ->
        List.exists hit args
      | Lifthenelse(e1, e2, e3) ->
        hit e1 || hit e2 || hit e3
      | Lsequence(e1, e2) ->
        hit e1 || hit e2
      | Lwhile(e1, e2) ->
        hit e1 || hit e2
      | Lsend (k, met, obj, args, _) ->
        hit met || hit obj || List.exists hit args
      | Lifused (v, e) ->
        hit e
    end;
  in hit l


(**
    [hit_mask mask lambda] iters through the lambda
    set the bit of corresponding [id] if [id] is hit.
    As an optimization step if [mask_check_all_hit],
    there is no need to iter such lambda any more
*)
let hit_mask ( mask : Hash_set_ident_mask.t) l =
  let rec hit (l : t) =
    match (l : t) with
    | Lvar id -> Hash_set_ident_mask.mask_check_all_hit id mask
    | Lassign(id, e) ->
      Hash_set_ident_mask.mask_check_all_hit id mask || hit e
    | Lstaticcatch(e1, (_,vars), e2) ->
      hit e1 || hit e2
    | Ltrywith(e1, exn, e2) ->
      hit e1 || hit e2
    | Lfunction{body;params} ->
      hit body;
    | Llet(str, id, arg, body) ->
      hit arg || hit body
    | Lletrec(decl, body) ->
      hit body ||
      List.exists (fun (id, exp) -> hit exp) decl
    | Lfor(v, e1, e2, dir, e3) ->
      hit e1 || hit e2 || hit e3
    | Lconst _ -> false
    | Lapply{fn; args; _} ->
      hit fn || List.exists hit args
    | Lglobal_module id (* playsafe *)
      -> false
    | Lprim {args; _} ->
      List.exists hit args
    | Lswitch(arg, sw) ->
      hit arg ||
      List.exists hit_case sw.sw_consts ||
      List.exists hit_case sw.sw_blocks ||
      begin match sw.sw_failaction with
        | None -> false
        | Some a -> hit a
      end
    | Lstringswitch (arg,cases,default) ->
      hit arg ||
      List.exists hit_case cases ||
      begin match default with
        | None -> false
        | Some a -> hit a
      end
    | Lstaticraise (_,args) ->
      List.exists hit args
    | Lifthenelse(e1, e2, e3) ->
      hit e1 || hit e2 || hit e3
    | Lsequence(e1, e2) ->
      hit e1 || hit e2
    | Lwhile(e1, e2) ->
      hit e1 || hit e2
    | Lsend (k, met, obj, args, _) ->
      hit met || hit obj || List.exists hit args
    | Lifused (v, e) ->
      hit e

  and hit_case : 'a. 'a * _ -> bool = fun  (_,case) -> hit case
  in hit l

let free_variables l =
  let fv = ref Ident_set.empty in
  let rec free (l : t) =
    begin
      match (l : t) with
      | Lvar id -> fv := Ident_set.add id !fv
      | Lassign(id, e) ->
        free e;
        fv := Ident_set.add id !fv
      | Lstaticcatch(e1, (_,vars), e2) ->
        free e1; free e2;
        List.iter (fun id -> fv := Ident_set.remove id !fv) vars
      | Ltrywith(e1, exn, e2) ->
        free e1; free e2;
        fv := Ident_set.remove exn !fv
      | Lfunction{body;params} ->
        free body;
        List.iter (fun param -> fv := Ident_set.remove param !fv) params
      | Llet(str, id, arg, body) ->
        free arg; free body;
        fv := Ident_set.remove id !fv
      | Lletrec(decl, body) ->
        free body;
        List.iter (fun (id, exp) -> free exp) decl;
        List.iter (fun (id, exp) -> fv := Ident_set.remove id !fv) decl
      | Lfor(v, e1, e2, dir, e3) ->
        free e1; free e2; free e3;
        fv := Ident_set.remove v !fv
      | Lconst _ -> ()
      | Lapply{fn; args; _} ->
        free fn; List.iter free args
      | Lglobal_module _ -> ()
      (* according to the existing semantics:
         [primitive] is not counted
      *)
      | Lprim {args; _} ->
        List.iter free args
      | Lswitch(arg, sw) ->
        free arg;
        List.iter (fun (key, case) -> free case) sw.sw_consts;
        List.iter (fun (key, case) -> free case) sw.sw_blocks;
        begin match sw.sw_failaction with
          | None -> ()
          | Some a -> free a
        end
      | Lstringswitch (arg,cases,default) ->
        free arg ;
        List.iter (fun (_,act) -> free act) cases ;
        begin match default with
          | None -> ()
          | Some a -> free a
        end
      | Lstaticraise (_,args) ->
        List.iter free args
      | Lifthenelse(e1, e2, e3) ->
        free e1; free e2; free e3
      | Lsequence(e1, e2) ->
        free e1; free e2
      | Lwhile(e1, e2) ->
        free e1; free e2
      | Lsend (k, met, obj, args, _) ->
        free met; free obj; List.iter free args
      | Lifused (v, e) ->
        free e
    end;
  in free l;
  !fv


(**
        [no_bounded_varaibles lambda]
        checks if [lambda] contains bounded variable, for
        example [Llet (str,id,arg,body) ] will fail such check.
        This is used to indicate such lambda expression if it is okay
        to inline directly since if it contains bounded variables it
        must be rebounded before inlining
*)
let rec no_bounded_variables (l : t) =
  match (l : t) with
  | Lvar _ -> true
  | Lconst _ -> true
  | Lassign(_id, e) ->
    no_bounded_variables e
  | Lapply{fn; args; _} ->
    no_bounded_variables fn && List.for_all no_bounded_variables args
  | Lglobal_module _ -> true
  | Lprim {args; primitive = _ ; } ->
    List.for_all no_bounded_variables args
  | Lswitch(arg, sw) ->
    no_bounded_variables arg &&
    List.for_all (fun (key, case) -> no_bounded_variables case) sw.sw_consts &&
    List.for_all (fun (key, case) -> no_bounded_variables case) sw.sw_blocks &&
    begin match sw.sw_failaction with
      | None -> true
      | Some a -> no_bounded_variables a
    end
  | Lstringswitch (arg,cases,default) ->
    no_bounded_variables arg &&
    List.for_all (fun (_,act) -> no_bounded_variables act) cases &&
    begin match default with
      | None -> true
      | Some a -> no_bounded_variables a
    end
  | Lstaticraise (_,args) ->
    List.for_all no_bounded_variables args
  | Lifthenelse(e1, e2, e3) ->
    no_bounded_variables e1 && no_bounded_variables e2 && no_bounded_variables e3
  | Lsequence(e1, e2) ->
    no_bounded_variables e1 && no_bounded_variables e2
  | Lwhile(e1, e2) ->
    no_bounded_variables e1 && no_bounded_variables e2
  | Lsend (k, met, obj, args, _) ->
    no_bounded_variables met  &&
    no_bounded_variables obj &&
    List.for_all no_bounded_variables args
  | Lifused (v, e) ->
    no_bounded_variables e


  | Lstaticcatch(e1, (_,vars), e2) ->
    vars = [] && no_bounded_variables e1 &&  no_bounded_variables e2
  | Lfunction{body;params} ->
    params = [] && no_bounded_variables body;
  | Lfor _  -> false
  | Ltrywith _ -> false
  | Llet _ ->false
  | Lletrec(decl, body) -> decl = [] && no_bounded_variables body




(**
   checks
   1. variables are not bound twice
   2. all variables are of right scope
*)
let check file lam =
  let defined_variables = Ident_hash_set.create 1000 in
  let success = ref true in
  let use (id : Ident.t)  =
    if not @@ Ident_hash_set.mem defined_variables id  then
      begin
        Format.fprintf Format.err_formatter "\n[SANITY]:%s/%d used before defined in %s\n" id.name id.stamp file ;
        success := false
      end
  in
  let def (id : Ident.t) =
    if Ident_hash_set.mem defined_variables id  then
      begin
        Format.fprintf Format.err_formatter "\n[SANITY]:%s/%d bound twice in %s\n" id.name id.stamp  file ;
        success := false
      end
    else Ident_hash_set.add defined_variables id
  in
  let rec iter (l : t) =
    begin
      match (l : t) with
      | Lvar id -> use id
      | Lglobal_module _ -> ()
      | Lprim {args; _} ->
        List.iter iter args
      | Lconst _ -> ()
      | Lapply{fn; args; _} ->
        iter fn; List.iter iter args
      | Lfunction{body;params} ->
        List.iter def params;
        iter body
      | Llet(str, id, arg, body) ->
        iter arg;
        def id;
        iter body
      | Lletrec(decl, body) ->
        List.iter (fun (id, exp) ->  def id) decl;
        List.iter (fun (id, exp) -> iter exp) decl;
        iter body

      | Lswitch(arg, sw) ->
        iter arg;
        List.iter (fun (key, case) -> iter case) sw.sw_consts;
        List.iter (fun (key, case) -> iter case) sw.sw_blocks;
        begin match sw.sw_failaction with
          | None -> ()
          | Some a -> iter a
        end
      | Lstringswitch (arg,cases,default) ->
        iter arg ;
        List.iter (fun (_,act) -> iter act) cases ;
        begin match default with
          | None -> ()
          | Some a -> iter a
        end
      | Lstaticraise (_,args) ->
        List.iter iter args
      | Lstaticcatch(e1, (_,vars), e2) ->
        iter e1;
        List.iter def vars;
        iter e2
      | Ltrywith(e1, exn, e2) ->
        iter e1;
        def exn;
        iter e2
      | Lifthenelse(e1, e2, e3) ->
        iter e1; iter e2; iter e3
      | Lsequence(e1, e2) ->
        iter e1; iter e2
      | Lwhile(e1, e2) ->
        iter e1; iter e2
      | Lfor(v, e1, e2, dir, e3) ->
        iter e1; iter e2;
        def v;
        iter e3;
      | Lassign(id, e) ->
        use id ;
        iter e
      | Lsend (k, met, obj, args, _) ->
        iter met; iter obj;
        List.iter iter args
      | Lifused (v, e) ->
        iter e
    end;
  in
  begin
    iter lam;
    assert (!success) ;
    lam
  end

type binop = t -> t -> t

type triop = t -> t -> t -> t

type unop = t -> t






let var id : t = Lvar id
let global_module id = Lglobal_module id
let const ct : t = Lconst ct


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
  | Lfunction {function_kind;
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
  | Lfunction {function_kind;
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
  | Lfunction {function_kind ;
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


let function_ ~arity ~function_kind ~params ~body : t =
  Lfunction { arity; function_kind; params ; body}

let let_ kind id e body :  t
  = Llet (kind,id,e,body)
let letrec bindings body : t =
  Lletrec(bindings,body)

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
      | Const_float_array _
      | Const_immstring _ -> b
    end
  | _ ->  Lifthenelse (a,b,c)


let abs_int x = if x < 0 then - x else x
let no_over_flow x  = abs_int x < 0x1fff_ffff

(** Make sure no int range overflow happens
    also we only check [int]
*)
let happens_to_be_diff
    (sw_consts :
       (int * Lambda.lambda) list) : int option =
  match sw_consts with
  | (a, Lconst (Const_pointer (a0,_)| Const_base (Const_int a0)))::
    (b, Lconst (Const_pointer (b0,_)| Const_base (Const_int b0)))::
    rest when
     no_over_flow a  &&
     no_over_flow a0 &&
     no_over_flow b &&
     no_over_flow b0 ->
    let diff = a0 - a in
    if b0 - b = diff then
      if List.for_all (fun (x, (lam : Lambda.lambda )) ->
          match lam with
          | Lconst (Const_pointer(x0,_) | Const_base(Const_int x0))
            when no_over_flow x0 && no_over_flow x ->
            x0 - x = diff
          | _ -> false
        ) rest  then
        Some diff
      else
        None
    else None
  | _ -> None


let switch lam (lam_switch : switch) : t =
  match lam with
  | Lconst ((Const_pointer (i,_) |  (Const_int i)))
    ->
    Ext_list.assoc_by_int lam_switch.sw_failaction i lam_switch.sw_consts
  | Lconst (Const_block (i,_,_)) ->
    Ext_list.assoc_by_int lam_switch.sw_failaction i lam_switch.sw_blocks
  | _ ->
    Lswitch(lam,lam_switch)

let stringswitch (lam : t) cases default : t =
  match lam with
  | Lconst (Const_string a) ->
    Ext_list.assoc_by_string default a cases
  | _ -> Lstringswitch(lam, cases, default)


let true_ : t =
  Lconst (Const_pointer ( 1, Pt_builtin_boolean))

let false_ : t =
  Lconst (Const_pointer( 0, Pt_builtin_boolean))

let unit : t =
  Lconst (Const_pointer( 0, Pt_constructor "()"))

(* let assert_false_unit : t =
  Lconst (Const_pointer( 0, Pt_constructor "impossible branch")) *)

(** [l || r ] *)
let sequor l r = if_ l true_ r

(** [l && r ] *)
let sequand l r = if_ l r false_

let seq a b : t =
  (* match a, b with
  | ( Lvar _
  | Lconst _
  | Lfunction _), _ -> b
  | _ -> *) (* TODO *)
  Lsequence (a, b)

let append_unit a  =
  Lsequence (a,unit)

let while_ a b : t  =
  Lwhile(a,b)

let try_  body id  handler : t =
  Ltrywith(body,id,handler)

let for_ v e1 e2 dir e3 : t  =
  Lfor(v,e1,e2,dir,e3)



let ifused v l : t  =
  Lifused (v,l)

let assign v l : t = Lassign(v,l)

let send u m o ll v : t =
  Lsend(u, m, o, ll, v)

let staticcatch  a b c : t = Lstaticcatch(a,b,c)

let staticraise a b : t = Lstaticraise(a,b)

let comparison (cmp : comparison) a b : bool =
  match cmp with
  | Ceq -> a = b
  | Cneq -> a <> b
  | Cgt -> a > b
  | Cle -> a <= b
  | Clt -> a < b
  | Cge -> a >= b

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

let prim ~primitive:(prim : primitive) ~args loc  : t =
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
      | Pnot , Const_pointer (a,_)
        -> Lift.bool (a = 0 )
      | _ -> default ()
    end


  | [Lconst a ; Lconst b] ->
    begin match prim, a, b  with
      | Pbintcomp(_, cmp),  (Const_int32 a),  (Const_int32 b)
        -> Lift.bool (comparison cmp a b)
      | Pbintcomp(_, cmp),  (Const_int64 a),  (Const_int64 b)
        -> Lift.bool (comparison cmp a b)
      | Pbintcomp(_, cmp),  (Const_nativeint a),  (Const_nativeint b)
        -> Lift.bool (comparison cmp a b)
      | Pfloatcomp  cmp,  (Const_nativeint a),  (Const_nativeint b)
        -> Lift.bool (comparison cmp a b)
      | Pintcomp cmp ,
        ( (Const_int a) | Const_pointer (a,_)),
        ( (Const_int b) | Const_pointer (b,_))
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
      | Psequand, Const_pointer (a, _), Const_pointer( b, _)
        ->
        Lift.bool (a = 1 && b = 1)
      | Psequor, Const_pointer (a, _), Const_pointer( b, _)
        ->
        Lift.bool (a = 1 || b = 1)
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
    -> append_unit result
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
(******************************************************************)


(** drop Lseq (List! ) etc *)
let rec drop_global_marker (lam : t) =
  match lam with
  | Lsequence (Lglobal_module id, rest) ->
    drop_global_marker rest
  | _ -> lam


let lam_prim ~primitive:( p : Lambda.primitive) ~args loc : t =
  match p with
  | Pint_as_pointer
  | Pidentity ->
    begin match args with [x] -> x | _ -> assert false end
  | Pccall _ -> assert false
  | Prevapply -> assert false
  | Pdirapply -> assert false
  | Ploc loc -> assert false (* already compiled away here*)

  | Pbytes_to_string (* handled very early *)
    -> prim ~primitive:Pbytes_to_string ~args loc
  | Pbytes_of_string -> prim ~primitive:Pbytes_of_string ~args loc
  | Pignore -> (* Pignore means return unit, it is not an nop *)
    begin match args with [x] -> seq x unit | _ -> assert false end
  | Pgetglobal id ->
    assert false
  | Psetglobal id ->
    (* we discard [Psetglobal] in the beginning*)
    begin match args with
      | [biglambda] ->
        drop_global_marker biglambda
      | _ -> assert false
    end
  (* prim ~primitive:(Psetglobal id) ~args loc *)
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


  | Praise _ ->
    prim ~primitive:Praise ~args loc
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




(******************************************************************)

type bindings = (Ident.t * t) list


let preprocess_deps (groups : bindings) : _ * Ident.t array * Int_vec.t array   =
  let len = List.length groups in
  let domain : _ Ordered_hash_map_local_ident.t =
    Ordered_hash_map_local_ident.create len in
  let mask = Hash_set_ident_mask.create len in
  List.iter (fun (x,lam) ->
      Ordered_hash_map_local_ident.add domain x lam;
      Hash_set_ident_mask.add_unmask mask x;
    ) groups ;
  let int_mapping = Ordered_hash_map_local_ident.to_sorted_array domain in
  let node_vec = Array.make (Array.length int_mapping) (Int_vec.empty ()) in
  domain
  |> Ordered_hash_map_local_ident.iter ( fun id lam key_index ->
      let base_key =  node_vec.(key_index) in
      ignore (hit_mask mask lam) ;
      mask |> Hash_set_ident_mask.iter_and_unmask (fun ident hit  ->
          if hit then
            begin
              let key = Ordered_hash_map_local_ident.rank domain ident in
              Int_vec.push key base_key;
            end
        );

    ) ;
  domain, int_mapping , node_vec


let is_function_bind (_, (x : t)) =
  match x with
  | Lfunction _ -> true
  | _ -> false

let sort_single_binding_group (group : bindings) =
  if List.for_all is_function_bind group then group
  else
    List.sort (fun (_,lama) (_,lamb) ->
        match lama,lamb with
        | Lfunction _, Lfunction _ ->  0
        | Lfunction _ , _ -> -1
        | _, Lfunction _ -> 1
        | _,_ -> 0
      ) group

(** TODO: even for a singleton recursive function, tell whehter it is recursive or not ? *)
let scc_bindings (groups : bindings) : bindings list =
  match groups with
  | [ _ ] -> [ sort_single_binding_group groups ]
  | _ ->
    let domain, int_mapping, node_vec = preprocess_deps groups in
    let clusters : Int_vec_vec.t = Ext_scc.graph node_vec in
    if Int_vec_vec.length clusters <= 1 then [ sort_single_binding_group groups]
    else
      Int_vec_vec.fold_right (fun  (v : Int_vec.t) acc ->
          let bindings =
            Int_vec.map_into_list (fun i ->
                let id = int_mapping.(i) in
                let lam  = Ordered_hash_map_local_ident.find_value domain  id in
                (id,lam)
              ) v  in
          sort_single_binding_group bindings :: acc
        )  clusters []
(* single binding, it does not make sense to do scc,
   we can eliminate {[ let rec f x = x + x  ]}, but it happens rarely in real world
*)
let scc  (groups :  bindings)  ( lam : t) ( body : t)
  =
  begin match groups with
    | [ (id,bind) ] ->
      if hit_any_variables (Ident_set.singleton id) bind
      then
        lam
      else let_ Strict id bind body
    | _ ->
      let (domain, int_mapping, node_vec)  = preprocess_deps groups in
      let clusters = Ext_scc.graph node_vec in
      if Int_vec_vec.length clusters <= 1 then lam
      else
        Int_vec_vec.fold_right (fun  (v : Int_vec.t) acc ->
            let bindings =
              Int_vec.map_into_list (fun i ->
                  let id = int_mapping.(i) in
                  let lam  = Ordered_hash_map_local_ident.find_value domain  id in
                  (id,lam)
                ) v  in
            match bindings with
            | [ id,lam ] ->
              let base_key = Ordered_hash_map_local_ident.rank domain id in
              if Int_vec_util.mem base_key node_vec.(base_key) then
                letrec bindings acc
              else  let_ Strict id lam acc
            | _ ->
              letrec bindings  acc
          )  clusters body
  end

(******************************************************************)


type required_modules = Lam_module_ident.Hash_set.t

let may_depend = Lam_module_ident.Hash_set.add
let convert exports lam : _ * _  =
  let alias_tbl = Ident_hashtbl.create 64 in
  let exit_map = Int_hashtbl.create 0 in
  let may_depends = Lam_module_ident.Hash_set.create 0 in

  let rec
    convert_ccall (a : Primitive.description)  (args : Lambda.lambda list) loc : t=
    let prim_name = a.prim_name in
    let prim_name_len  = String.length prim_name in
    match External_ffi_types.from_string a.prim_native_name with
    | Ffi_normal ->
      if prim_name_len > 0 && String.unsafe_get prim_name 0 = '#' then
        convert_js_primitive a args loc
      else
        (* COMPILER CHECK *)
        (* Here the invariant we should keep is that all exception
           created should be captured
        *)
        begin match prim_name  ,  args with
          | "caml_set_oo_id" ,
            [ Lprim (Pmakeblock(tag,( Blk_exception| Blk_extension), _),
                     Lconst (Const_base(Const_string(name,_))) :: _,
                     loc
                    )]
            -> prim ~primitive:(Pcreate_extension name) ~args:[] loc
          | _ , _->
            let args = Ext_list.map convert_aux args in
            prim ~primitive:(Pccall a) ~args loc
        end
    | Ffi_obj_create labels ->
      let args = Ext_list.map convert_aux args in
      prim ~primitive:(Pjs_object_create labels) ~args loc
    | Ffi_bs(arg_types, result_type, ffi) ->
      let args = Ext_list.map convert_aux args in
      handle_bs_non_obj_ffi arg_types result_type ffi args loc prim_name


  and convert_js_primitive (p: Primitive.description) (args : Lambda.lambda list) loc =
    let s = p.prim_name in
    match () with
    | _ when s = "#raw_expr" ->
      begin match args with
        | [Lconst( Const_base (Const_string(s,_)))] ->
          prim ~primitive:(Praw_js_code_exp s)
            ~args:[] loc
        | _ -> assert false
      end
    | _ when s = "#raw_stmt" ->
      begin match args with
        | [Lconst( Const_base (Const_string(s,_)))] ->
          prim ~primitive:(Praw_js_code_stmt s)
            ~args:[] loc
        | _ -> assert false
      end
    | _ when s =  "#debugger"  ->
      (* ATT: Currently, the arity is one due to PPX *)
      prim ~primitive:Pdebugger ~args:[] loc
    | _ when s = "#null" ->
      Lconst (Const_js_null)
    | _ when s = "#undefined" ->
      Lconst (Const_js_undefined)
    | _ when s = "#init_mod" ->
      let args = Ext_list.map convert_aux args in
      begin match args with
      | [_loc; Lconst(Const_block(0,_,[Const_block(0,_,[])]))]
        ->
        unit
      | _ -> prim ~primitive:Pinit_mod ~args loc
      end
    | _ when s = "#update_mod" ->
      let args = Ext_list.map convert_aux args in
      begin match args with
      | [Lconst(Const_block(0,_,[Const_block(0,_,[])]));_;_]
        -> unit
      | _ -> prim ~primitive:Pupdate_mod ~args loc
      end
    | _ ->
      let primitive =
        match s with
        | "#apply" -> Pjs_runtime_apply
        | "#apply1"
        | "#apply2"
        | "#apply3"
        | "#apply4"
        | "#apply5"
        | "#apply6"
        | "#apply7"
        | "#apply8" -> Pjs_apply
        | "#makemutablelist" ->
          Pmakeblock(0,Lambda.Blk_constructor("::",1),Mutable)
        | "#setfield1" ->
          Psetfield(1, true, Fld_set_na)
        | "#undefined_to_opt" -> Pundefined_to_opt
        | "#null_undefined_to_opt" -> Pnull_undefined_to_opt
        | "#null_to_opt" -> Pnull_to_opt
        | "#is_nil_undef" -> Pis_null_undefined
        | "#string_append" -> Pstringadd


        | "#string_of_small_int_array" -> Pjs_string_of_small_array
        (* {[String.fromCharCode.apply(null,x)]}
           Note if we have better suport [@bs.splice],
           we can get rid of it*)
        | "#obj_set_length" -> Pcaml_obj_set_length
        | "#obj_length" -> Pcaml_obj_length
        | "#function_length" -> Pjs_function_length

        | "#unsafe_lt" -> Pjscomp Clt
        | "#unsafe_gt" -> Pjscomp Cgt
        | "#unsafe_le" -> Pjscomp Cle
        | "#unsafe_ge" -> Pjscomp Cge
        | "#unsafe_eq" -> Pjscomp Ceq
        | "#unsafe_neq" -> Pjscomp Cneq

        | "#typeof" -> Pjs_typeof
        | "#fn_run" | "#method_run" -> Pjs_fn_run(int_of_string p.prim_native_name)
        | "#fn_mk" -> Pjs_fn_make (int_of_string p.prim_native_name)
        | "#fn_method" -> Pjs_fn_method (int_of_string p.prim_native_name)
        | "#unsafe_downgrade" -> Pjs_unsafe_downgrade (Ext_string.empty,loc)
        | _ -> Location.raise_errorf ~loc
                 "@{<error>Error:@} internal error, using unrecorgnized primitive %s" s
      in
      let args = Ext_list.map convert_aux args in
      prim ~primitive ~args loc
  and convert_constant ( const : Lambda.structured_constant) : constant =
    match const with
    | Const_base (Const_int i) -> (Const_int i)
    | Const_base (Const_char i) -> (Const_char i)
    | Const_base (Const_string(i,opt)) ->
      begin match opt with
        | Some opt when
            Ext_string.equal opt Literals.escaped_j_delimiter ->
          Const_unicode i
        | _ ->
          Const_string i
      end
    | Const_base (Const_float i) -> (Const_float i)
    | Const_base (Const_int32 i) -> (Const_int32 i)
    | Const_base (Const_int64 i) -> (Const_int64 i)
    | Const_base (Const_nativeint i) -> (Const_nativeint i)
    | Const_pointer(i,p) -> Const_pointer (i,p)
    | Const_float_array (s) -> Const_float_array(s)
    | Const_immstring s -> Const_immstring s
    | Const_block (i,t,xs) ->
      Const_block (i,t, Ext_list.map convert_constant xs)
  and convert_aux (lam : Lambda.lambda) : t =
    match lam with
    | Lvar x ->
      let var = Ident_hashtbl.find_default alias_tbl x x in
      if Ident.persistent var then
        Lglobal_module var
      else
        Lvar var
    | Lconst x ->
      Lconst (convert_constant x )
    | Lapply (fn,args,loc)
      ->
      begin match fn with
        (*
        | Lfunction(kind,params,Lprim(prim,inner_args,inner_loc))
          when List.for_all2_no_exn (fun x y ->
          match y with
          | Lambda.Lvar y when Ident.same x y -> true
          | _ -> false
           ) params inner_args
          ->
          let rec aux outer_args params =
            match outer_args, params with
            | x::xs , _::ys ->
              x :: aux xs ys
            | [], [] -> []
            | x::xs, [] ->
            | [], y::ys
          if Ext_list.same_length inner_args args then
            aux (Lprim(prim,args,inner_loc))
          else

           {[
             (fun x y -> f x y) (computation;e) -->
             (fun y -> f (computation;e) y)
           ]}
              is wrong

              or
           {[
             (fun x y -> f x y ) ([|1;2;3|]) -->
             (fun y -> f [|1;2;3|] y)
           ]}
              is also wrong.

              It seems, we need handle [@bs.splice] earlier

              or
           {[
             (fun x y -> f x y) ([|1;2;3|]) -->
             let x0, x1, x2 =1,2,3 in
             (fun y -> f [|x0;x1;x2|] y)
           ]}
              But this still need us to know [@bs.splice] in advance


           we should not remove it immediately, since we have to be careful
                  where it is used, it can be [exported], [Lvar] or [Lassign] etc
                  The other common mistake is that
           {[
             let x = y (* elimiated x/y*)
             let u = x  (* eliminated u/x *)
           ]}

            however, [x] is already eliminated
           To improve the algorithm
           {[
             let x = y (* x/y *)
             let u = x (* u/y *)
           ]}
                  This looks more correct, but lets be conservative here

                  global module inclusion {[ include List ]}
                  will cause code like {[ let include =a Lglobal_module (list)]}

                  when [u] is global, it can not be bound again,
                  it should always be the leaf
        *)
        | _ ->

          (** we need do this eargly in case [aux fn] add some wrapper *)
          apply (convert_aux fn) (Ext_list.map convert_aux args)
            loc App_na
      end
    | Lfunction (Tupled,_,_) -> assert false
    | Lfunction (Curried,  params,body)
      ->  function_
            ~arity:(List.length params) ~function_kind:Curried ~params
            ~body:(convert_aux body)
    | Llet (kind,id,e,body)
      ->

      begin match kind, e with
        | Alias , (Lvar u ) ->
          let new_u = (Ident_hashtbl.find_default alias_tbl u u) in
          Ident_hashtbl.add alias_tbl id new_u ;
          if Ident_set.mem id exports then
            Llet(kind, id, Lvar new_u, convert_aux body)
          else convert_aux body
        | Alias ,  Lprim (Pgetglobal u,[], _) when not (Ident.is_predef_exn u)
          ->
          Ident_hashtbl.add alias_tbl id u;
          may_depend may_depends (Lam_module_ident.of_ml u);

          if Ident_set.mem id exports then
            Llet(kind, id, Lvar u, convert_aux body)
          else convert_aux body

        | _, _ -> Llet(kind,id,convert_aux e, convert_aux body)
      end
    | Lletrec (bindings,body)
      ->
      let bindings = Ext_list.map (fun (id, e) -> id, convert_aux e) bindings in
      let body = convert_aux body in
      let lam = Lletrec (bindings, body) in
      scc bindings lam body
    (* inlining will affect how mututal recursive behave *)
    | Lprim(Prevapply, [x ; f ],  outer_loc)
    | Lprim(Pdirapply, [f ; x],  outer_loc) ->
      begin match f with
        (* [x|>f]
           TODO: [airty = 0] when arity =0, it can not be escaped user can only
           write  [f x ] instead of [x |> f ]
        *)
        | Lfunction(kind, [param],Lprim(external_fn,[Lvar inner_arg],inner_loc))
          when Ident.same param inner_arg
          ->
          convert_aux  (Lprim(external_fn,  [x], outer_loc))

        |  Lapply(Lfunction(kind, params,Lprim(external_fn,inner_args,inner_loc)), args, outer_loc ) (* x |> f a *)

          when Ext_list.for_all2_no_exn (fun x y -> match y with Lambda.Lvar y when Ident.same x y  -> true | _ -> false ) params inner_args
               &&
               Ext_list.length_larger_than_n 1 inner_args args
          ->

          convert_aux (Lprim(external_fn, Ext_list.append args [x], outer_loc))
        | _ ->
          let x  = convert_aux x in
          let f =  convert_aux f in
          begin match  f with
            | Lapply{fn;args} ->
              apply fn (args @[x]) outer_loc App_na
            | _ ->
              apply f [x] outer_loc App_na
          end
      end
    | Lprim (Prevapply, _, _ ) -> assert false
    | Lprim(Pdirapply, _, _) -> assert false
    | Lprim(Pccall a, args, loc)  ->
      convert_ccall a args loc
    | Lprim (Pgetglobal id, args, loc) ->
      let args = Ext_list.map convert_aux args in
      if Ident.is_predef_exn id then
        Lprim {primitive = Pglobal_exception id; args ; loc}
      else
        begin
          may_depend may_depends (Lam_module_ident.of_ml id);
          assert (args = []);
          Lglobal_module id
        end
    | Lprim (primitive,args, loc)
      ->
      let args = Ext_list.map convert_aux args in
      lam_prim ~primitive ~args loc
    | Lswitch (e,s) ->
      let  e = convert_aux e in
      begin match s with
        | {
          sw_failaction = None ;
          sw_blocks = [];
          sw_numblocks = 0;
          sw_consts ;
          sw_numconsts ;
        } ->
          begin match happens_to_be_diff sw_consts with
            | Some 0 -> e
            | Some i ->
              prim
              ~primitive:Paddint
               ~args:[e; Lconst(Const_int i)]
               Location.none
            | None ->
              Lswitch(e,
                      {sw_failaction = None;
                       sw_blocks = [];
                       sw_numblocks = 0;
                       sw_consts =
                         Ext_list.map (fun (i,lam) -> i, convert_aux lam) sw_consts;
                       sw_numconsts
                      })
          end
        | _ -> Lswitch ( e, aux_switch s)
      end
    | Lstringswitch (e, cases, default,_) ->
      Lstringswitch (convert_aux e, Ext_list.map (fun (x, b) -> x, convert_aux b ) cases,
                     match default with
                     | None -> None
                     | Some x -> Some (convert_aux x)
                    )
    | Lstaticraise (id,[]) ->
      begin match Int_hashtbl.find_opt exit_map id  with
        | None -> Lstaticraise (id,[])
        | Some new_id -> Lstaticraise (new_id,[])
      end
    | Lstaticraise (id, args) ->
      Lstaticraise (id, Ext_list.map convert_aux args)
    | Lstaticcatch (b, (i,[]), Lstaticraise (j,[]) )
      -> (* peep-hole [i] aliased to [j] *)

      let new_i = Int_hashtbl.find_default exit_map j j in
      Int_hashtbl.add exit_map i new_i ;
      convert_aux b
    | Lstaticcatch (b, (i, ids), handler) ->
      Lstaticcatch (convert_aux b, (i,ids), convert_aux handler)
    | Ltrywith (b, id, handler) ->
      let body = convert_aux b in
      let handler = convert_aux handler in
      if exception_id_escaped id handler then
        let newId = Ident.create ("raw_" ^ id.name) in
        Ltrywith (body, newId,
                  let_ StrictOpt id
                    (prim ~primitive:Pwrap_exn ~args:[var newId] Location.none)
                    handler
                 )
      else
        Ltrywith( body, id, handler)
    | Lifthenelse (b,then_,else_) ->
      Lifthenelse (convert_aux b, convert_aux then_, convert_aux else_)
    | Lsequence (a,b)
      -> Lsequence (convert_aux a, convert_aux b)
    | Lwhile (b,body) ->
      Lwhile (convert_aux b, convert_aux body)
    | Lfor (id, from_, to_, dir, loop) ->
      Lfor (id, convert_aux from_, convert_aux to_, dir, convert_aux loop)
    | Lassign (id, body) ->
      Lassign (id, convert_aux body)
    | Lsend (kind, a,b,ls, loc) ->
      (* Format.fprintf Format.err_formatter "%a@." Printlambda.lambda b ; *)
      begin match convert_aux b with
        | Lprim {primitive =  Pjs_unsafe_downgrade(_,loc);  args}
          ->
          begin match kind, ls with
            | Public (Some name), [] ->
              prim ~primitive:(Pjs_unsafe_downgrade (name,loc))
                ~args loc
            | _ -> assert false
          end
        | b ->
          Lsend(kind, convert_aux a,  b, Ext_list.map convert_aux ls, loc )
      end
    | Levent (e, event) ->
      (* disabled by upstream*)
      assert false
    | Lifused (id, e) ->
      Lifused(id, convert_aux e) (* TODO: remove it ASAP *)
  and aux_switch (s : Lambda.lambda_switch) : switch =
    { sw_numconsts = s.sw_numconsts ;
      sw_consts = Ext_list.map (fun (i, lam) -> i, convert_aux lam) s.sw_consts;
      sw_numblocks = s.sw_numblocks;
      sw_blocks = Ext_list.map (fun (i,lam) -> i, convert_aux lam ) s.sw_blocks;
      sw_failaction =
        match s.sw_failaction with
        | None -> None
        | Some a -> Some (convert_aux a)
    }  in
  convert_aux lam , may_depends


