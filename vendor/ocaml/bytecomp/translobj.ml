(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Misc
open Asttypes
open Longident
open Lambda

(* Get oo primitives identifiers *)

let oo_prim name =
  try
    transl_normal_path
      (fst (Env.lookup_value (Ldot (Lident "CamlinternalOO", name)) Env.empty))
  with Not_found ->
    fatal_error ("Primitive " ^ name ^ " not found.")

(* Share blocks *)

let consts : (structured_constant, Ident.t) Hashtbl.t = Hashtbl.create 17

let share c =
  match c with
    Const_block (_n,_, l) when l <> [] ->
      begin try
        Lvar (Hashtbl.find consts c)
      with Not_found ->
        let id = Ident.create "shared" in
        Hashtbl.add consts c id;
        Lvar id
      end
  | _ -> Lconst c

(* Collect labels *)

let cache_required = ref false
let method_cache = ref lambda_unit
let method_count = ref 0
let method_table = ref []

let meth_tag s = Lconst(Const_base(Const_int(Btype.hash_variant s)))

let next_cache tag =
  let n = !method_count in
  incr method_count;
  (tag, [!method_cache; Lconst(Const_base(Const_int n))])

let rec is_path = function
    Lvar _ | Lprim (Pgetglobal _, [], _) | Lconst _ -> true
  | Lprim (Pfield _, [lam], _) -> is_path lam
  | Lprim ((Parrayrefu _ | Parrayrefs _), [lam1; lam2], _) ->
      is_path lam1 && is_path lam2
  | _ -> false

let meth obj lab =
  let tag = meth_tag lab in
  if not (!cache_required && !Clflags.native_code) then (tag, []) else
  if not (is_path obj) then next_cache tag else
  try
    let r = List.assoc obj !method_table in
    try
      (tag, List.assoc tag !r)
    with Not_found ->
      let p = next_cache tag in
      r := p :: !r;
      p
  with Not_found ->
    let p = next_cache tag in
    method_table := (obj, ref [p]) :: !method_table;
    p

let reset_labels () =
  Hashtbl.clear consts;
  method_count := 0;
  method_table := []

(* Insert labels *)

let int n = Lconst (Const_base (Const_int n))

let prim_makearray =
  Primitive.simple ~name:"caml_make_vect" ~arity:2 ~alloc:true

(* Also use it for required globals *)
let transl_label_init_general f =
  let expr, size = f () in
  let expr =
    Hashtbl.fold
      (fun c id expr -> Llet(Alias, Pgenval, id, Lconst c, expr))
      consts expr
  in
  (*let expr =
    List.fold_right
      (fun id expr -> Lsequence(Lprim(Pgetglobal id, [], Location.none), expr))
      (Env.get_required_globals ()) expr
  in
  Env.reset_required_globals ();*)
  reset_labels ();
  expr, size

let transl_label_init_flambda f =
  assert(Config.flambda);
  let method_cache_id = Ident.create "method_cache" in
  method_cache := Lvar method_cache_id;
  (* Calling f (usually Translmod.transl_struct) requires the
     method_cache variable to be initialised to be able to generate
     method accesses. *)
  let expr, size = f () in
  let expr =
    if !method_count = 0 then expr
    else
      Llet (Strict, Pgenval, method_cache_id,
        Lprim (Pccall prim_makearray,
               [int !method_count; int 0],
               Location.none),
        expr)
  in
  transl_label_init_general (fun () -> expr, size)

let transl_store_label_init glob size f arg =
  assert(not Config.flambda);
  assert(!Clflags.native_code);
  method_cache := Lprim(Pfield (size, Fld_na),
                        [Lprim(Pgetglobal glob, [], Location.none)],
                        Location.none);
  let expr = f arg in
  let (size, expr) =
    if !method_count = 0 then (size, expr) else
    (size+1,
     Lsequence(
     Lprim(Psetfield(size, Pointer, Root_initialization, Fld_set_na),
           [Lprim(Pgetglobal glob, [], Location.none);
            Lprim (Pccall prim_makearray,
                   [int !method_count; int 0],
                   Location.none)],
           Location.none),
     expr))
  in
  let lam, size = transl_label_init_general (fun () -> (expr, size)) in
  size, lam

let transl_label_init f =
  if !Clflags.native_code then
    transl_label_init_flambda f
  else
    transl_label_init_general f

(* Share classes *)

let wrapping = ref false
let top_env = ref Env.empty
let classes = ref []
let method_ids = ref IdentSet.empty

let oo_add_class id =
  classes := id :: !classes;
  (!top_env, !cache_required)

let oo_wrap env req f x =
  if !wrapping then
    if !cache_required then f x else
    try cache_required := true; let lam = f x in cache_required := false; lam
    with exn -> cache_required := false; raise exn
  else try
    wrapping := true;
    cache_required := req;
    top_env := env;
    classes := [];
    method_ids := IdentSet.empty;
    let lambda = f x in
    let lambda =
      List.fold_left
        (fun lambda id ->
          Llet(StrictOpt, Pgenval, id,
               Lprim(Pmakeblock(0, Lambda.default_tag_info, Mutable, None),
                     [lambda_unit; lambda_unit; lambda_unit],
                     Location.none),
               lambda))
        lambda !classes
    in
    wrapping := false;
    top_env := Env.empty;
    lambda
  with exn ->
    wrapping := false;
    top_env := Env.empty;
    raise exn

let reset () =
  Hashtbl.clear consts;
  cache_required := false;
  method_cache := lambda_unit;
  method_count := 0;
  method_table := [];
  wrapping := false;
  top_env := Env.empty;
  classes := [];
  method_ids := IdentSet.empty
