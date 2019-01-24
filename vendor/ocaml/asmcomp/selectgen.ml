(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Selection of pseudo-instructions, assignment of pseudo-registers,
   sequentialization. *)

open Misc
open Cmm
open Reg
open Mach

type environment =
  { vars : (Ident.t, Reg.t array) Tbl.t;
    static_exceptions : (int, Reg.t array list) Tbl.t;
    (** Which registers must be populated when jumping to the given
        handler. *)
  }

let env_add id v env =
  { env with vars = Tbl.add id v env.vars }

let env_add_static_exception id v env =
  { env with static_exceptions = Tbl.add id v env.static_exceptions }

let env_find id env =
  Tbl.find id env.vars

let env_find_static_exception id env =
  Tbl.find id env.static_exceptions

let env_empty = {
  vars = Tbl.empty;
  static_exceptions = Tbl.empty;
}

(* Infer the type of the result of an operation *)

let oper_result_type = function
    Capply ty -> ty
  | Cextcall(_s, ty, _alloc, _) -> ty
  | Cload (c, _) ->
      begin match c with
      | Word_val -> typ_val
      | Single | Double | Double_u -> typ_float
      | _ -> typ_int
      end
  | Calloc -> typ_val
  | Cstore (_c, _) -> typ_void
  | Caddi | Csubi | Cmuli | Cmulhi | Cdivi | Cmodi |
    Cand | Cor | Cxor | Clsl | Clsr | Casr |
    Ccmpi _ | Ccmpa _ | Ccmpf _ -> typ_int
  | Caddv -> typ_val
  | Cadda -> typ_addr
  | Cnegf | Cabsf | Caddf | Csubf | Cmulf | Cdivf -> typ_float
  | Cfloatofint -> typ_float
  | Cintoffloat -> typ_int
  | Craise _ -> typ_void
  | Ccheckbound -> typ_void

(* Infer the size in bytes of the result of an expression whose evaluation
   may be deferred (cf. [emit_parts]). *)

let size_expr (env:environment) exp =
  let rec size localenv = function
      Cconst_int _ | Cconst_natint _ -> Arch.size_int
    | Cconst_symbol _ | Cconst_pointer _ | Cconst_natpointer _ ->
        Arch.size_addr
    | Cconst_float _ -> Arch.size_float
    | Cblockheader _ -> Arch.size_int
    | Cvar id ->
        begin try
          Tbl.find id localenv
        with Not_found ->
        try
          let regs = env_find id env in
          size_machtype (Array.map (fun r -> r.typ) regs)
        with Not_found ->
          fatal_error("Selection.size_expr: unbound var " ^
                      Ident.unique_name id)
        end
    | Ctuple el ->
        List.fold_right (fun e sz -> size localenv e + sz) el 0
    | Cop(op, _, _) ->
        size_machtype(oper_result_type op)
    | Clet(id, arg, body) ->
        size (Tbl.add id (size localenv arg) localenv) body
    | Csequence(_e1, e2) ->
        size localenv e2
    | _ ->
        fatal_error "Selection.size_expr"
  in size Tbl.empty exp

(* Swap the two arguments of an integer comparison *)

let swap_intcomp = function
    Isigned cmp -> Isigned(swap_comparison cmp)
  | Iunsigned cmp -> Iunsigned(swap_comparison cmp)

(* Naming of registers *)

let all_regs_anonymous rv =
  try
    for i = 0 to Array.length rv - 1 do
      if not (Reg.anonymous rv.(i)) then raise Exit
    done;
    true
  with Exit ->
    false

let name_regs id rv =
  if Array.length rv = 1 then
    rv.(0).raw_name <- Raw_name.create_from_ident id
  else
    for i = 0 to Array.length rv - 1 do
      rv.(i).raw_name <- Raw_name.create_from_ident id;
      rv.(i).part <- Some i
    done

(* "Join" two instruction sequences, making sure they return their results
   in the same registers. *)

let join opt_r1 seq1 opt_r2 seq2 =
  match (opt_r1, opt_r2) with
    (None, _) -> opt_r2
  | (_, None) -> opt_r1
  | (Some r1, Some r2) ->
      let l1 = Array.length r1 in
      assert (l1 = Array.length r2);
      let r = Array.make l1 Reg.dummy in
      for i = 0 to l1-1 do
        if Reg.anonymous r1.(i)
          && Cmm.ge_component r1.(i).typ r2.(i).typ
        then begin
          r.(i) <- r1.(i);
          seq2#insert_move r2.(i) r1.(i)
        end else if Reg.anonymous r2.(i)
          && Cmm.ge_component r2.(i).typ r1.(i).typ
        then begin
          r.(i) <- r2.(i);
          seq1#insert_move r1.(i) r2.(i)
        end else begin
          let typ = Cmm.lub_component r1.(i).typ r2.(i).typ in
          r.(i) <- Reg.create typ;
          seq1#insert_move r1.(i) r.(i);
          seq2#insert_move r2.(i) r.(i)
        end
      done;
      Some r

(* Same, for N branches *)

let join_array rs =
  let some_res = ref None in
  for i = 0 to Array.length rs - 1 do
    let (r, _) = rs.(i) in
    match r with
    | None -> ()
    | Some r ->
      match !some_res with
      | None -> some_res := Some (r, Array.map (fun r -> r.typ) r)
      | Some (r', types) ->
        let types =
          Array.map2 (fun r typ -> Cmm.lub_component r.typ typ) r types
        in
        some_res := Some (r', types)
  done;
  match !some_res with
    None -> None
  | Some (template, types) ->
      let size_res = Array.length template in
      let res = Array.make size_res Reg.dummy in
      for i = 0 to size_res - 1 do
        res.(i) <- Reg.create types.(i)
      done;
      for i = 0 to Array.length rs - 1 do
        let (r, s) = rs.(i) in
        match r with
          None -> ()
        | Some r -> s#insert_moves r res
      done;
      Some res

(* Name of function being compiled *)
let current_function_name = ref ""

module Effect = struct
  type t =
    | None
    | Raise
    | Arbitrary

  let join t1 t2 =
    match t1, t2 with
    | None, t2 -> t2
    | t1, None -> t1
    | Raise, Raise -> Raise
    | Arbitrary, _ | _, Arbitrary -> Arbitrary

  let pure = function
    | None -> true
    | Raise | Arbitrary -> false
end

module Coeffect = struct
  type t =
    | None
    | Read_mutable
    | Arbitrary

  let join t1 t2 =
    match t1, t2 with
    | None, t2 -> t2
    | t1, None -> t1
    | Read_mutable, Read_mutable -> Read_mutable
    | Arbitrary, _ | _, Arbitrary -> Arbitrary

  let copure = function
    | None -> true
    | Read_mutable | Arbitrary -> false
end

module Effect_and_coeffect : sig
  type t

  val none : t
  val arbitrary : t

  val effect : t -> Effect.t
  val coeffect : t -> Coeffect.t

  val pure_and_copure : t -> bool

  val effect_only : Effect.t -> t
  val coeffect_only : Coeffect.t -> t

  val join : t -> t -> t
  val join_list_map : 'a list -> ('a -> t) -> t
end = struct
  type t = Effect.t * Coeffect.t

  let none = Effect.None, Coeffect.None
  let arbitrary = Effect.Arbitrary, Coeffect.Arbitrary

  let effect (e, _ce) = e
  let coeffect (_e, ce) = ce

  let pure_and_copure (e, ce) = Effect.pure e && Coeffect.copure ce

  let effect_only e = e, Coeffect.None
  let coeffect_only ce = Effect.None, ce

  let join (e1, ce1) (e2, ce2) =
    Effect.join e1 e2, Coeffect.join ce1 ce2

  let join_list_map xs f =
    match xs with
    | [] -> none
    | x::xs -> List.fold_left (fun acc x -> join acc (f x)) (f x) xs
end

(* The default instruction selection class *)

class virtual selector_generic = object (self)

(* A syntactic criterion used in addition to judgements about (co)effects as
   to whether the evaluation of a given expression may be deferred by
   [emit_parts].  This criterion is a property of the instruction selection
   algorithm in this file rather than a property of the Cmm language.
*)
method is_simple_expr = function
    Cconst_int _ -> true
  | Cconst_natint _ -> true
  | Cconst_float _ -> true
  | Cconst_symbol _ -> true
  | Cconst_pointer _ -> true
  | Cconst_natpointer _ -> true
  | Cblockheader _ -> true
  | Cvar _ -> true
  | Ctuple el -> List.for_all self#is_simple_expr el
  | Clet(_id, arg, body) -> self#is_simple_expr arg && self#is_simple_expr body
  | Csequence(e1, e2) -> self#is_simple_expr e1 && self#is_simple_expr e2
  | Cop(op, args, _) ->
      begin match op with
        (* The following may have side effects *)
      | Capply _ | Cextcall _ | Calloc | Cstore _ | Craise _ -> false
        (* The remaining operations are simple if their args are *)
      | Cload _ | Caddi | Csubi | Cmuli | Cmulhi | Cdivi | Cmodi | Cand | Cor
      | Cxor | Clsl | Clsr | Casr | Ccmpi _ | Caddv | Cadda | Ccmpa _ | Cnegf
      | Cabsf | Caddf | Csubf | Cmulf | Cdivf | Cfloatofint | Cintoffloat
      | Ccmpf _ | Ccheckbound -> List.for_all self#is_simple_expr args
      end
  | Cassign _ | Cifthenelse _ | Cswitch _ | Cloop _ | Ccatch _ | Cexit _
  | Ctrywith _ -> false

(* Analyses the effects and coeffects of an expression.  This is used across
   a whole list of expressions with a view to determining which expressions
   may have their evaluation deferred.  The result of this function, modulo
   target-specific judgements if the [effects_of] method is overridden, is a
   property of the Cmm language rather than anything particular about the
   instruction selection algorithm in this file.

   In the case of e.g. an OCaml function call, the arguments whose evaluation
   cannot be deferred (cf. [emit_parts], below) are computed in right-to-left
   order first with their results going into temporaries, then the block is
   allocated, then the remaining arguments are evaluated before being
   combined with the temporaries. *)
method effects_of exp =
  let module EC = Effect_and_coeffect in
  match exp with
  | Cconst_int _ | Cconst_natint _ | Cconst_float _ | Cconst_symbol _
  | Cconst_pointer _ | Cconst_natpointer _ | Cblockheader _
  | Cvar _ -> EC.none
  | Ctuple el -> EC.join_list_map el self#effects_of
  | Clet (_id, arg, body) ->
    EC.join (self#effects_of arg) (self#effects_of body)
  | Csequence (e1, e2) ->
    EC.join (self#effects_of e1) (self#effects_of e2)
  | Cifthenelse (cond, ifso, ifnot) ->
    EC.join (self#effects_of cond)
      (EC.join (self#effects_of ifso) (self#effects_of ifnot))
  | Cop (op, args, _) ->
    let from_op =
      match op with
      | Capply _ | Cextcall _ -> EC.arbitrary
      | Calloc -> EC.none
      | Cstore _ -> EC.effect_only Effect.Arbitrary
      | Craise _ | Ccheckbound -> EC.effect_only Effect.Raise
      | Cload (_, Asttypes.Immutable) -> EC.none
      | Cload (_, Asttypes.Mutable) -> EC.coeffect_only Coeffect.Read_mutable
      | Caddi | Csubi | Cmuli | Cmulhi | Cdivi | Cmodi | Cand | Cor | Cxor
      | Clsl | Clsr | Casr | Ccmpi _ | Caddv | Cadda | Ccmpa _ | Cnegf | Cabsf
      | Caddf | Csubf | Cmulf | Cdivf | Cfloatofint | Cintoffloat | Ccmpf _ ->
        EC.none
    in
    EC.join from_op (EC.join_list_map args self#effects_of)
  | Cassign _ | Cswitch _ | Cloop _ | Ccatch _ | Cexit _ | Ctrywith _ ->
    EC.arbitrary

(* Says whether an integer constant is a suitable immediate argument *)

method virtual is_immediate : int -> bool

(* Selection of addressing modes *)

method virtual select_addressing :
  Cmm.memory_chunk -> Cmm.expression -> Arch.addressing_mode * Cmm.expression

(* Default instruction selection for stores (of words) *)

method select_store is_assign addr arg =
  (Istore(Word_val, addr, is_assign), arg)

(* call marking methods, documented in selectgen.mli *)

method mark_call =
  Proc.contains_calls := true

method mark_tailcall = ()

method mark_c_tailcall = ()

method mark_instr = function
  | Iop (Icall_ind _ | Icall_imm _ | Iextcall _) ->
      self#mark_call
  | Iop (Itailcall_ind _ | Itailcall_imm _) ->
      self#mark_tailcall
  | Iop (Ialloc _) ->
      self#mark_call (* caml_alloc*, caml_garbage_collection *)
  | Iop (Iintop (Icheckbound _) | Iintop_imm(Icheckbound _, _)) ->
      self#mark_c_tailcall (* caml_ml_array_bound_error *)
  | Iraise raise_kind ->
    begin match raise_kind with
      | Cmm.Raise_notrace -> ()
      | Cmm.Raise_withtrace ->
          (* PR#6239 *)
          (* caml_stash_backtrace; we #mark_call rather than
             #mark_c_tailcall to get a good stack backtrace *)
          self#mark_call
    end
  | Itrywith _ ->
    self#mark_call
  | _ -> ()

(* Default instruction selection for operators *)

method select_allocation words =
  Ialloc { words; spacetime_index = 0; label_after_call_gc = None; }
method select_allocation_args _env = [| |]

method select_checkbound () =
  Icheckbound { spacetime_index = 0; label_after_error = None; }
method select_checkbound_extra_args () = []

method select_operation op args _dbg =
  match (op, args) with
  | (Capply _, Cconst_symbol func :: rem) ->
    let label_after = Cmm.new_label () in
    (Icall_imm { func; label_after; }, rem)
  | (Capply _, _) ->
    let label_after = Cmm.new_label () in
    (Icall_ind { label_after; }, args)
  | (Cextcall(func, _ty, alloc, label_after), _) ->
    let label_after =
      match label_after with
      | None -> Cmm.new_label ()
      | Some label_after -> label_after
    in
    Iextcall { func; alloc; label_after; }, args
  | (Cload (chunk, _mut), [arg]) ->
      let (addr, eloc) = self#select_addressing chunk arg in
      (Iload(chunk, addr), [eloc])
  | (Cstore (chunk, init), [arg1; arg2]) ->
      let (addr, eloc) = self#select_addressing chunk arg1 in
      let is_assign =
        match init with
        | Lambda.Root_initialization -> false
        | Lambda.Heap_initialization -> false
        | Lambda.Assignment -> true
      in
      if chunk = Word_int || chunk = Word_val then begin
        let (op, newarg2) = self#select_store is_assign addr arg2 in
        (op, [newarg2; eloc])
      end else begin
        (Istore(chunk, addr, is_assign), [arg2; eloc])
        (* Inversion addr/datum in Istore *)
      end
  | (Calloc, _) -> (self#select_allocation 0), args
  | (Caddi, _) -> self#select_arith_comm Iadd args
  | (Csubi, _) -> self#select_arith Isub args
  | (Cmuli, _) -> self#select_arith_comm Imul args
  | (Cmulhi, _) -> self#select_arith_comm Imulh args
  | (Cdivi, _) -> (Iintop Idiv, args)
  | (Cmodi, _) -> (Iintop Imod, args)
  | (Cand, _) -> self#select_arith_comm Iand args
  | (Cor, _) -> self#select_arith_comm Ior args
  | (Cxor, _) -> self#select_arith_comm Ixor args
  | (Clsl, _) -> self#select_shift Ilsl args
  | (Clsr, _) -> self#select_shift Ilsr args
  | (Casr, _) -> self#select_shift Iasr args
  | (Ccmpi comp, _) -> self#select_arith_comp (Isigned comp) args
  | (Caddv, _) -> self#select_arith_comm Iadd args
  | (Cadda, _) -> self#select_arith_comm Iadd args
  | (Ccmpa comp, _) -> self#select_arith_comp (Iunsigned comp) args
  | (Cnegf, _) -> (Inegf, args)
  | (Cabsf, _) -> (Iabsf, args)
  | (Caddf, _) -> (Iaddf, args)
  | (Csubf, _) -> (Isubf, args)
  | (Cmulf, _) -> (Imulf, args)
  | (Cdivf, _) -> (Idivf, args)
  | (Cfloatofint, _) -> (Ifloatofint, args)
  | (Cintoffloat, _) -> (Iintoffloat, args)
  | (Ccheckbound, _) ->
    let extra_args = self#select_checkbound_extra_args () in
    let op = self#select_checkbound () in
    self#select_arith op (args @ extra_args)
  | _ -> fatal_error "Selection.select_oper"

method private select_arith_comm op = function
    [arg; Cconst_int n] when self#is_immediate n ->
      (Iintop_imm(op, n), [arg])
  | [arg; Cconst_pointer n] when self#is_immediate n ->
      (Iintop_imm(op, n), [arg])
  | [Cconst_int n; arg] when self#is_immediate n ->
      (Iintop_imm(op, n), [arg])
  | [Cconst_pointer n; arg] when self#is_immediate n ->
      (Iintop_imm(op, n), [arg])
  | args ->
      (Iintop op, args)

method private select_arith op = function
    [arg; Cconst_int n] when self#is_immediate n ->
      (Iintop_imm(op, n), [arg])
  | [arg; Cconst_pointer n] when self#is_immediate n ->
      (Iintop_imm(op, n), [arg])
  | args ->
      (Iintop op, args)

method private select_shift op = function
    [arg; Cconst_int n] when n >= 0 && n < Arch.size_int * 8 ->
      (Iintop_imm(op, n), [arg])
  | args ->
      (Iintop op, args)

method private select_arith_comp cmp = function
    [arg; Cconst_int n] when self#is_immediate n ->
      (Iintop_imm(Icomp cmp, n), [arg])
  | [arg; Cconst_pointer n] when self#is_immediate n ->
      (Iintop_imm(Icomp cmp, n), [arg])
  | [Cconst_int n; arg] when self#is_immediate n ->
      (Iintop_imm(Icomp(swap_intcomp cmp), n), [arg])
  | [Cconst_pointer n; arg] when self#is_immediate n ->
      (Iintop_imm(Icomp(swap_intcomp cmp), n), [arg])
  | args ->
      (Iintop(Icomp cmp), args)

(* Instruction selection for conditionals *)

method select_condition = function
    Cop(Ccmpi cmp, [arg1; Cconst_int n], _) when self#is_immediate n ->
      (Iinttest_imm(Isigned cmp, n), arg1)
  | Cop(Ccmpi cmp, [Cconst_int n; arg2], _) when self#is_immediate n ->
      (Iinttest_imm(Isigned(swap_comparison cmp), n), arg2)
  | Cop(Ccmpi cmp, [arg1; Cconst_pointer n], _) when self#is_immediate n ->
      (Iinttest_imm(Isigned cmp, n), arg1)
  | Cop(Ccmpi cmp, [Cconst_pointer n; arg2], _) when self#is_immediate n ->
      (Iinttest_imm(Isigned(swap_comparison cmp), n), arg2)
  | Cop(Ccmpi cmp, args, _) ->
      (Iinttest(Isigned cmp), Ctuple args)
  | Cop(Ccmpa cmp, [arg1; Cconst_pointer n], _) when self#is_immediate n ->
      (Iinttest_imm(Iunsigned cmp, n), arg1)
  | Cop(Ccmpa cmp, [arg1; Cconst_int n], _) when self#is_immediate n ->
      (Iinttest_imm(Iunsigned cmp, n), arg1)
  | Cop(Ccmpa cmp, [Cconst_pointer n; arg2], _) when self#is_immediate n ->
      (Iinttest_imm(Iunsigned(swap_comparison cmp), n), arg2)
  | Cop(Ccmpa cmp, [Cconst_int n; arg2], _) when self#is_immediate n ->
      (Iinttest_imm(Iunsigned(swap_comparison cmp), n), arg2)
  | Cop(Ccmpa cmp, args, _) ->
      (Iinttest(Iunsigned cmp), Ctuple args)
  | Cop(Ccmpf cmp, args, _) ->
      (Ifloattest(cmp, false), Ctuple args)
  | Cop(Cand, [arg; Cconst_int 1], _) ->
      (Ioddtest, arg)
  | arg ->
      (Itruetest, arg)

(* Return an array of fresh registers of the given type.
   Normally implemented as Reg.createv, but some
   ports (e.g. Arm) can override this definition to store float values
   in pairs of integer registers. *)

method regs_for tys = Reg.createv tys

(* Buffering of instruction sequences *)

val mutable instr_seq = dummy_instr

method insert_debug desc dbg arg res =
  instr_seq <- instr_cons_debug desc arg res dbg instr_seq

method insert desc arg res =
  instr_seq <- instr_cons desc arg res instr_seq

method extract_core ~end_instr =
  let rec extract res i =
    if i == dummy_instr
    then res
    else extract {i with next = res} i.next in
  extract end_instr instr_seq

method extract =
  self#extract_core ~end_instr:(end_instr ())

(* Insert a sequence of moves from one pseudoreg set to another. *)

method insert_move src dst =
  if src.stamp <> dst.stamp then
    self#insert (Iop Imove) [|src|] [|dst|]

method insert_moves src dst =
  for i = 0 to min (Array.length src) (Array.length dst) - 1 do
    self#insert_move src.(i) dst.(i)
  done

(* Adjust the types of destination pseudoregs for a [Cassign] assignment.
   The type inferred at [let] binding might be [Int] while we assign
   something of type [Val] (PR#6501). *)

method adjust_type src dst =
  let ts = src.typ and td = dst.typ in
  if ts <> td then
    match ts, td with
    | Val, Int -> dst.typ <- Val
    | Int, Val -> ()
    | _, _ -> fatal_error("Selection.adjust_type: bad assignment to "
                                                           ^ Reg.name dst)

method adjust_types src dst =
  for i = 0 to min (Array.length src) (Array.length dst) - 1 do
    self#adjust_type src.(i) dst.(i)
  done

(* Insert moves and stack offsets for function arguments and results *)

method insert_move_args arg loc stacksize =
  if stacksize <> 0 then self#insert (Iop(Istackoffset stacksize)) [||] [||];
  self#insert_moves arg loc

method insert_move_results loc res stacksize =
  if stacksize <> 0 then self#insert(Iop(Istackoffset(-stacksize))) [||] [||];
  self#insert_moves loc res

(* Add an Iop opcode. Can be overridden by processor description
   to insert moves before and after the operation, i.e. for two-address
   instructions, or instructions using dedicated registers. *)

method insert_op_debug op dbg rs rd =
  self#insert_debug (Iop op) dbg rs rd;
  rd

method insert_op op rs rd =
  self#insert_op_debug op Debuginfo.none rs rd

method emit_blockheader _env n _dbg =
  let r = self#regs_for typ_int in
  Some(self#insert_op (Iconst_int n) [||] r)

method about_to_emit_call _env _insn _arg = None

(* Prior to a function call, update the Spacetime node hole pointer hard
   register. *)

method private maybe_emit_spacetime_move ~spacetime_reg =
  Misc.Stdlib.Option.iter (fun reg ->
      self#insert_moves reg [| Proc.loc_spacetime_node_hole |])
    spacetime_reg

(* Add the instructions for the given expression
   at the end of the self sequence *)

method emit_expr (env:environment) exp =
  match exp with
    Cconst_int n ->
      let r = self#regs_for typ_int in
      Some(self#insert_op (Iconst_int(Nativeint.of_int n)) [||] r)
  | Cconst_natint n ->
      let r = self#regs_for typ_int in
      Some(self#insert_op (Iconst_int n) [||] r)
  | Cconst_float n ->
      let r = self#regs_for typ_float in
      Some(self#insert_op (Iconst_float (Int64.bits_of_float n)) [||] r)
  | Cconst_symbol n ->
      let r = self#regs_for typ_val in
      Some(self#insert_op (Iconst_symbol n) [||] r)
  | Cconst_pointer n ->
      let r = self#regs_for typ_val in  (* integer as Caml value *)
      Some(self#insert_op (Iconst_int(Nativeint.of_int n)) [||] r)
  | Cconst_natpointer n ->
      let r = self#regs_for typ_val in  (* integer as Caml value *)
      Some(self#insert_op (Iconst_int n) [||] r)
  | Cblockheader(n, dbg) ->
      self#emit_blockheader env n dbg
  | Cvar v ->
      begin try
        Some(env_find v env)
      with Not_found ->
        fatal_error("Selection.emit_expr: unbound var " ^ Ident.unique_name v)
      end
  | Clet(v, e1, e2) ->
      begin match self#emit_expr env e1 with
        None -> None
      | Some r1 -> self#emit_expr (self#bind_let env v r1) e2
      end
  | Cassign(v, e1) ->
      let rv =
        try
          env_find v env
        with Not_found ->
          fatal_error ("Selection.emit_expr: unbound var " ^ Ident.name v) in
      begin match self#emit_expr env e1 with
        None -> None
      | Some r1 -> self#adjust_types r1 rv; self#insert_moves r1 rv; Some [||]
      end
  | Ctuple [] ->
      Some [||]
  | Ctuple exp_list ->
      begin match self#emit_parts_list env exp_list with
        None -> None
      | Some(simple_list, ext_env) ->
          Some(self#emit_tuple ext_env simple_list)
      end
  | Cop(Craise k, [arg], dbg) ->
      begin match self#emit_expr env arg with
        None -> None
      | Some r1 ->
          let rd = [|Proc.loc_exn_bucket|] in
          self#insert (Iop Imove) r1 rd;
          self#insert_debug (Iraise k) dbg rd [||];
          None
      end
  | Cop(Ccmpf _, _, _) ->
      self#emit_expr env (Cifthenelse(exp, Cconst_int 1, Cconst_int 0))
  | Cop(op, args, dbg) ->
      begin match self#emit_parts_list env args with
        None -> None
      | Some(simple_args, env) ->
          let ty = oper_result_type op in
          let (new_op, new_args) = self#select_operation op simple_args dbg in
          match new_op with
            Icall_ind _ ->
              let r1 = self#emit_tuple env new_args in
              let rarg = Array.sub r1 1 (Array.length r1 - 1) in
              let rd = self#regs_for ty in
              let (loc_arg, stack_ofs) = Proc.loc_arguments rarg in
              let loc_res = Proc.loc_results rd in
              let spacetime_reg =
                self#about_to_emit_call env (Iop new_op) [| r1.(0) |]
              in
              self#insert_move_args rarg loc_arg stack_ofs;
              self#maybe_emit_spacetime_move ~spacetime_reg;
              self#insert_debug (Iop new_op) dbg
                          (Array.append [|r1.(0)|] loc_arg) loc_res;
              self#insert_move_results loc_res rd stack_ofs;
              Some rd
          | Icall_imm _ ->
              let r1 = self#emit_tuple env new_args in
              let rd = self#regs_for ty in
              let (loc_arg, stack_ofs) = Proc.loc_arguments r1 in
              let loc_res = Proc.loc_results rd in
              let spacetime_reg =
                self#about_to_emit_call env (Iop new_op) [| |]
              in
              self#insert_move_args r1 loc_arg stack_ofs;
              self#maybe_emit_spacetime_move ~spacetime_reg;
              self#insert_debug (Iop new_op) dbg loc_arg loc_res;
              self#insert_move_results loc_res rd stack_ofs;
              Some rd
          | Iextcall _ ->
              let spacetime_reg =
                self#about_to_emit_call env (Iop new_op) [| |]
              in
              let (loc_arg, stack_ofs) = self#emit_extcall_args env new_args in
              self#maybe_emit_spacetime_move ~spacetime_reg;
              let rd = self#regs_for ty in
              let loc_res =
                self#insert_op_debug new_op dbg
                  loc_arg (Proc.loc_external_results rd) in
              self#insert_move_results loc_res rd stack_ofs;
              Some rd
          | Ialloc { words; spacetime_index; label_after_call_gc; } ->
              assert (words <= Config.max_young_wosize);
              let rd = self#regs_for typ_val in
              let size = size_expr env (Ctuple new_args) in
              let op =
                Ialloc { words = size; spacetime_index; label_after_call_gc; }
              in
              let args = self#select_allocation_args env in
              self#insert_debug (Iop op) dbg args rd;
              self#emit_stores env new_args rd;
              Some rd
          | op ->
              let r1 = self#emit_tuple env new_args in
              let rd = self#regs_for ty in
              Some (self#insert_op_debug op dbg r1 rd)
      end
  | Csequence(e1, e2) ->
      begin match self#emit_expr env e1 with
        None -> None
      | Some _ -> self#emit_expr env e2
      end
  | Cifthenelse(econd, eif, eelse) ->
      let (cond, earg) = self#select_condition econd in
      begin match self#emit_expr env earg with
        None -> None
      | Some rarg ->
          let (rif, sif) = self#emit_sequence env eif in
          let (relse, selse) = self#emit_sequence env eelse in
          let r = join rif sif relse selse in
          self#insert (Iifthenelse(cond, sif#extract, selse#extract))
                      rarg [||];
          r
      end
  | Cswitch(esel, index, ecases, _dbg) ->
      begin match self#emit_expr env esel with
        None -> None
      | Some rsel ->
          let rscases = Array.map (self#emit_sequence env) ecases in
          let r = join_array rscases in
          self#insert (Iswitch(index,
                               Array.map (fun (_, s) -> s#extract) rscases))
                      rsel [||];
          r
      end
  | Cloop(ebody) ->
      let (_rarg, sbody) = self#emit_sequence env ebody in
      self#insert (Iloop(sbody#extract)) [||] [||];
      Some [||]
  | Ccatch(_, [], e1) ->
      self#emit_expr env e1
  | Ccatch(rec_flag, handlers, body) ->
      let handlers =
        List.map (fun (nfail, ids, e2) ->
            let rs =
              List.map
                (* CR-someday mshinwell: consider how we can do better than
                   [typ_val] when appropriate. *)
                (fun id -> let r = self#regs_for typ_val in name_regs id r; r)
                ids in
            (nfail, ids, rs, e2))
          handlers
      in
      let env =
        (* Since the handlers may be recursive, and called from the body,
           the same environment is used for translating both the handlers and
           the body. *)
        List.fold_left (fun env (nfail, _ids, rs, _e2) ->
            env_add_static_exception nfail rs env)
          env handlers
      in
      let (r_body, s_body) = self#emit_sequence env body in
      let translate_one_handler (nfail, ids, rs, e2) =
        assert(List.length ids = List.length rs);
        let new_env =
          List.fold_left (fun env (id, r) -> env_add id r env)
            env (List.combine ids rs)
        in
        let (r, s) = self#emit_sequence new_env e2 in
        (nfail, (r, s))
      in
      let l = List.map translate_one_handler handlers in
      let a = Array.of_list ((r_body, s_body) :: List.map snd l) in
      let r = join_array a in
      let aux (nfail, (_r, s)) = (nfail, s#extract) in
      self#insert (Icatch (rec_flag, List.map aux l, s_body#extract)) [||] [||];
      r
  | Cexit (nfail,args) ->
      begin match self#emit_parts_list env args with
        None -> None
      | Some (simple_list, ext_env) ->
          let src = self#emit_tuple ext_env simple_list in
          let dest_args =
            try env_find_static_exception nfail env
            with Not_found ->
              fatal_error ("Selection.emit_expr: unboun label "^
                           string_of_int nfail)
          in
          (* Intermediate registers to handle cases where some
             registers from src are present in dest *)
          let tmp_regs = Reg.createv_like src in
          (* Ccatch registers are created with type Val. They must not
             contain out of heap pointers *)
          Array.iter (fun reg -> assert(reg.typ <> Addr)) src;
          self#insert_moves src tmp_regs ;
          self#insert_moves tmp_regs (Array.concat dest_args) ;
          self#insert (Iexit nfail) [||] [||];
          None
      end
  | Ctrywith(e1, v, e2) ->
      let (r1, s1) = self#emit_sequence env e1 in
      let rv = self#regs_for typ_val in
      let (r2, s2) = self#emit_sequence (env_add v rv env) e2 in
      let r = join r1 s1 r2 s2 in
      self#insert
        (Itrywith(s1#extract,
                  instr_cons (Iop Imove) [|Proc.loc_exn_bucket|] rv
                             (s2#extract)))
        [||] [||];
      r

method private emit_sequence (env:environment) exp =
  let s = {< instr_seq = dummy_instr >} in
  let r = s#emit_expr env exp in
  (r, s)

method private bind_let (env:environment) v r1 =
  if all_regs_anonymous r1 then begin
    name_regs v r1;
    env_add v r1 env
  end else begin
    let rv = Reg.createv_like r1 in
    name_regs v rv;
    self#insert_moves r1 rv;
    env_add v rv env
  end

(* The following two functions, [emit_parts] and [emit_parts_list], force
   right-to-left evaluation order as required by the Flambda [Un_anf] pass
   (and to be consistent with the bytecode compiler). *)

method private emit_parts (env:environment) ~effects_after exp =
  let module EC = Effect_and_coeffect in
  let may_defer_evaluation =
    let ec = self#effects_of exp in
    match EC.effect ec with
    | Effect.Arbitrary | Effect.Raise ->
      (* Preserve the ordering of effectful expressions by evaluating them
         early (in the correct order) and assigning their results to
         temporaries.  We can avoid this in just one case: if we know that
         every [exp'] in the original expression list (cf. [emit_parts_list])
         to be evaluated after [exp] cannot possibly affect the result of
         [exp] or depend on the result of [exp], then [exp] may be deferred.
         (Checking purity here is not enough: we need to check copurity too
         to avoid e.g. moving mutable reads earlier than the raising of
         an exception.) *)
      EC.pure_and_copure effects_after
    | Effect.None ->
      match EC.coeffect ec with
      | Coeffect.None ->
        (* Pure expressions may be moved. *)
        true
      | Coeffect.Read_mutable -> begin
        (* Read-mutable expressions may only be deferred if evaluation of
           every [exp'] (for [exp'] as in the comment above) has no effects
           "worse" (in the sense of the ordering in [Effect.t]) than raising
           an exception. *)
        match EC.effect effects_after with
        | Effect.None | Effect.Raise -> true
        | Effect.Arbitrary -> false
      end
      | Coeffect.Arbitrary -> begin
        (* Arbitrary expressions may only be deferred if evaluation of
           every [exp'] (for [exp'] as in the comment above) has no effects. *)
        match EC.effect effects_after with
        | Effect.None -> true
        | Effect.Arbitrary | Effect.Raise -> false
      end
  in
  (* Even though some expressions may look like they can be deferred from
     the (co)effect analysis, it may be forbidden to move them. *)
  if may_defer_evaluation && self#is_simple_expr exp then
    Some (exp, env)
  else begin
    match self#emit_expr env exp with
      None -> None
    | Some r ->
        if Array.length r = 0 then
          Some (Ctuple [], env)
        else begin
          (* The normal case *)
          let id = Ident.create "bind" in
          if all_regs_anonymous r then
            (* r is an anonymous, unshared register; use it directly *)
            Some (Cvar id, env_add id r env)
          else begin
            (* Introduce a fresh temp to hold the result *)
            let tmp = Reg.createv_like r in
            self#insert_moves r tmp;
            Some (Cvar id, env_add id tmp env)
          end
        end
  end

method private emit_parts_list (env:environment) exp_list =
  let module EC = Effect_and_coeffect in
  let exp_list_right_to_left, _effect =
    (* Annotate each expression with the (co)effects that happen after it
       when the original expression list is evaluated from right to left.
       The resulting expression list has the rightmost expression first. *)
    List.fold_left (fun (exp_list, effects_after) exp ->
        let exp_effect = self#effects_of exp in
        (exp, effects_after)::exp_list, EC.join exp_effect effects_after)
      ([], EC.none)
      exp_list
  in
  List.fold_left (fun results_and_env (exp, effects_after) ->
      match results_and_env with
      | None -> None
      | Some (result, env) ->
          match self#emit_parts env exp ~effects_after with
          | None -> None
          | Some (exp_result, env) -> Some (exp_result :: result, env))
    (Some ([], env))
    exp_list_right_to_left

method private emit_tuple_not_flattened env exp_list =
  let rec emit_list = function
    [] -> []
  | exp :: rem ->
      (* Again, force right-to-left evaluation *)
      let loc_rem = emit_list rem in
      match self#emit_expr env exp with
        None -> assert false  (* should have been caught in emit_parts *)
      | Some loc_exp -> loc_exp :: loc_rem
  in
  emit_list exp_list

method private emit_tuple env exp_list =
  Array.concat (self#emit_tuple_not_flattened env exp_list)

method emit_extcall_args env args =
  let args = self#emit_tuple_not_flattened env args in
  let arg_hard_regs, stack_ofs =
    Proc.loc_external_arguments (Array.of_list args)
  in
  (* Flattening [args] and [arg_hard_regs] causes parts of values split
     across multiple registers to line up correctly, by virtue of the
     semantics of [split_int64_for_32bit_target] in cmmgen.ml, and the
     required semantics of [loc_external_arguments] (see proc.mli). *)
  let args = Array.concat args in
  let arg_hard_regs = Array.concat (Array.to_list arg_hard_regs) in
  self#insert_move_args args arg_hard_regs stack_ofs;
  arg_hard_regs, stack_ofs

method emit_stores env data regs_addr =
  let a =
    ref (Arch.offset_addressing Arch.identity_addressing (-Arch.size_int)) in
  List.iter
    (fun e ->
      let (op, arg) = self#select_store false !a e in
      match self#emit_expr env arg with
        None -> assert false
      | Some regs ->
          match op with
            Istore(_, _, _) ->
              for i = 0 to Array.length regs - 1 do
                let r = regs.(i) in
                let kind = if r.typ = Float then Double_u else Word_val in
                self#insert (Iop(Istore(kind, !a, false)))
                            (Array.append [|r|] regs_addr) [||];
                a := Arch.offset_addressing !a (size_component r.typ)
              done
          | _ ->
              self#insert (Iop op) (Array.append regs regs_addr) [||];
              a := Arch.offset_addressing !a (size_expr env e))
    data

(* Same, but in tail position *)

method private emit_return (env:environment) exp =
  match self#emit_expr env exp with
    None -> ()
  | Some r ->
      let loc = Proc.loc_results r in
      self#insert_moves r loc;
      self#insert Ireturn loc [||]

method emit_tail (env:environment) exp =
  match exp with
    Clet(v, e1, e2) ->
      begin match self#emit_expr env e1 with
        None -> ()
      | Some r1 -> self#emit_tail (self#bind_let env v r1) e2
      end
  | Cop((Capply ty) as op, args, dbg) ->
      begin match self#emit_parts_list env args with
        None -> ()
      | Some(simple_args, env) ->
          let (new_op, new_args) = self#select_operation op simple_args dbg in
          match new_op with
            Icall_ind { label_after; } ->
              let r1 = self#emit_tuple env new_args in
              let rarg = Array.sub r1 1 (Array.length r1 - 1) in
              let (loc_arg, stack_ofs) = Proc.loc_arguments rarg in
              if stack_ofs = 0 then begin
                let call = Iop (Itailcall_ind { label_after; }) in
                let spacetime_reg =
                  self#about_to_emit_call env call [| r1.(0) |]
                in
                self#insert_moves rarg loc_arg;
                self#maybe_emit_spacetime_move ~spacetime_reg;
                self#insert_debug call dbg
                            (Array.append [|r1.(0)|] loc_arg) [||];
              end else begin
                let rd = self#regs_for ty in
                let loc_res = Proc.loc_results rd in
                let spacetime_reg =
                  self#about_to_emit_call env (Iop new_op) [| r1.(0) |]
                in
                self#insert_move_args rarg loc_arg stack_ofs;
                self#maybe_emit_spacetime_move ~spacetime_reg;
                self#insert_debug (Iop new_op) dbg
                            (Array.append [|r1.(0)|] loc_arg) loc_res;
                self#insert(Iop(Istackoffset(-stack_ofs))) [||] [||];
                self#insert Ireturn loc_res [||]
              end
          | Icall_imm { func; label_after; } ->
              let r1 = self#emit_tuple env new_args in
              let (loc_arg, stack_ofs) = Proc.loc_arguments r1 in
              if stack_ofs = 0 then begin
                let call = Iop (Itailcall_imm { func; label_after; }) in
                let spacetime_reg =
                  self#about_to_emit_call env call [| |]
                in
                self#insert_moves r1 loc_arg;
                self#maybe_emit_spacetime_move ~spacetime_reg;
                self#insert_debug call dbg loc_arg [||];
              end else if func = !current_function_name then begin
                let call = Iop (Itailcall_imm { func; label_after; }) in
                let loc_arg' = Proc.loc_parameters r1 in
                let spacetime_reg =
                  self#about_to_emit_call env call [| |]
                in
                self#insert_moves r1 loc_arg';
                self#maybe_emit_spacetime_move ~spacetime_reg;
                self#insert_debug call dbg loc_arg' [||];
              end else begin
                let rd = self#regs_for ty in
                let loc_res = Proc.loc_results rd in
                let spacetime_reg =
                  self#about_to_emit_call env (Iop new_op) [| |]
                in
                self#insert_move_args r1 loc_arg stack_ofs;
                self#maybe_emit_spacetime_move ~spacetime_reg;
                self#insert_debug (Iop new_op) dbg loc_arg loc_res;
                self#insert(Iop(Istackoffset(-stack_ofs))) [||] [||];
                self#insert Ireturn loc_res [||]
              end
          | _ -> fatal_error "Selection.emit_tail"
      end
  | Csequence(e1, e2) ->
      begin match self#emit_expr env e1 with
        None -> ()
      | Some _ -> self#emit_tail env e2
      end
  | Cifthenelse(econd, eif, eelse) ->
      let (cond, earg) = self#select_condition econd in
      begin match self#emit_expr env earg with
        None -> ()
      | Some rarg ->
          self#insert (Iifthenelse(cond, self#emit_tail_sequence env eif,
                                         self#emit_tail_sequence env eelse))
                      rarg [||]
      end
  | Cswitch(esel, index, ecases, _dbg) ->
      begin match self#emit_expr env esel with
        None -> ()
      | Some rsel ->
          self#insert
            (Iswitch(index, Array.map (self#emit_tail_sequence env) ecases))
            rsel [||]
      end
  | Ccatch(_, [], e1) ->
      self#emit_tail env e1
  | Ccatch(rec_flag, handlers, e1) ->
      let handlers =
        List.map (fun (nfail, ids, e2) ->
            let rs =
              List.map
                (fun id -> let r = self#regs_for typ_val in name_regs id r; r)
                ids in
            (nfail, ids, rs, e2))
          handlers in
      let env =
        List.fold_left (fun env (nfail, _ids, rs, _e2) ->
            env_add_static_exception nfail rs env)
          env handlers in
      let s_body = self#emit_tail_sequence env e1 in
      let aux (nfail, ids, rs, e2) =
        assert(List.length ids = List.length rs);
        let new_env =
          List.fold_left
            (fun env (id,r) -> env_add id r env)
            env (List.combine ids rs) in
        nfail, self#emit_tail_sequence new_env e2
      in
      self#insert (Icatch(rec_flag, List.map aux handlers, s_body)) [||] [||]
  | Ctrywith(e1, v, e2) ->
      let (opt_r1, s1) = self#emit_sequence env e1 in
      let rv = self#regs_for typ_val in
      let s2 = self#emit_tail_sequence (env_add v rv env) e2 in
      self#insert
        (Itrywith(s1#extract,
                  instr_cons (Iop Imove) [|Proc.loc_exn_bucket|] rv s2))
        [||] [||];
      begin match opt_r1 with
        None -> ()
      | Some r1 ->
          let loc = Proc.loc_results r1 in
          self#insert_moves r1 loc;
          self#insert Ireturn loc [||]
      end
  | _ ->
      self#emit_return env exp

method private emit_tail_sequence env exp =
  let s = {< instr_seq = dummy_instr >} in
  s#emit_tail env exp;
  s#extract

(* Insertion of the function prologue *)

method insert_prologue _f ~loc_arg ~rarg ~spacetime_node_hole:_ ~env:_ =
  self#insert_moves loc_arg rarg;
  None

(* Sequentialization of a function definition *)

method initial_env () = env_empty

method emit_fundecl f =
  Proc.contains_calls := false;
  current_function_name := f.Cmm.fun_name;
  let rargs =
    List.map
      (fun (id, ty) -> let r = self#regs_for ty in name_regs id r; r)
      f.Cmm.fun_args in
  let rarg = Array.concat rargs in
  let loc_arg = Proc.loc_parameters rarg in
  (* To make it easier to add the Spacetime instrumentation code, we
     first emit the body and extract the resulting instruction sequence;
     then we emit the prologue followed by any Spacetime instrumentation.  The
     sequence resulting from extracting the latter (prologue + instrumentation)
     together is then simply prepended to the body. *)
  let env =
    List.fold_right2
      (fun (id, _ty) r env -> env_add id r env)
      f.Cmm.fun_args rargs (self#initial_env ()) in
  let spacetime_node_hole, env =
    if not Config.spacetime then None, env
    else begin
      let reg = self#regs_for typ_int in
      let node_hole = Ident.create "spacetime_node_hole" in
      Some (node_hole, reg), env_add node_hole reg env
    end
  in
  self#emit_tail env f.Cmm.fun_body;
  let body = self#extract in
  instr_seq <- dummy_instr;
  let fun_spacetime_shape =
    self#insert_prologue f ~loc_arg ~rarg ~spacetime_node_hole ~env
  in
  let body = self#extract_core ~end_instr:body in
  instr_iter (fun instr -> self#mark_instr instr.Mach.desc) body;
  { fun_name = f.Cmm.fun_name;
    fun_args = loc_arg;
    fun_body = body;
    fun_fast = f.Cmm.fun_fast;
    fun_dbg  = f.Cmm.fun_dbg;
    fun_spacetime_shape;
  }

end

(* Tail call criterion (estimated).  Assumes:
- all arguments are of type "int" (always the case for OCaml function calls)
- one extra argument representing the closure environment (conservative).
*)

let is_tail_call nargs =
  assert (Reg.dummy.typ = Int);
  let args = Array.make (nargs + 1) Reg.dummy in
  let (_loc_arg, stack_ofs) = Proc.loc_arguments args in
  stack_ofs = 0

let _ =
  Simplif.is_tail_native_heuristic := is_tail_call

let reset () =
  current_function_name := ""
