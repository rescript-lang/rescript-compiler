(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Selection of pseudo-instructions, assignment of pseudo-registers,
   sequentialization. *)

open Misc
open Cmm
open Reg
open Mach

type environment = (Ident.t, Reg.t array) Tbl.t

(* Infer the type of the result of an operation *)

let oper_result_type = function
    Capply(ty, _) -> ty
  | Cextcall(s, ty, alloc, _) -> ty
  | Cload c ->
      begin match c with
        Word -> typ_addr
      | Single | Double | Double_u -> typ_float
      | _ -> typ_int
      end
  | Calloc -> typ_addr
  | Cstore c -> typ_void
  | Caddi | Csubi | Cmuli | Cmulhi | Cdivi | Cmodi |
    Cand | Cor | Cxor | Clsl | Clsr | Casr |
    Ccmpi _ | Ccmpa _ | Ccmpf _ -> typ_int
  | Cadda | Csuba -> typ_addr
  | Cnegf | Cabsf | Caddf | Csubf | Cmulf | Cdivf -> typ_float
  | Cfloatofint -> typ_float
  | Cintoffloat -> typ_int
  | Craise _ -> typ_void
  | Ccheckbound _ -> typ_void

(* Infer the size in bytes of the result of a simple expression *)

let size_expr env exp =
  let rec size localenv = function
      Cconst_int _ | Cconst_natint _
    | Cconst_blockheader _ -> Arch.size_int
    | Cconst_symbol _ | Cconst_pointer _ | Cconst_natpointer _ ->
        Arch.size_addr
    | Cconst_float _ -> Arch.size_float
    | Cvar id ->
        begin try
          Tbl.find id localenv
        with Not_found ->
        try
          let regs = Tbl.find id env in
          size_machtype (Array.map (fun r -> r.typ) regs)
        with Not_found ->
          fatal_error("Selection.size_expr: unbound var " ^
                      Ident.unique_name id)
        end
    | Ctuple el ->
        List.fold_right (fun e sz -> size localenv e + sz) el 0
    | Cop(op, args) ->
        size_machtype(oper_result_type op)
    | Clet(id, arg, body) ->
        size (Tbl.add id (size localenv arg) localenv) body
    | Csequence(e1, e2) ->
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
        if Reg.anonymous r1.(i) then begin
          r.(i) <- r1.(i);
          seq2#insert_move r2.(i) r1.(i)
        end else if Reg.anonymous r2.(i) then begin
          r.(i) <- r2.(i);
          seq1#insert_move r1.(i) r2.(i)
        end else begin
          r.(i) <- Reg.create r1.(i).typ;
          seq1#insert_move r1.(i) r.(i);
          seq2#insert_move r2.(i) r.(i)
        end
      done;
      Some r

(* Same, for N branches *)

let join_array rs =
  let some_res = ref None in
  for i = 0 to Array.length rs - 1 do
    let (r, s) = rs.(i) in
    if r <> None then some_res := r
  done;
  match !some_res with
    None -> None
  | Some template ->
      let size_res = Array.length template in
      let res = Array.make size_res Reg.dummy in
      for i = 0 to size_res - 1 do
        res.(i) <- Reg.create template.(i).typ
      done;
      for i = 0 to Array.length rs - 1 do
        let (r, s) = rs.(i) in
        match r with
          None -> ()
        | Some r -> s#insert_moves r res
      done;
      Some res

(* Extract debug info contained in a C-- operation *)
let debuginfo_op = function
  | Capply(_, dbg) -> dbg
  | Cextcall(_, _, _, dbg) -> dbg
  | Craise (_, dbg) -> dbg
  | Ccheckbound dbg -> dbg
  | _ -> Debuginfo.none

(* Registers for catch constructs *)
let catch_regs = ref []

(* Name of function being compiled *)
let current_function_name = ref ""

(* The default instruction selection class *)

class virtual selector_generic = object (self)

(* Says if an expression is "simple". A "simple" expression has no
   side-effects and its execution can be delayed until its value
   is really needed. In the case of e.g. an [alloc] instruction,
   the non-simple arguments are computed in right-to-left order
   first, then the block is allocated, then the simple arguments are
   evaluated and stored. *)

method is_simple_expr = function
    Cconst_int _ -> true
  | Cconst_natint _ -> true
  | Cconst_blockheader _ -> true
  | Cconst_float _ -> true
  | Cconst_symbol _ -> true
  | Cconst_pointer _ -> true
  | Cconst_natpointer _ -> true
  | Cvar _ -> true
  | Ctuple el -> List.for_all self#is_simple_expr el
  | Clet(id, arg, body) -> self#is_simple_expr arg && self#is_simple_expr body
  | Csequence(e1, e2) -> self#is_simple_expr e1 && self#is_simple_expr e2
  | Cop(op, args) ->
      begin match op with
        (* The following may have side effects *)
      | Capply _ | Cextcall _ | Calloc | Cstore _ | Craise _ -> false
        (* The remaining operations are simple if their args are *)
      | _ ->
          List.for_all self#is_simple_expr args
      end
  | _ -> false

(* Says whether an integer constant is a suitable immediate argument *)

method virtual is_immediate : int -> bool

(* Selection of addressing modes *)

method virtual select_addressing :
  Cmm.memory_chunk -> Cmm.expression -> Arch.addressing_mode * Cmm.expression

(* Default instruction selection for stores (of words) *)

method select_store is_assign addr arg =
  (Istore(Word, addr, is_assign), arg)

(* call marking methods, documented in selectgen.mli *)

method mark_call =
  Proc.contains_calls := true

method mark_tailcall = ()

method mark_c_tailcall = ()

method mark_instr = function
  | Iop (Icall_ind | Icall_imm _ | Iextcall _) ->
      self#mark_call
  | Iop (Itailcall_ind | Itailcall_imm _) ->
      self#mark_tailcall
  | Iop (Ialloc _) ->
      self#mark_call (* caml_alloc*, caml_garbage_collection *)
  | Iop (Iintop Icheckbound | Iintop_imm(Icheckbound, _)) ->
      self#mark_c_tailcall (* caml_ml_array_bound_error *)
  | Iraise raise_kind ->
    begin match raise_kind with
      | Lambda.Raise_notrace -> ()
      | Lambda.Raise_regular | Lambda.Raise_reraise ->
        if !Clflags.debug then (* PR#6239 *)
        (* caml_stash_backtrace; we #mark_call rather than
           #mark_c_tailcall to get a good stack backtrace *)
          self#mark_call
    end
  | Itrywith _ ->
    self#mark_call
  | _ -> ()

(* Default instruction selection for operators *)

method select_operation op args =
  match (op, args) with
    (Capply(ty, dbg), Cconst_symbol s :: rem) -> (Icall_imm s, rem)
  | (Capply(ty, dbg), _) -> (Icall_ind, args)
  | (Cextcall(s, ty, alloc, dbg), _) -> (Iextcall(s, alloc), args)
  | (Cload chunk, [arg]) ->
      let (addr, eloc) = self#select_addressing chunk arg in
      (Iload(chunk, addr), [eloc])
  | (Cstore chunk, [arg1; arg2]) ->
      let (addr, eloc) = self#select_addressing chunk arg1 in
      if chunk = Word then begin
        let (op, newarg2) = self#select_store true addr arg2 in
        (op, [newarg2; eloc])
      end else begin
        (Istore(chunk, addr, true), [arg2; eloc])
        (* Inversion addr/datum in Istore *)
      end
  | (Calloc, _) -> (Ialloc 0, args)
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
  | (Cadda, _) -> self#select_arith_comm Iadd args
  | (Csuba, _) -> self#select_arith Isub args
  | (Ccmpa comp, _) -> self#select_arith_comp (Iunsigned comp) args
  | (Cnegf, _) -> (Inegf, args)
  | (Cabsf, _) -> (Iabsf, args)
  | (Caddf, _) -> (Iaddf, args)
  | (Csubf, _) -> (Isubf, args)
  | (Cmulf, _) -> (Imulf, args)
  | (Cdivf, _) -> (Idivf, args)
  | (Cfloatofint, _) -> (Ifloatofint, args)
  | (Cintoffloat, _) -> (Iintoffloat, args)
  | (Ccheckbound _, _) -> self#select_arith Icheckbound args
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
    Cop(Ccmpi cmp, [arg1; Cconst_int n]) when self#is_immediate n ->
      (Iinttest_imm(Isigned cmp, n), arg1)
  | Cop(Ccmpi cmp, [Cconst_int n; arg2]) when self#is_immediate n ->
      (Iinttest_imm(Isigned(swap_comparison cmp), n), arg2)
  | Cop(Ccmpi cmp, [arg1; Cconst_pointer n]) when self#is_immediate n ->
      (Iinttest_imm(Isigned cmp, n), arg1)
  | Cop(Ccmpi cmp, [Cconst_pointer n; arg2]) when self#is_immediate n ->
      (Iinttest_imm(Isigned(swap_comparison cmp), n), arg2)
  | Cop(Ccmpi cmp, args) ->
      (Iinttest(Isigned cmp), Ctuple args)
  | Cop(Ccmpa cmp, [arg1; Cconst_pointer n]) when self#is_immediate n ->
      (Iinttest_imm(Iunsigned cmp, n), arg1)
  | Cop(Ccmpa cmp, [arg1; Cconst_int n]) when self#is_immediate n ->
      (Iinttest_imm(Iunsigned cmp, n), arg1)
  | Cop(Ccmpa cmp, [Cconst_pointer n; arg2]) when self#is_immediate n ->
      (Iinttest_imm(Iunsigned(swap_comparison cmp), n), arg2)
  | Cop(Ccmpa cmp, [Cconst_int n; arg2]) when self#is_immediate n ->
      (Iinttest_imm(Iunsigned(swap_comparison cmp), n), arg2)
  | Cop(Ccmpa cmp, args) ->
      (Iinttest(Iunsigned cmp), Ctuple args)
  | Cop(Ccmpf cmp, args) ->
      (Ifloattest(cmp, false), Ctuple args)
  | Cop(Cand, [arg; Cconst_int 1]) ->
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

method extract =
  let rec extract res i =
    if i == dummy_instr
    then res
    else extract {i with next = res} i.next in
  extract (end_instr()) instr_seq

(* Insert a sequence of moves from one pseudoreg set to another. *)

method insert_move src dst =
  if src.stamp <> dst.stamp then
    self#insert (Iop Imove) [|src|] [|dst|]

method insert_moves src dst =
  for i = 0 to min (Array.length src) (Array.length dst) - 1 do
    self#insert_move src.(i) dst.(i)
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

(* Add the instructions for the given expression
   at the end of the self sequence *)

method emit_expr env exp =
  match exp with
    Cconst_int n ->
      let r = self#regs_for typ_int in
      Some(self#insert_op (Iconst_int(Nativeint.of_int n)) [||] r)
  | Cconst_natint n ->
      let r = self#regs_for typ_int in
      Some(self#insert_op (Iconst_int n) [||] r)
  | Cconst_blockheader n ->
      let r = self#regs_for typ_int in
      Some(self#insert_op (Iconst_blockheader n) [||] r)
  | Cconst_float n ->
      let r = self#regs_for typ_float in
      Some(self#insert_op (Iconst_float n) [||] r)
  | Cconst_symbol n ->
      let r = self#regs_for typ_addr in
      Some(self#insert_op (Iconst_symbol n) [||] r)
  | Cconst_pointer n ->
      let r = self#regs_for typ_addr in
      Some(self#insert_op (Iconst_int(Nativeint.of_int n)) [||] r)
  | Cconst_natpointer n ->
      let r = self#regs_for typ_addr in
      Some(self#insert_op (Iconst_int n) [||] r)
  | Cvar v ->
      begin try
        Some(Tbl.find v env)
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
          Tbl.find v env
        with Not_found ->
          fatal_error ("Selection.emit_expr: unbound var " ^ Ident.name v) in
      begin match self#emit_expr env e1 with
        None -> None
      | Some r1 -> self#insert_moves r1 rv; Some [||]
      end
  | Ctuple [] ->
      Some [||]
  | Ctuple exp_list ->
      begin match self#emit_parts_list env exp_list with
        None -> None
      | Some(simple_list, ext_env) ->
          Some(self#emit_tuple ext_env simple_list)
      end
  | Cop(Craise (k, dbg), [arg]) ->
      begin match self#emit_expr env arg with
        None -> None
      | Some r1 ->
          let rd = [|Proc.loc_exn_bucket|] in
          self#insert (Iop Imove) r1 rd;
          self#insert_debug (Iraise k) dbg rd [||];
          None
      end
  | Cop(Ccmpf comp, args) ->
      self#emit_expr env (Cifthenelse(exp, Cconst_int 1, Cconst_int 0))
  | Cop(op, args) ->
      begin match self#emit_parts_list env args with
        None -> None
      | Some(simple_args, env) ->
          let ty = oper_result_type op in
          let (new_op, new_args) = self#select_operation op simple_args in
          let dbg = debuginfo_op op in
          match new_op with
            Icall_ind ->
              let r1 = self#emit_tuple env new_args in
              let rarg = Array.sub r1 1 (Array.length r1 - 1) in
              let rd = self#regs_for ty in
              let (loc_arg, stack_ofs) = Proc.loc_arguments rarg in
              let loc_res = Proc.loc_results rd in
              self#insert_move_args rarg loc_arg stack_ofs;
              self#insert_debug (Iop Icall_ind) dbg
                          (Array.append [|r1.(0)|] loc_arg) loc_res;
              self#insert_move_results loc_res rd stack_ofs;
              Some rd
          | Icall_imm lbl ->
              let r1 = self#emit_tuple env new_args in
              let rd = self#regs_for ty in
              let (loc_arg, stack_ofs) = Proc.loc_arguments r1 in
              let loc_res = Proc.loc_results rd in
              self#insert_move_args r1 loc_arg stack_ofs;
              self#insert_debug (Iop(Icall_imm lbl)) dbg loc_arg loc_res;
              self#insert_move_results loc_res rd stack_ofs;
              Some rd
          | Iextcall(lbl, alloc) ->
              let (loc_arg, stack_ofs) =
                self#emit_extcall_args env new_args in
              let rd = self#regs_for ty in
              let loc_res = self#insert_op_debug (Iextcall(lbl, alloc)) dbg
                                    loc_arg (Proc.loc_external_results rd) in
              self#insert_move_results loc_res rd stack_ofs;
              Some rd
          | Ialloc _ ->
              let rd = self#regs_for typ_addr in
              let size = size_expr env (Ctuple new_args) in
              self#insert (Iop(Ialloc size)) [||] rd;
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
      | Some r1 -> self#emit_expr env e2
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
  | Cswitch(esel, index, ecases) ->
      begin match self#emit_expr env esel with
        None -> None
      | Some rsel ->
          let rscases = Array.map (self#emit_sequence env) ecases in
          let r = join_array rscases in
          self#insert (Iswitch(index,
                               Array.map (fun (r, s) -> s#extract) rscases))
                      rsel [||];
          r
      end
  | Cloop(ebody) ->
      let (rarg, sbody) = self#emit_sequence env ebody in
      self#insert (Iloop(sbody#extract)) [||] [||];
      Some [||]
  | Ccatch(nfail, ids, e1, e2) ->
      let rs =
        List.map
          (fun id ->
            let r = self#regs_for typ_addr in name_regs id r; r)
          ids in
      catch_regs := (nfail, Array.concat rs) :: !catch_regs ;
      let (r1, s1) = self#emit_sequence env e1 in
      catch_regs := List.tl !catch_regs ;
      let new_env =
        List.fold_left
        (fun env (id,r) -> Tbl.add id r env)
        env (List.combine ids rs) in
      let (r2, s2) = self#emit_sequence new_env e2 in
      let r = join r1 s1 r2 s2 in
      self#insert (Icatch(nfail, s1#extract, s2#extract)) [||] [||];
      r
  | Cexit (nfail,args) ->
      begin match self#emit_parts_list env args with
        None -> None
      | Some (simple_list, ext_env) ->
          let src = self#emit_tuple ext_env simple_list in
          let dest =
            try List.assoc nfail !catch_regs
            with Not_found ->
              Misc.fatal_error
                ("Selectgen.emit_expr, on exit("^string_of_int nfail^")") in
          self#insert_moves src dest ;
          self#insert (Iexit nfail) [||] [||];
          None
      end
  | Ctrywith(e1, v, e2) ->
      let (r1, s1) = self#emit_sequence env e1 in
      let rv = self#regs_for typ_addr in
      let (r2, s2) = self#emit_sequence (Tbl.add v rv env) e2 in
      let r = join r1 s1 r2 s2 in
      self#insert
        (Itrywith(s1#extract,
                  instr_cons (Iop Imove) [|Proc.loc_exn_bucket|] rv
                             (s2#extract)))
        [||] [||];
      r

method private emit_sequence env exp =
  let s = {< instr_seq = dummy_instr >} in
  let r = s#emit_expr env exp in
  (r, s)

method private bind_let env v r1 =
  if all_regs_anonymous r1 then begin
    name_regs v r1;
    Tbl.add v r1 env
  end else begin
    let rv = Reg.createv_like r1 in
    name_regs v rv;
    self#insert_moves r1 rv;
    Tbl.add v rv env
  end

method private emit_parts env exp =
  if self#is_simple_expr exp then
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
            Some (Cvar id, Tbl.add id r env)
          else begin
            (* Introduce a fresh temp to hold the result *)
            let tmp = Reg.createv_like r in
            self#insert_moves r tmp;
            Some (Cvar id, Tbl.add id tmp env)
          end
        end
  end

method private emit_parts_list env exp_list =
  match exp_list with
    [] -> Some ([], env)
  | exp :: rem ->
      (* This ensures right-to-left evaluation, consistent with the
         bytecode compiler *)
      match self#emit_parts_list env rem with
        None -> None
      | Some(new_rem, new_env) ->
          match self#emit_parts new_env exp with
            None -> None
          | Some(new_exp, fin_env) -> Some(new_exp :: new_rem, fin_env)

method private emit_tuple env exp_list =
  let rec emit_list = function
    [] -> []
  | exp :: rem ->
      (* Again, force right-to-left evaluation *)
      let loc_rem = emit_list rem in
      match self#emit_expr env exp with
        None -> assert false  (* should have been caught in emit_parts *)
      | Some loc_exp -> loc_exp :: loc_rem in
  Array.concat(emit_list exp_list)

method emit_extcall_args env args =
  let r1 = self#emit_tuple env args in
  let (loc_arg, stack_ofs as arg_stack) = Proc.loc_external_arguments r1 in
  self#insert_move_args r1 loc_arg stack_ofs;
  arg_stack

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
                let kind = if r.typ = Float then Double_u else Word in
                self#insert (Iop(Istore(kind, !a, false)))
                            (Array.append [|r|] regs_addr) [||];
                a := Arch.offset_addressing !a (size_component r.typ)
              done
          | _ ->
              self#insert (Iop op) (Array.append regs regs_addr) [||];
              a := Arch.offset_addressing !a (size_expr env e))
    data

(* Same, but in tail position *)

method private emit_return env exp =
  match self#emit_expr env exp with
    None -> ()
  | Some r ->
      let loc = Proc.loc_results r in
      self#insert_moves r loc;
      self#insert Ireturn loc [||]

method emit_tail env exp =
  match exp with
    Clet(v, e1, e2) ->
      begin match self#emit_expr env e1 with
        None -> ()
      | Some r1 -> self#emit_tail (self#bind_let env v r1) e2
      end
  | Cop(Capply(ty, dbg) as op, args) ->
      begin match self#emit_parts_list env args with
        None -> ()
      | Some(simple_args, env) ->
          let (new_op, new_args) = self#select_operation op simple_args in
          match new_op with
            Icall_ind ->
              let r1 = self#emit_tuple env new_args in
              let rarg = Array.sub r1 1 (Array.length r1 - 1) in
              let (loc_arg, stack_ofs) = Proc.loc_arguments rarg in
              if stack_ofs = 0 then begin
                self#insert_moves rarg loc_arg;
                self#insert (Iop Itailcall_ind)
                            (Array.append [|r1.(0)|] loc_arg) [||]
              end else begin
                let rd = self#regs_for ty in
                let loc_res = Proc.loc_results rd in
                self#insert_move_args rarg loc_arg stack_ofs;
                self#insert_debug (Iop Icall_ind) dbg
                            (Array.append [|r1.(0)|] loc_arg) loc_res;
                self#insert(Iop(Istackoffset(-stack_ofs))) [||] [||];
                self#insert Ireturn loc_res [||]
              end
          | Icall_imm lbl ->
              let r1 = self#emit_tuple env new_args in
              let (loc_arg, stack_ofs) = Proc.loc_arguments r1 in
              if stack_ofs = 0 then begin
                self#insert_moves r1 loc_arg;
                self#insert (Iop(Itailcall_imm lbl)) loc_arg [||]
              end else if lbl = !current_function_name then begin
                let loc_arg' = Proc.loc_parameters r1 in
                self#insert_moves r1 loc_arg';
                self#insert (Iop(Itailcall_imm lbl)) loc_arg' [||]
              end else begin
                let rd = self#regs_for ty in
                let loc_res = Proc.loc_results rd in
                self#insert_move_args r1 loc_arg stack_ofs;
                self#insert_debug (Iop(Icall_imm lbl)) dbg loc_arg loc_res;
                self#insert(Iop(Istackoffset(-stack_ofs))) [||] [||];
                self#insert Ireturn loc_res [||]
              end
          | _ -> fatal_error "Selection.emit_tail"
      end
  | Csequence(e1, e2) ->
      begin match self#emit_expr env e1 with
        None -> ()
      | Some r1 -> self#emit_tail env e2
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
  | Cswitch(esel, index, ecases) ->
      begin match self#emit_expr env esel with
        None -> ()
      | Some rsel ->
          self#insert
            (Iswitch(index, Array.map (self#emit_tail_sequence env) ecases))
            rsel [||]
      end
  | Ccatch(nfail, ids, e1, e2) ->
       let rs =
        List.map
          (fun id ->
            let r = self#regs_for typ_addr in
            name_regs id r  ;
            r)
          ids in
      catch_regs := (nfail, Array.concat rs) :: !catch_regs ;
      let s1 = self#emit_tail_sequence env e1 in
      catch_regs := List.tl !catch_regs ;
      let new_env =
        List.fold_left
        (fun env (id,r) -> Tbl.add id r env)
        env (List.combine ids rs) in
      let s2 = self#emit_tail_sequence new_env e2 in
      self#insert (Icatch(nfail, s1, s2)) [||] [||]
  | Ctrywith(e1, v, e2) ->
      let (opt_r1, s1) = self#emit_sequence env e1 in
      let rv = self#regs_for typ_addr in
      let s2 = self#emit_tail_sequence (Tbl.add v rv env) e2 in
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

(* Sequentialization of a function definition *)

method emit_fundecl f =
  Proc.contains_calls := false;
  current_function_name := f.Cmm.fun_name;
  let rargs =
    List.map
      (fun (id, ty) -> let r = self#regs_for ty in name_regs id r; r)
      f.Cmm.fun_args in
  let rarg = Array.concat rargs in
  let loc_arg = Proc.loc_parameters rarg in
  let env =
    List.fold_right2
      (fun (id, ty) r env -> Tbl.add id r env)
      f.Cmm.fun_args rargs Tbl.empty in
  self#insert_moves loc_arg rarg;
  self#emit_tail env f.Cmm.fun_body;
  let body = self#extract in
  instr_iter (fun instr -> self#mark_instr instr.Mach.desc) body;
  { fun_name = f.Cmm.fun_name;
    fun_args = loc_arg;
    fun_body = body;
    fun_fast = f.Cmm.fun_fast;
    fun_dbg  = f.Cmm.fun_dbg }

end

(* Tail call criterion (estimated).  Assumes:
- all arguments are of type "int" (always the case for OCaml function calls)
- one extra argument representing the closure environment (conservative).
*)

let is_tail_call nargs =
  assert (Reg.dummy.typ = Int);
  let args = Array.make (nargs + 1) Reg.dummy in
  let (loc_arg, stack_ofs) = Proc.loc_arguments args in
  stack_ofs = 0

let _ =
  Simplif.is_tail_native_heuristic := is_tail_call

let reset () =
  catch_regs := [];
  current_function_name := ""
