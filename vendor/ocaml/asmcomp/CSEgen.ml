(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Common subexpression elimination by value numbering over extended
   basic blocks. *)

open Mach

type valnum = int

(* Classification of operations *)

type op_class =
  | Op_pure           (* pure arithmetic, produce one or several result *)
  | Op_checkbound     (* checkbound-style: no result, can raise an exn *)
  | Op_load           (* memory load *)
  | Op_store of bool  (* memory store, false = init, true = assign *)
  | Op_other   (* anything else that does not allocate nor store in memory *)

(* We maintain sets of equations of the form
       valnums = operation(valnums)
   plus a mapping from registers to valnums (value numbers). *)

type rhs = operation * valnum array

module Equations = struct
  module Rhs_map =
    Map.Make(struct type t = rhs let compare = Pervasives.compare end)

  type 'a t =
    { load_equations : 'a Rhs_map.t;
      other_equations : 'a Rhs_map.t }

  let empty =
    { load_equations = Rhs_map.empty;
      other_equations = Rhs_map.empty }

  let add op_class op v m =
    match op_class with
    | Op_load ->
      { m with load_equations = Rhs_map.add op v m.load_equations }
    | _ ->
      { m with other_equations = Rhs_map.add op v m.other_equations }

  let find op_class op m =
    match op_class with
    | Op_load ->
      Rhs_map.find op m.load_equations
    | _ ->
      Rhs_map.find op m.other_equations

  let remove_loads m =
    { load_equations = Rhs_map.empty;
      other_equations = m.other_equations }
end

type numbering =
  { num_next: int;                      (* next fresh value number *)
    num_eqs: valnum array Equations.t;  (* mapping rhs -> valnums *)
    num_reg: valnum Reg.Map.t }         (* mapping register -> valnum *)

let empty_numbering =
  { num_next = 0; num_eqs = Equations.empty; num_reg = Reg.Map.empty }

(** Generate a fresh value number [v] and associate it to register [r].
  Returns a pair [(n',v)] with the updated value numbering [n']. *)

let fresh_valnum_reg n r =
  let v = n.num_next in
  ({n with num_next = v + 1; num_reg = Reg.Map.add r v n.num_reg}, v)

(* Same, for a set of registers [rs]. *)

let array_fold_transf (f: numbering -> 'a -> numbering * 'b) n (a: 'a array)
                      : numbering * 'b array =
  match Array.length a with
  | 0 -> (n, [||])
  | 1 -> let (n', b) = f n a.(0) in (n', [|b|])
  | l -> let b = Array.make l 0 and n = ref n in
         for i = 0 to l - 1 do
           let (n', x) = f !n a.(i) in
           b.(i) <- x; n := n'
         done;
         (!n, b)

let fresh_valnum_regs n rs =
  array_fold_transf fresh_valnum_reg n rs

(** [valnum_reg n r] returns the value number for the contents of
  register [r].  If none exists, a fresh value number is returned
  and associated with register [r].  The possibly updated numbering
  is also returned.  [valnum_regs] is similar, but for an array of
  registers. *)

let valnum_reg n r =
  try
    (n, Reg.Map.find r n.num_reg)
  with Not_found ->
    fresh_valnum_reg n r

let valnum_regs n rs =
  array_fold_transf valnum_reg n rs

(* Look up the set of equations for an equation with the given rhs.
   Return [Some res] if there is one, where [res] is the lhs. *)

let find_equation op_class n rhs =
  try
    Some(Equations.find op_class rhs n.num_eqs)
  with Not_found ->
    None

(* Find a register containing the given value number. *)

let find_reg_containing n v =
  Reg.Map.fold (fun r v' res -> if v' = v then Some r else res)
               n.num_reg None

(* Find a set of registers containing the given value numbers. *)

let find_regs_containing n vs =
  match Array.length vs with
  | 0 -> Some [||]
  | 1 -> begin match find_reg_containing n vs.(0) with
         | None -> None
         | Some r -> Some [|r|]
         end
  | l -> let rs = Array.make l Reg.dummy in
         begin try
           for i = 0 to l - 1 do
             match find_reg_containing n vs.(i) with
             | None -> raise Exit
             | Some r -> rs.(i) <- r
           done;
           Some rs
         with Exit ->
           None
         end

(* Associate the given value number to the given result register,
   without adding new equations. *)

let set_known_reg n r v =
  { n with num_reg = Reg.Map.add r v n.num_reg }

(* Associate the given value numbers to the given result registers,
   without adding new equations. *)

let array_fold2 f n a1 a2 =
  let l = Array.length a1 in
  assert (l = Array.length a2);
  let n = ref n in
  for i = 0 to l - 1 do n := f !n a1.(i) a2.(i) done;
  !n

let set_known_regs n rs vs =
  array_fold2 set_known_reg n rs vs

(* Record the effect of a move: no new equations, but the result reg
   maps to the same value number as the argument reg. *)

let set_move n src dst =
  let (n1, v) = valnum_reg n src in
  { n1 with num_reg = Reg.Map.add dst v n1.num_reg }

(* Record the equation [fresh valnums = rhs] and associate the given
   result registers [rs] to [fresh valnums]. *)

let set_fresh_regs n rs rhs op_class =
  let (n1, vs) = fresh_valnum_regs n rs in
  { n1 with num_eqs = Equations.add op_class rhs vs n.num_eqs }

(* Forget everything we know about the given result registers,
   which are receiving unpredictable values at run-time. *)

let set_unknown_regs n rs =
  { n with num_reg = Array.fold_right Reg.Map.remove rs n.num_reg }

(* Keep only the equations satisfying the given predicate. *)

let remove_load_numbering n =
  { n with num_eqs = Equations.remove_loads n.num_eqs }

(* Forget everything we know about registers of type [Addr]. *)

let kill_addr_regs n =
  { n with num_reg =
              Reg.Map.filter (fun r _n -> r.Reg.typ <> Cmm.Addr) n.num_reg }

(* Prepend a set of moves before [i] to assign [srcs] to [dsts].  *)

let insert_single_move i src dst = instr_cons (Iop Imove) [|src|] [|dst|] i

let insert_move srcs dsts i =
  match Array.length srcs with
  | 0 -> i
  | 1 -> instr_cons (Iop Imove) srcs dsts i
  | _ -> (* Parallel move: first copy srcs into tmps one by one,
            then copy tmps into dsts one by one *)
         let tmps = Reg.createv_like srcs in
         let i1 = array_fold2 insert_single_move i tmps dsts in
         array_fold2 insert_single_move i1 srcs tmps

class cse_generic = object (self)

(* Default classification of operations.  Can be overridden in
   processor-specific files to classify specific operations better. *)

method class_of_operation op =
  match op with
  | Imove | Ispill | Ireload -> assert false   (* treated specially *)
  | Iconst_int _ | Iconst_float _ | Iconst_symbol _ -> Op_pure
  | Icall_ind _ | Icall_imm _ | Itailcall_ind _ | Itailcall_imm _
  | Iextcall _ -> assert false                 (* treated specially *)
  | Istackoffset _ -> Op_other
  | Iload(_,_) -> Op_load
  | Istore(_,_,asg) -> Op_store asg
  | Ialloc _ -> assert false                   (* treated specially *)
  | Iintop(Icheckbound _) -> Op_checkbound
  | Iintop _ -> Op_pure
  | Iintop_imm(Icheckbound _, _) -> Op_checkbound
  | Iintop_imm(_, _) -> Op_pure
  | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
  | Ifloatofint | Iintoffloat -> Op_pure
  | Ispecific _ -> Op_other
  | Iname_for_debugger _ -> Op_pure

(* Operations that are so cheap that it isn't worth factoring them. *)

method is_cheap_operation op =
  match op with
  | Iconst_int _ -> true
  | _ -> false

(* Forget all equations involving memory loads.  Performed after a
   non-initializing store *)

method private kill_loads n =
  remove_load_numbering n

(* Perform CSE on the given instruction [i] and its successors.
   [n] is the value numbering current at the beginning of [i]. *)

method private cse n i =
  match i.desc with
  | Iend | Ireturn | Iop(Itailcall_ind _) | Iop(Itailcall_imm _)
  | Iexit _ | Iraise _ ->
      i
  | Iop (Imove | Ispill | Ireload) ->
      (* For moves, we associate the same value number to the result reg
         as to the argument reg. *)
      let n1 = set_move n i.arg.(0) i.res.(0) in
      {i with next = self#cse n1 i.next}
  | Iop (Icall_ind _ | Icall_imm _ | Iextcall _) ->
      (* For function calls, we should at least forget:
         - equations involving memory loads, since the callee can
           perform arbitrary memory stores;
         - equations involving arithmetic operations that can
           produce [Addr]-typed derived pointers into the heap
           (see below for Ialloc);
         - mappings from hardware registers to value numbers,
           since the callee does not preserve these registers.
         That doesn't leave much usable information: checkbounds
         could be kept, but won't be usable for CSE as one of their
         arguments is always a memory load.  For simplicity, we
         just forget everything. *)
      {i with next = self#cse empty_numbering i.next}
  | Iop (Ialloc _) ->
      (* For allocations, we must avoid extending the live range of a
         pseudoregister across the allocation if this pseudoreg
         is a derived heap pointer (a pointer into the heap that does
         not point to the beginning of a Caml block).  PR#6484 is an
         example of this situation.  Such pseudoregs have type [Addr].
         Pseudoregs with types other than [Addr] can be kept.
         Moreover, allocation can trigger the asynchronous execution
         of arbitrary Caml code (finalizer, signal handler, context
         switch), which can contain non-initializing stores.
         Hence, all equations over loads must be removed. *)
       let n1 = kill_addr_regs (self#kill_loads n) in
       let n2 = set_unknown_regs n1 i.res in
       {i with next = self#cse n2 i.next}
  | Iop op ->
      begin match self#class_of_operation op with
      | (Op_pure | Op_checkbound | Op_load) as op_class ->
          let (n1, varg) = valnum_regs n i.arg in
          let n2 = set_unknown_regs n1 (Proc.destroyed_at_oper i.desc) in
          begin match find_equation op_class n1 (op, varg) with
          | Some vres ->
              (* This operation was computed earlier. *)
              (* Are there registers that hold the results computed earlier? *)
              begin match find_regs_containing n1 vres with
              | Some res when (not (self#is_cheap_operation op))
                           && (not (Proc.regs_are_volatile res)) ->
                  (* We can replace res <- op args with r <- move res,
                     provided res are stable (non-volatile) registers.
                     If the operation is very cheap to compute, e.g.
                     an integer constant, don't bother. *)
                  let n3 = set_known_regs n1 i.res vres in
                  (* This is n1 above and not n2 because the move
                     does not destroy any regs *)
                  insert_move res i.res (self#cse n3 i.next)
              | _ ->
                  (* We already computed the operation but lost its
                     results.  Associate the result registers to
                     the result valnums of the previous operation. *)
                  let n3 = set_known_regs n2 i.res vres in
                  {i with next = self#cse n3 i.next}
              end
          | None ->
              (* This operation produces a result we haven't seen earlier. *)
              let n3 = set_fresh_regs n2 i.res (op, varg) op_class in
              {i with next = self#cse n3 i.next}
          end
      | Op_store false | Op_other ->
          (* An initializing store or an "other" operation do not invalidate
             any equations, but we do not know anything about the results. *)
         let n1 = set_unknown_regs n (Proc.destroyed_at_oper i.desc) in
         let n2 = set_unknown_regs n1 i.res in
         {i with next = self#cse n2 i.next}
      | Op_store true ->
          (* A non-initializing store can invalidate
             anything we know about prior loads. *)
         let n1 = set_unknown_regs n (Proc.destroyed_at_oper i.desc) in
         let n2 = set_unknown_regs n1 i.res in
         let n3 = self#kill_loads n2 in
         {i with next = self#cse n3 i.next}
      end
  (* For control structures, we set the numbering to empty at every
     join point, but propagate the current numbering across fork points. *)
  | Iifthenelse(test, ifso, ifnot) ->
     let n1 = set_unknown_regs n (Proc.destroyed_at_oper i.desc) in
      {i with desc = Iifthenelse(test, self#cse n1 ifso, self#cse n1 ifnot);
              next = self#cse empty_numbering i.next}
  | Iswitch(index, cases) ->
     let n1 = set_unknown_regs n (Proc.destroyed_at_oper i.desc) in
      {i with desc = Iswitch(index, Array.map (self#cse n1) cases);
              next = self#cse empty_numbering i.next}
  | Iloop(body) ->
      {i with desc = Iloop(self#cse empty_numbering body);
              next = self#cse empty_numbering i.next}
  | Icatch(rec_flag, handlers, body) ->
      let aux (nfail, handler) =
        nfail, self#cse empty_numbering handler
      in
      {i with desc = Icatch(rec_flag, List.map aux handlers, self#cse n body);
              next = self#cse empty_numbering i.next}
  | Itrywith(body, handler) ->
      {i with desc = Itrywith(self#cse n body,
                              self#cse empty_numbering handler);
              next = self#cse empty_numbering i.next}

method fundecl f =
  {f with fun_body = self#cse empty_numbering f.fun_body}

end
