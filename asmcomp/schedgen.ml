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

(* Instruction scheduling *)

open Reg
open Mach
open Linearize

(* Representation of the code DAG. *)

type code_dag_node =
  { instr: instruction;                 (* The instruction *)
    delay: int;           (* How many cycles before result is available *)
    mutable sons: (code_dag_node * int) list;
                                        (* Instructions that depend on it *)
    mutable date: int;                  (* Start date *)
    mutable length: int;                (* Length of longest path to result *)
    mutable ancestors: int;             (* Number of ancestors *)
    mutable emitted_ancestors: int }    (* Number of emitted ancestors *)

let dummy_node =
  { instr = end_instr; delay = 0; sons = []; date = 0;
    length = -1; ancestors = 0; emitted_ancestors = 0 }

(* The code dag itself is represented by two tables from registers to nodes:
   - "results" maps registers to the instructions that produced them;
   - "uses" maps registers to the instructions that use them.
   In addition:
   - code_stores contains the latest store nodes emitted so far
   - code_loads contains all load nodes emitted since the last store
   - code_checkbounds contains the latest checkbound node not matched
     by a subsequent load or store. *)

let code_results = (Hashtbl.create 31 : (location, code_dag_node) Hashtbl.t)
let code_uses = (Hashtbl.create 31 : (location, code_dag_node) Hashtbl.t)
let code_stores = ref ([] : code_dag_node list)
let code_loads = ref ([] : code_dag_node list)
let code_checkbounds = ref ([] : code_dag_node list)

let clear_code_dag () =
  Hashtbl.clear code_results;
  Hashtbl.clear code_uses;
  code_stores := [];
  code_loads := [];
  code_checkbounds := []

(* Add an edge to the code DAG *)

let add_edge ancestor son delay =
  ancestor.sons <- (son, delay) :: ancestor.sons;
  son.ancestors <- son.ancestors + 1

let add_edge_after son ancestor = add_edge ancestor son 0

(* Add edges from all instructions that define a pseudoregister [arg] being used
   as argument to node [node] (RAW dependencies *)

let add_RAW_dependencies node arg =
  try
    let ancestor = Hashtbl.find code_results arg.loc in
    add_edge ancestor node ancestor.delay
  with Not_found ->
    ()

(* Add edges from all instructions that use a pseudoregister [res] that is
   defined by node [node] (WAR dependencies). *)

let add_WAR_dependencies node res =
  let ancestors = Hashtbl.find_all code_uses res.loc in
  List.iter (add_edge_after node) ancestors

(* Add edges from all instructions that have already defined a pseudoregister
   [res] that is defined by node [node] (WAW dependencies). *)

let add_WAW_dependencies node res =
  try
    let ancestor = Hashtbl.find code_results res.loc in
    add_edge ancestor node 0
  with Not_found ->
    ()

(* Compute length of longest path to a result.
   For leafs of the DAG, see whether their result is used in the instruction
   immediately following the basic block (a "critical" output). *)

let is_critical critical_outputs results =
  try
    for i = 0 to Array.length results - 1 do
      let r = results.(i).loc in
      for j = 0 to Array.length critical_outputs - 1 do
        if critical_outputs.(j).loc = r then raise Exit
      done
    done;
    false
  with Exit ->
    true

let rec longest_path critical_outputs node =
  if node.length < 0 then begin
    match node.sons with
      [] ->
        node.length <-
          if is_critical critical_outputs node.instr.res
          || node.instr.desc = Lreloadretaddr (* alway critical *)
          then node.delay
          else 0
    | sons ->
        node.length <-
          List.fold_left
            (fun len (son, delay) ->
              max len (longest_path critical_outputs son + delay))
            0 sons
  end;
  node.length

(* Remove an instruction from the ready queue *)

let rec remove_instr node = function
    [] -> []
  | instr :: rem ->
      if instr == node then rem else instr :: remove_instr node rem

(* We treat Lreloadretaddr as a word-sized load *)

let some_load = (Iload(Cmm.Word, Arch.identity_addressing))

(* The generic scheduler *)

class virtual scheduler_generic = object (self)

val mutable trywith_nesting = 0

(* Determine whether an operation ends a basic block or not.
   Can be overridden for some processors to signal specific instructions
   that terminate a basic block. *)

method oper_in_basic_block = function
    Icall_ind -> false
  | Icall_imm _ -> false
  | Itailcall_ind -> false
  | Itailcall_imm _ -> false
  | Iextcall _ -> false
  | Istackoffset _ -> false
  | Ialloc _ -> false
  | _ -> true

(* Determine whether an instruction ends a basic block or not *)

(* PR#2719: it is generally incorrect to schedule checkbound instructions
   within a try ... with Invalid_argument _ -> ...
   Hence, a checkbound instruction within a try...with block ends the
   current basic block. *)

method private instr_in_basic_block instr try_nesting =
  match instr.desc with
    Lop op ->
      self#oper_in_basic_block op &&
      not (try_nesting > 0 && self#is_checkbound op)
  | Lreloadretaddr -> true
  | _ -> false

(* Determine whether an operation is a memory store or a memory load.
   Can be overridden for some processors to signal specific
   load or store instructions (e.g. on the I386). *)

method is_store = function
    Istore(_, _, _) -> true
  | _ -> false

method is_load = function
    Iload(_, _) -> true
  | _ -> false

method is_checkbound = function
    Iintop Icheckbound -> true
  | Iintop_imm(Icheckbound, _) -> true
  | _ -> false

method private instr_is_store instr =
  match instr.desc with
    Lop op -> self#is_store op
  | _ -> false

method private instr_is_load instr =
  match instr.desc with
    Lop op -> self#is_load op
  | _ -> false

method private instr_is_checkbound instr =
  match instr.desc with
    Lop op -> self#is_checkbound op
  | _ -> false

(* Estimate the latency of an operation. *)

method virtual oper_latency : Mach.operation -> int

(* Estimate the latency of a Lreloadretaddr operation. *)

method reload_retaddr_latency = self#oper_latency some_load

(* Estimate the delay needed to evaluate an instruction *)

method private instr_latency instr =
  match instr.desc with
    Lop op -> self#oper_latency op
  | Lreloadretaddr -> self#reload_retaddr_latency
  | _ -> assert false

(* Estimate the number of cycles consumed by emitting an operation. *)

method virtual oper_issue_cycles : Mach.operation -> int

(* Estimate the number of cycles consumed by emitting a Lreloadretaddr. *)

method reload_retaddr_issue_cycles = self#oper_issue_cycles some_load

(* Estimate the number of cycles consumed by emitting an instruction. *)

method private instr_issue_cycles instr =
  match instr.desc with
    Lop op -> self#oper_issue_cycles op
  | Lreloadretaddr -> self#reload_retaddr_issue_cycles
  | _ -> assert false

(* Pseudoregisters destroyed by an instruction *)

method private destroyed_by_instr instr =
  match instr.desc with
  | Lop op -> Proc.destroyed_at_oper (Iop op)
  | Lreloadretaddr -> [||]
  | _ -> assert false

(* Add an instruction to the code dag *)

method private add_instruction ready_queue instr =
  let delay = self#instr_latency instr in
  let destroyed = self#destroyed_by_instr instr in
  let node =
    { instr = instr;
      delay = delay;
      sons = [];
      date = 0;
      length = -1;
      ancestors = 0;
      emitted_ancestors = 0 } in
  (* Add edges from all instructions that define one of the registers used
     (RAW dependencies) *)
  Array.iter (add_RAW_dependencies node) instr.arg;
  (* Also add edges from all instructions that use one of the result regs
     of this instruction, or a reg destroyed by this instruction
     (WAR dependencies). *)
  Array.iter (add_WAR_dependencies node) instr.res;
  Array.iter (add_WAR_dependencies node) destroyed;   (* PR#5731 *)
  (* Also add edges from all instructions that have already defined one
     of the results of this instruction, or a reg destroyed by
     this instruction (WAW dependencies). *)
  Array.iter (add_WAW_dependencies node) instr.res;
  Array.iter (add_WAW_dependencies node) destroyed;   (* PR#5731 *)
  (* If this is a load, add edges from the most recent store viewed so
     far (if any) and remember the load.  Also add edges from the most
     recent checkbound and forget that checkbound. *)
  if self#instr_is_load instr then begin
    List.iter (add_edge_after node) !code_stores;
    code_loads := node :: !code_loads;
    List.iter (add_edge_after node) !code_checkbounds;
    code_checkbounds := []
  end
  (* If this is a store, add edges from the most recent store,
     as well as all loads viewed since then, and also the most recent
     checkbound. Remember the store,
     discarding the previous stores, loads and checkbounds. *)
  else if self#instr_is_store instr then begin
    List.iter (add_edge_after node) !code_stores;
    List.iter (add_edge_after node) !code_loads;
    List.iter (add_edge_after node) !code_checkbounds;
    code_stores := [node];
    code_loads := [];
    code_checkbounds := []
  end
  else if self#instr_is_checkbound instr then begin
    code_checkbounds := [node]
  end;
  (* Remember the registers used and produced by this instruction *)
  for i = 0 to Array.length instr.res - 1 do
    Hashtbl.add code_results instr.res.(i).loc node
  done;
  for i = 0 to Array.length destroyed - 1 do
    Hashtbl.add code_results destroyed.(i).loc node  (* PR#5731 *)
  done;
  for i = 0 to Array.length instr.arg - 1 do
    Hashtbl.add code_uses instr.arg.(i).loc node
  done;
  (* If this is a root instruction (all arguments already computed),
     add it to the ready queue *)
  if node.ancestors = 0 then node :: ready_queue else ready_queue

(* Given a list of instructions and a date, choose one or several
   that are ready to be computed (start date <= current date)
   and that we can emit in one cycle.  Favor instructions with
   maximal distance to result.  If we can't find any, return None.
   This does not take multiple issues into account, though. *)

method private ready_instruction date queue =
  let rec extract best = function
    [] ->
      if best == dummy_node then None else Some best
  | instr :: rem ->
      let new_best =
        if instr.date <= date && instr.length > best.length
        then instr else best in
      extract new_best rem in
  extract dummy_node queue

(* Schedule a basic block, adding its instructions in front of the given
   instruction sequence *)

method private reschedule ready_queue date cont =
  if ready_queue = [] then cont else begin
    match self#ready_instruction date ready_queue with
      None ->
        self#reschedule ready_queue (date + 1) cont
    | Some node ->
        (* Remove node from queue *)
        let new_queue = ref (remove_instr node ready_queue) in
        (* Update the start date and number of ancestors emitted of
           all descendents of this node. Enter those that become ready
           in the queue. *)
        let issue_cycles = self#instr_issue_cycles node.instr in
        List.iter
          (fun (son, delay) ->
            let completion_date = date + issue_cycles + delay - 1 in
            if son.date < completion_date then son.date <- completion_date;
            son.emitted_ancestors <- son.emitted_ancestors + 1;
            if son.emitted_ancestors = son.ancestors then
              new_queue := son :: !new_queue)
          node.sons;
        { node.instr with next =
            self#reschedule !new_queue (date + issue_cycles) cont }
  end

(* Entry point *)
(* Don't bother to schedule for initialization code and the like. *)

method schedule_fundecl f =

  let rec schedule i try_nesting =
    match i.desc with
    | Lend -> i
    | Lpushtrap -> { i with next = schedule i.next (try_nesting + 1) }
    | Lpoptrap -> { i with next = schedule i.next (try_nesting - 1) }
    | _ ->
        if self#instr_in_basic_block i try_nesting then begin
          clear_code_dag();
          schedule_block [] i try_nesting
        end else
          { i with next = schedule i.next try_nesting }

  and schedule_block ready_queue i try_nesting =
    if self#instr_in_basic_block i try_nesting then
      schedule_block (self#add_instruction ready_queue i) i.next try_nesting
    else begin
      let critical_outputs =
        match i.desc with
          Lop(Icall_ind | Itailcall_ind) -> [| i.arg.(0) |]
        | Lop(Icall_imm _ | Itailcall_imm _ | Iextcall _) -> [||]
        | Lreturn -> [||]
        | _ -> i.arg in
      List.iter (fun x -> ignore (longest_path critical_outputs x)) ready_queue;
      self#reschedule ready_queue 0 (schedule i try_nesting)
    end in

  if f.fun_fast then begin
    let new_body = schedule f.fun_body 0 in
    clear_code_dag();
    { fun_name = f.fun_name;
      fun_body = new_body;
      fun_fast = f.fun_fast;
      fun_dbg  = f.fun_dbg }
  end else
    f

end

let reset () = clear_code_dag ()
