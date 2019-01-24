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

(* Compaction of an automata *)

open Lexgen

(* Code for memory actions  *)
let code = Table.create 0

(* instructions are 2 8-bits integers, a 0xff byte means return *)

let emit_int i = Table.emit code i

let ins_mem i c =  match i with
  | Copy (dst, src) -> dst::src::c
  | Set dst         -> dst::0xff::c


let ins_tag i c = match i with
  | SetTag (dst, src) -> dst::src::c
  | EraseTag dst      -> dst::0xff::c


let do_emit_code c =
  let r = Table.size code in
  List.iter emit_int c ;
  emit_int 0xff ;
  r

let memory = Hashtbl.create 101

let mem_emit_code c =
  try Hashtbl.find memory c with
  | Not_found ->
      let r = do_emit_code c in
      Hashtbl.add memory c r ;
      r

(* Code address 0 is the empty code (ie do nothing) *)
let _ = mem_emit_code []

let emit_tag_code c = mem_emit_code (List.fold_right ins_tag c [])
and emit_mem_code c  =mem_emit_code (List.fold_right ins_mem c [])

(*******************************************)
(* Compact the transition and check arrays *)
(*******************************************)


(* Determine the integer occurring most frequently in an array *)

let most_frequent_elt v =
  let frequencies = Hashtbl.create 17 in
  let max_freq = ref 0 in
  let most_freq = ref (v.(0)) in
  for i = 0 to Array.length v - 1 do
    let e = v.(i) in
    let r =
      try
        Hashtbl.find frequencies e
      with Not_found ->
        let r = ref 1 in Hashtbl.add frequencies e r; r in
    incr r;
    if !r > !max_freq then begin max_freq := !r; most_freq := e end
  done;
  !most_freq

(* Transform an array into a list of (position, non-default element) *)

let non_default_elements def v =
  let rec nondef i =
    if i >= Array.length v then [] else begin
      let e = v.(i) in
      if e = def then nondef(i+1) else (i, e) :: nondef(i+1)
    end in
  nondef 0


type t_compact =
 {mutable c_trans : int array ;
  mutable c_check : int array ;
  mutable c_last_used : int ; }

let create_compact () =
  { c_trans = Array.make 1024 0 ;
    c_check = Array.make 1024 (-1) ;
    c_last_used = 0 ; }

let reset_compact c =
  c.c_trans <- Array.make 1024 0 ;
  c.c_check <- Array.make 1024 (-1) ;
  c.c_last_used <- 0

(* One compacted table for transitions, one other for memory actions *)
let trans = create_compact ()
and moves = create_compact ()


let grow_compact c =
  let old_trans = c.c_trans
  and old_check = c.c_check in
  let n = Array.length old_trans in
  c.c_trans <- Array.make (2*n) 0;
  Array.blit old_trans 0 c.c_trans 0 c.c_last_used;
  c.c_check <- Array.make (2*n) (-1);
  Array.blit old_check 0 c.c_check 0 c.c_last_used

let do_pack state_num orig compact =
  let default = most_frequent_elt orig in
  let nondef = non_default_elements default orig in
  let rec pack_from b =
    while
      b + 257 > Array.length compact.c_trans
    do
      grow_compact compact
    done;
    let rec try_pack = function
      [] -> b
    | (pos, _v) :: rem ->
        if compact.c_check.(b + pos) = -1 then
          try_pack rem
        else pack_from (b+1) in
    try_pack nondef in
  let base = pack_from 0 in
  List.iter
    (fun (pos, v) ->
      compact.c_trans.(base + pos) <- v;
      compact.c_check.(base + pos) <- state_num)
    nondef;
  if base + 257 > compact.c_last_used then
    compact.c_last_used <- base + 257;
  (base, default)

let pack_moves state_num move_t =
  let move_v = Array.make 257 0
  and move_m = Array.make 257 0 in
  for i = 0 to 256 do
    let act,c = move_t.(i) in
    move_v.(i) <- (match act with Backtrack -> -1 | Goto n -> n) ;
    move_m.(i) <- emit_mem_code c
  done ;
  let pk_trans = do_pack state_num move_v trans
  and pk_moves = do_pack state_num move_m moves in
  pk_trans, pk_moves


(* Build the tables *)

type lex_tables =
  { tbl_base: int array;                 (* Perform / Shift *)
    tbl_backtrk: int array;              (* No_remember / Remember *)
    tbl_default: int array;              (* Default transition *)
    tbl_trans: int array;                (* Transitions (compacted) *)
    tbl_check: int array;                (* Check (compacted) *)
(* code addresses are managed in a similar fashion as transitions *)
    tbl_base_code : int array;           (* code ptr / base for Shift *)
    tbl_backtrk_code : int array;        (* nothing / code when Remember *)
(* moves to execute before transitions (compacted) *)
    tbl_default_code : int array;
    tbl_trans_code : int array;
    tbl_check_code : int array;
(* byte code itself *)
    tbl_code: int array;}


let compact_tables state_v =
  let n = Array.length state_v in
  let base = Array.make n 0
  and backtrk = Array.make n (-1)
  and default = Array.make n 0
  and base_code = Array.make n 0
  and backtrk_code = Array.make n 0
  and default_code = Array.make n 0 in
  for i = 0 to n - 1 do
    match state_v.(i) with
    | Perform (n,c) ->
        base.(i) <- -(n+1) ;
        base_code.(i) <- emit_tag_code c
    | Shift(trans, move) ->
        begin match trans with
        | No_remember -> ()
        | Remember (n,c) ->
            backtrk.(i) <- n ;
            backtrk_code.(i) <- emit_tag_code c
        end;
        let (b_trans, d_trans),(b_moves,d_moves) = pack_moves i move in
        base.(i) <- b_trans; default.(i) <- d_trans ;
        base_code.(i) <- b_moves; default_code.(i) <- d_moves ;
  done;
  let code = Table.trim code in
  let tables =
    if Array.length code > 1 then
      { tbl_base = base;
        tbl_backtrk = backtrk;
        tbl_default = default;
        tbl_trans = Array.sub trans.c_trans 0 trans.c_last_used;
        tbl_check = Array.sub trans.c_check 0 trans.c_last_used;
        tbl_base_code = base_code ;
        tbl_backtrk_code = backtrk_code;
        tbl_default_code = default_code;
        tbl_trans_code = Array.sub moves.c_trans 0 moves.c_last_used;
        tbl_check_code = Array.sub moves.c_check 0 moves.c_last_used;
        tbl_code = code}
    else (* when no memory moves, do not emit related tables *)
       { tbl_base = base;
        tbl_backtrk = backtrk;
        tbl_default = default;
        tbl_trans = Array.sub trans.c_trans 0 trans.c_last_used;
        tbl_check = Array.sub trans.c_check 0 trans.c_last_used;
        tbl_base_code = [||] ;
        tbl_backtrk_code = [||];
        tbl_default_code = [||];
        tbl_trans_code = [||];
        tbl_check_code = [||];
        tbl_code = [||]}
  in
  reset_compact trans ;
  reset_compact moves ;
  tables
