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

(* Compiling a lexer definition *)

open Syntax

(* Deep abstract syntax for regular expressions *)

type regexp =
    Empty
  | Chars of int
  | Action of int
  | Seq of regexp * regexp
  | Alt of regexp * regexp
  | Star of regexp

(* From shallow to deep syntax *)

(***

let print_char_class c =
  let print_interval low high =
    prerr_int low;
    if high - 1 > low then begin
      prerr_char '-';
      prerr_int (high-1)
    end;
    prerr_char ' ' in
  let rec print_class first next = function
    [] -> print_interval first next
  | c::l ->
      if char.code c = next
      then print_class first (next+1) l
      else begin
        print_interval first next;
        print_class (char.code c) (char.code c + 1) l
      end in
  match c with
    [] -> prerr_newline()
  | c::l -> print_class (char.code c) (char.code c + 1) l; prerr_newline()


let rec print_regexp = function
    Empty -> prerr_string "Empty"
  | Chars n -> prerr_string "Chars "; prerr_int n
  | Action n -> prerr_string "Action "; prerr_int n
  | Seq(r1,r2) -> print_regexp r1; prerr_string "; "; print_regexp r2
  | Alt(r1,r2) ->
      prerr_string "("; print_regexp r1; prerr_string " | "; print_regexp r2;
      prerr_string ")"
  | Star r -> prerr_string "("; print_regexp r; prerr_string ")*"

***)

let chars = ref ([] : char list list)
let chars_count = ref 0
let actions = ref ([] : (int * location) list)
let actions_count = ref 0

let rec encode_regexp = function
    Epsilon -> Empty
  | Characters cl ->
      let n = !chars_count in
(***      prerr_int n; prerr_char ' '; print_char_class cl; ***)
      chars := cl :: !chars;
      chars_count := !chars_count + 1;
      Chars(n)
  | Sequence(r1,r2) ->
      Seq(encode_regexp r1, encode_regexp r2)
  | Alternative(r1,r2) ->
      Alt(encode_regexp r1, encode_regexp r2)
  | Repetition r ->
      Star (encode_regexp r)


let encode_casedef =
  List.fold_left
   (fun reg (expr,act) ->
     let act_num = !actions_count in
     actions_count := !actions_count + 1;
     actions := (act_num, act) :: !actions;
     Alt(reg, Seq(encode_regexp expr, Action act_num)))
  Empty


let encode_lexdef (Lexdef(_, ld)) =
  chars := [];
  chars_count := 0;
  actions := [];
  actions_count := 0;
  let name_regexp_list =
    List.map (fun (name, casedef) -> (name, encode_casedef casedef)) ld in
(*  List.iter print_char_class chars; *)
  let chr = Array.of_list (List.rev !chars)
  and act = !actions in
  chars := [];
  actions := [];
  (chr, name_regexp_list, act)


(* To generate directly a NFA from a regular expression.
   Confer Aho-Sethi-Ullman, dragon book, chap. 3 *)

type transition =
    OnChars of int
  | ToAction of int


let rec merge_trans l1 l2 =
  match (l1, l2) with
    ([], s2) -> s2
  | (s1, []) -> s1
  | ((OnChars n1 as t1) :: r1 as s1), ((OnChars n2 as t2) :: r2 as s2) ->
      if n1 = n2 then t1 :: merge_trans r1 r2 else
      if n1 < n2 then t1 :: merge_trans r1 s2 else
                      t2 :: merge_trans s1 r2
  | ((ToAction n1 as t1) :: r1 as s1), ((ToAction n2 as t2) :: r2 as s2) ->
      if n1 = n2 then t1 :: merge_trans r1 r2 else
      if n1 < n2 then t1 :: merge_trans r1 s2 else
                      t2 :: merge_trans s1 r2
  | ((OnChars n1 as t1) :: r1), ((ToAction n2) :: r2 as s2) ->
      t1 :: merge_trans r1 s2
  | ((ToAction n1) :: r1 as s1), ((OnChars n2 as t2) :: r2) ->
      t2 :: merge_trans s1 r2


let rec nullable = function
    Empty      -> true
  | Chars _    -> false
  | Action _   -> false
  | Seq(r1,r2) -> nullable r1 && nullable r2
  | Alt(r1,r2) -> nullable r1 || nullable r2
  | Star r     -> true


let rec firstpos = function
    Empty      -> []
  | Chars pos  -> [OnChars pos]
  | Action act -> [ToAction act]
  | Seq(r1,r2) -> if nullable r1
                  then merge_trans (firstpos r1) (firstpos r2)
                  else firstpos r1
  | Alt(r1,r2) -> merge_trans (firstpos r1) (firstpos r2)
  | Star r     -> firstpos r


let rec lastpos = function
    Empty      -> []
  | Chars pos  -> [OnChars pos]
  | Action act -> [ToAction act]
  | Seq(r1,r2) -> if nullable r2
                  then merge_trans (lastpos r1) (lastpos r2)
                  else lastpos r2
  | Alt(r1,r2) -> merge_trans (lastpos r1) (lastpos r2)
  | Star r     -> lastpos r


let followpos size name_regexp_list =
  let v = Array.make size [] in
    let fill_pos first = function
        OnChars pos -> v.(pos) <- merge_trans first v.(pos); ()
      | ToAction _  -> () in
    let rec fill = function
        Seq(r1,r2) ->
          fill r1; fill r2;
          List.iter (fill_pos (firstpos r2)) (lastpos r1)
      | Alt(r1,r2) ->
          fill r1; fill r2
      | Star r ->
          fill r;
          List.iter (fill_pos (firstpos r)) (lastpos r)
      | _ -> () in
    List.iter (fun (name, regexp) -> fill regexp) name_regexp_list;
    v


let no_action = 0x3FFFFFFF

let split_trans_set =
  List.fold_left
    (fun (act, pos_set as act_pos_set) trans ->
       match trans with
         OnChars pos   -> (act, pos :: pos_set)
       | ToAction act1 -> if act1 < act then (act1, pos_set)
                                             else act_pos_set)
    (no_action, [])


let memory = (Hashtbl.create 131 : (transition list, int) Hashtbl.t)
let todo = ref ([] : (transition list * int) list)
let next = ref 0

let get_state st =
  try
    Hashtbl.find memory st
  with Not_found ->
    let nbr = !next in
    next := !next + 1;
    Hashtbl.add memory st nbr;
    todo := (st, nbr) :: !todo;
    nbr

let rec map_on_states f =
  match !todo with
    []  -> []
  | (st,i)::r -> todo := r; let res = f st in (res,i) :: map_on_states f

let number_of_states () = !next

let goto_state = function
    [] -> Backtrack
  | ps -> Goto (get_state ps)


let transition_from chars follow pos_set =
  let tr = Array.make 256 []
  and shift = Array.make 256 Backtrack in
    List.iter
      (fun pos ->
        List.iter
          (fun c ->
             tr.(Char.code c) <-
               merge_trans tr.(Char.code c) follow.(pos))
          chars.(pos))
      pos_set;
    for i = 0 to 255 do
      shift.(i) <- goto_state tr.(i)
    done;
    shift


let translate_state chars follow state =
  match split_trans_set state with
    n, [] -> Perform n
  | n, ps -> Shift( (if n = no_action then No_remember else Remember n),
                    transition_from chars follow ps)


let make_dfa lexdef =
  let (chars, name_regexp_list, actions) =
    encode_lexdef lexdef in
(**
  List.iter (fun (name, regexp) ->
               prerr_string name; prerr_string " = "; print_regexp regexp;
               prerr_newline())
            name_regexp_list;
**)
  let follow =
    followpos (Array.length chars) name_regexp_list in
  let initial_states =
    List.map (fun (name, regexp) -> (name, get_state(firstpos regexp)))
             name_regexp_list in
  let states =
    map_on_states (translate_state chars follow) in
  let v =
    Array.make (number_of_states()) (Perform 0) in
  List.iter (fun (auto, i) -> v.(i) <- auto) states;
  (initial_states, v, actions)
