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

(* Translation of string matching from closed lambda to C-- *)

open Lambda
open Cmm

module type I = sig
  val string_block_length : Cmm.expression -> Cmm.expression
  val transl_switch :
      Cmm.expression -> int -> int ->
        (int * Cmm.expression) list -> Cmm.expression ->
          Cmm.expression
end

module Make(I:I) = struct

(* Debug *)

  let dbg = false

  let mask =
    let open Nativeint in
    sub (shift_left one 8) one

  let pat_as_string p =
    let rec digits k n p =
      if n <= 0 then k
      else
        let d = Nativeint.to_int (Nativeint.logand mask p) in
        let d = Char.escaped (Char.chr d) in
        digits (d::k) (n-1) (Nativeint.shift_right_logical p  8) in
    let ds = digits [] Arch.size_addr p in
    let ds =
      if Arch.big_endian then ds else List.rev ds in
    String.concat "" ds

  let do_pp_cases chan cases =
    List.iter
      (fun (ps,_) ->
        Printf.fprintf chan "  [%s]\n"
          (String.concat "; " (List.map pat_as_string ps)))
      cases

  let pp_cases chan tag cases =
    Printf.eprintf "%s:\n" tag ;
    do_pp_cases chan cases

  let pp_match chan tag idxs cases =
    Printf.eprintf
      "%s: idx=[%s]\n" tag
      (String.concat "; " (List.map string_of_int idxs)) ;
    do_pp_cases chan cases

(* Utilities *)

  let gen_cell_id () = Ident.create "cell"
  let gen_size_id () = Ident.create "size"

  let mk_let_cell id str ind body =
    let cell =
      Cop(Cload Word,[Cop(Cadda,[str;Cconst_int(Arch.size_int*ind)])]) in
    Clet(id, cell, body)

  let mk_let_size id str body =
    let size = I.string_block_length str in
    Clet(id, size, body)

  let mk_cmp_gen cmp_op id nat ifso ifnot =
    let test = Cop (Ccmpi cmp_op, [ Cvar id; Cconst_natpointer nat ]) in
    Cifthenelse (test, ifso, ifnot)

  let mk_lt = mk_cmp_gen Clt
  let mk_eq = mk_cmp_gen Ceq

  module IntArg =
    struct
      type t = int
      let compare (x:int) (y:int) =
        if x < y then -1
        else if x > y then 1
        else 0
    end

  let interval m0 n =
    let rec do_rec m =
      if m >= n then []
      else m::do_rec (m+1) in
    do_rec m0


(*****************************************************)
(* Compile strings to a lists of words [native ints] *)
(*****************************************************)

  let pat_of_string str =
    let len = String.length str in
    let n = len / Arch.size_addr + 1 in
    let get_byte i =
      if i < len then int_of_char str.[i]
      else if i < n * Arch.size_addr - 1 then 0
      else n * Arch.size_addr - 1 - len in
    let mk_word ind =
      let w = ref 0n in
      let imin = ind * Arch.size_addr
      and imax = (ind + 1) * Arch.size_addr - 1 in
      if Arch.big_endian then
        for i = imin to imax do
          w := Nativeint.logor (Nativeint.shift_left !w 8)
              (Nativeint.of_int (get_byte i));
        done
      else
        for i = imax downto imin do
          w := Nativeint.logor (Nativeint.shift_left !w 8)
              (Nativeint.of_int (get_byte i));
        done;
      !w in
    let rec mk_words ind  =
      if ind >= n then []
      else mk_word ind::mk_words (ind+1) in
    mk_words 0

(*****************************)
(* Discriminating heuristics *)
(*****************************)

  module IntSet = Set.Make(IntArg)
  module NativeSet = Set.Make(Nativeint)

  let rec add_one sets ps = match sets,ps with
  | [],[] -> []
  | set::sets,p::ps ->
      let sets = add_one sets ps in
      NativeSet.add p set::sets
  | _,_ -> assert false

  let count_arities cases = match cases with
  | [] -> assert false
  | (ps,_)::_ ->
      let sets =
        List.fold_left
          (fun sets (ps,_) -> add_one sets ps)
          (List.map (fun _ -> NativeSet.empty) ps) cases in
      List.map NativeSet.cardinal sets

  let count_arities_first cases =
    let set =
      List.fold_left
        (fun set case -> match case with
        | (p::_,_) -> NativeSet.add p set
        | _ -> assert false)
        NativeSet.empty cases in
    NativeSet.cardinal set

  let count_arities_length cases =
    let set =
      List.fold_left
        (fun set (ps,_) -> IntSet.add (List.length ps) set)
        IntSet.empty cases in
    IntSet.cardinal set

  let best_col =
    let rec do_rec kbest best k = function
      | [] -> kbest
      | x::xs ->
          if x < best then
            do_rec k x (k+1) xs
          else
            do_rec kbest best (k+1) xs in
    let smallest = do_rec (-1) max_int 0 in
    fun cases ->
      let ars = count_arities cases in
      smallest ars

  let swap_list =
    let rec do_rec k xs = match xs with
    | [] -> assert false
    | x::xs ->
        if k <= 0 then [],x,xs
        else
          let xs,mid,ys = do_rec (k-1) xs in
          x::xs,mid,ys in
    fun k xs ->
      let xs,x,ys = do_rec  k xs in
      x::xs @ ys

  let swap k idxs cases =
    if k = 0 then idxs,cases
    else
      let idxs = swap_list k idxs
      and cases =
        List.map
          (fun (ps,act) -> swap_list k ps,act)
          cases in
      if dbg then begin
        pp_match stderr "SWAP" idxs cases
      end ;
      idxs,cases

  let best_first idxs cases = match idxs with
  | []|[_] -> idxs,cases (* optimisation: one column only *)
  | _ ->
      let k = best_col cases in
      swap k idxs cases

(************************************)
(* Divide according to first column *)
(************************************)

  module Divide(O:Set.OrderedType) = struct

    module OMap = Map.Make(O)

    let do_find key env =
      try OMap.find key env
      with Not_found -> assert false

    let divide cases =
      let env =
        List.fold_left
          (fun env (p,psact) ->
            let old =
              try OMap.find p env
              with Not_found -> [] in
            OMap.add p ((psact)::old) env)
          OMap.empty cases in
      let r =  OMap.fold (fun key v k -> (key,v)::k) env [] in
      List.rev r (* Now sorted *)
  end

(***************)
(* Compilation *)
(***************)

(* Group by cell *)

    module DivideNative = Divide(Nativeint)

    let by_cell cases =
      DivideNative.divide
        (List.map
           (fun case -> match case with
           | (p::ps),act -> p,(ps,act)
           | [],_ -> assert false)
           cases)

(* Split into two halves *)

    let rec do_split idx env = match env with
    | [] -> assert false
    | (midkey,_ as x)::rem ->
        if idx <= 0 then [],midkey,env
        else
          let lt,midkey,ge = do_split (idx-1) rem in
          x::lt,midkey,ge

    let split_env len env = do_split (len/2) env

(* Switch according to one cell *)

(*
  Emit the switch, here as a comparison tree.
  Argument compile_rec is to be called to compile the rest of patterns,
  as match_on_cell can be called in two different contexts :
  from do_compile_pats and top_compile below.
 *)
    let match_oncell compile_rec str default idx env =
      let id = gen_cell_id () in
      let rec comp_rec env =
        let len = List.length env in
        if len <= 3 then
          List.fold_right
            (fun (key,cases) ifnot ->
              mk_eq id key
                (compile_rec str default cases)
              ifnot)
            env default
        else
          let lt,midkey,ge = split_env len env in
          mk_lt id midkey (comp_rec lt) (comp_rec ge) in
      mk_let_cell id str idx (comp_rec env)

(*
  Recursive 'list of cells' compile function:
  - choose the matched cell and switch on it
  - notice: patterns (and idx) all have the same length
 *)

    let rec do_compile_pats idxs str default cases =
      if dbg then begin
        pp_match stderr "COMPILE" idxs cases
      end ;
      match idxs with
      | [] ->
          begin match cases with
          | [] -> default
          | (_,e)::_ -> e
          end
      | _::_ ->
          let idxs,cases = best_first idxs cases in
          begin match idxs with
          | [] -> assert false
          | idx::idxs ->
              match_oncell
                (do_compile_pats idxs) str default idx (by_cell cases)
          end


(* Group by size *)

    module DivideInt = Divide(IntArg)


    let by_size cases =
      DivideInt.divide
        (List.map
           (fun (ps,_ as case) -> List.length ps,case)
           cases)
(*
  Switch according to pattern size
  Argument from_ind is the starting index, it can be zero
  or one (when the swicth on the cell 0 has already been performed.
  In that latter case pattern len is string length-1 and is corrected.
 *)

    let compile_by_size from_ind str default cases =
      let size_cases =
        List.map
          (fun (len,cases) ->
            let len = len+from_ind in
            let act =
              do_compile_pats
                (interval from_ind len)
                str default  cases in
            (len,act))
          (by_size cases) in
      let id = gen_size_id () in
      let switch = I.transl_switch (Cvar id) 1 max_int size_cases default in
      mk_let_size id str switch

(*
  Compilation entry point: we choose to switch
  either on size or on first cell, using the
  'least discriminant' heuristics.
 *)
    let top_compile str default cases =
      let a_len = count_arities_length cases
      and a_fst = count_arities_first cases in
      if a_len <= a_fst then begin
        if dbg then pp_cases stderr "SIZE" cases ;
        compile_by_size 0 str default cases
      end else begin
        if dbg then pp_cases stderr "FIRST COL" cases ;
        let compile_size_rest str default cases =
          compile_by_size 1 str default cases in
        match_oncell compile_size_rest str default 0 (by_cell cases)
      end

(* Module entry point *)

    let catch arg k = match arg with
    | Cexit (e,[]) ->  k arg
    | _ ->
        let e =  next_raise_count () in
        Ccatch (e,[],k (Cexit (e,[])),arg)

    let compile str default cases =
(* We do not attempt to really optimise default=None *)
      let cases,default = match cases,default with
      | (_,e)::cases,None
      | cases,Some e -> cases,e
      | [],None -> assert false in
      let cases =
        List.rev_map
          (fun (s,act) -> pat_of_string s,act)
          cases in
      catch default (fun default -> top_compile str default cases)

  end
