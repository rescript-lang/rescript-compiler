[@@@bs.config{no_export}]
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


module Re_fmt
= struct
#1 "re_fmt.ml"
(** Very small tooling for format printers. *)

include Format

type 'a t = Format.formatter -> 'a -> unit

(* Only in the stdlib since 4.02, so we copy. *)
let rec list ?(pp_sep = pp_print_cut) pp ppf = function
  | [] -> ()
  | [v] -> pp ppf v
  | v :: vs ->
    pp ppf v;
    pp_sep ppf ();
    list ~pp_sep pp ppf vs

(* want this name to make sure we don't use pp_print_list from stdlib
   accidentally *)
let pp_print_list = list

let str = pp_print_string
let sexp fmt s pp x = fprintf fmt "@[<3>(%s@ %a)@]" s pp x
let pair pp1 pp2 fmt (v1,v2) =
  pp1 fmt v1; pp_print_space fmt () ; pp2 fmt v2
let triple pp1 pp2 pp3 fmt (v1, v2, v3) =
  pp1 fmt v1; pp_print_space fmt () ;
  pp2 fmt v2; pp_print_space fmt () ;
  pp3 fmt v3
let int = pp_print_int
let optint fmt = function
  | None -> ()
  | Some i -> fprintf fmt "@ %d" i

let quote fmt s = Format.fprintf fmt "\"%s\"" s

let pp_olist pp_elem fmt =
  Format.fprintf fmt "@[<3>[@ %a@ ]@]"
    (pp_print_list
       ~pp_sep:(fun fmt () -> fprintf fmt ";@ ")
       pp_elem)

let pp_str_list = pp_olist quote

let to_to_string pp x =
  let b = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer b in
  pp fmt x;
  Buffer.contents b

end
module Re_cset : sig 
#1 "re_cset.mli"
(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

(* Character sets, represented as sorted list of intervals *)

type c = int
type t

val iter : t -> f:(c -> c -> unit) -> unit

val union : t -> t -> t
val inter : t -> t -> t
val diff : t -> t -> t
val offset : int -> t -> t

val empty : t
val single : c -> t
val seq : c -> c -> t
val add : c -> t -> t

val mem : c -> t -> bool

type hash
val hash : t -> hash

val pp : Format.formatter -> t -> unit

val one_char : t -> c option

val fold_right : t -> init:'acc -> f:(c * c -> 'acc ->  'acc) -> 'acc

val hash_rec : t -> int

module CSetMap : Map.S with type key = int * t

val cany : t

val csingle : char -> t

val is_empty : t -> bool

val prepend : t -> 'a list -> (t * 'a list) list -> (t * 'a list) list

val pick : t -> c

end = struct
#1 "re_cset.ml"
(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

type c = int
type t = (c * c) list

let rec union l l' =
  match l, l' with
    _, [] -> l
  | [], _ -> l'
  | (c1, c2)::r, (c1', c2')::r' ->
    if c2 + 1 < c1' then
      (c1, c2)::union r l'
    else if c2' + 1 < c1 then
      (c1', c2')::union l r'
    else if c2 < c2' then
      union r ((min c1 c1', c2')::r')
    else
      union ((min c1 c1', c2)::r) r'

let rec inter l l' =
  match l, l' with
    _, [] -> []
  | [], _ -> []
  | (c1, c2)::r, (c1', c2')::r' ->
    if c2 < c1' then
      inter r l'
    else if c2' < c1 then
      inter l r'
    else if c2 < c2' then
      (max c1 c1', c2)::inter r l'
    else
      (max c1 c1', c2')::inter l r'

let rec diff l l' =
  match l, l' with
    _, [] -> l
  | [], _ -> []
  | (c1, c2)::r, (c1', c2')::r' ->
    if c2 < c1' then
      (c1, c2)::diff r l'
    else if c2' < c1 then
      diff l r'
    else
      let r'' = if c2' < c2 then (c2' + 1, c2) :: r else r in
      if c1 < c1' then
        (c1, c1' - 1)::diff r'' r'
      else
        diff r'' r'

let single c = [c, c]

let add c l = union (single c) l

let seq c c' = if c <= c' then [c, c'] else [c', c]

let rec offset o l =
  match l with
    []            -> []
  | (c1, c2) :: r -> (c1 + o, c2 + o) :: offset o r

let empty = []

let rec mem (c : int) s =
  match s with
    []              -> false
  | (c1, c2) :: rem -> if c <= c2 then c >= c1 else mem c rem

(****)

type hash = int

let rec hash_rec = function
  | []        -> 0
  | (i, j)::r -> i + 13 * j + 257 * hash_rec r
let hash l = (hash_rec l) land 0x3FFFFFFF

(****)

let print_one ch (c1, c2) =
  if c1 = c2 then
    Format.fprintf ch "%d" c1
  else
    Format.fprintf ch "%d-%d" c1 c2

let pp = Re_fmt.list print_one

let rec iter t ~f =
  match t with
  | [] -> ()
  | (x, y)::xs ->
    f x y;
    iter xs  ~f

let one_char = function
  | [i, j] when i = j -> Some i
  | _ -> None


module CSetMap = Map.Make (struct
    type t = int * (int * int) list
    let compare (i, u) (j, v) =
      let c = compare i j in
      if c <> 0
      then c
      else compare u v
  end)

let fold_right t ~init ~f = List.fold_right f t init

let csingle c = single (Char.code c)

let cany = [0, 255]

let is_empty = function
  | [] -> true
  | _ -> false

let rec prepend s x l =
  match s, l with
  | [], _ -> l
  | _r, [] -> []
  | (_c, c') :: r, ([d, _d'], _x') :: _r' when c' < d -> prepend r x l
  | (c, c') :: r, ([d, d'], x') :: r' ->
    if c <= d then begin
      if c' < d'
      then ([d, c'], x @ x') :: prepend r x (([c' + 1, d'], x') :: r')
      else ([d, d'], x @ x') :: prepend s x r'
    end else begin
      if c > d'
      then ([d, d'], x') :: prepend s x r'
      else ([d, c - 1], x') :: prepend s x (([c, d'], x') :: r')
    end
  | _ -> assert false

let pick = function
  | [] -> invalid_arg "Re_cset.pick"
  | (x, _)::_ -> x

end
module Re_automata : sig 
#1 "re_automata.mli"
(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

(* Regular expressions *)

(** Categories represent the various kinds of characters that can be tested
    by look-ahead and look-behind operations.

    This is more restricted than Cset, but faster.
*)
module Category : sig
  type t
  val (++) : t -> t -> t
  val from_char : char -> t

  val inexistant : t
  val letter : t
  val not_letter : t
  val newline : t
  val lastnewline : t
  val search_boundary : t
end

type mark = int

type sem = [ `Longest | `Shortest | `First ]
type rep_kind = [ `Greedy | `Non_greedy ]

val pp_sem : Format.formatter -> sem -> unit
val pp_rep_kind : Format.formatter -> rep_kind -> unit

module Pmark : sig
  type t = private int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val gen : unit -> t
  val pp : Format.formatter -> t -> unit
end

type expr
val is_eps : expr -> bool
val pp : Format.formatter -> expr -> unit

type ids
val create_ids : unit -> ids

val cst : ids -> Re_cset.t -> expr
val empty : ids -> expr
val alt : ids -> expr list -> expr
val seq : ids -> sem -> expr -> expr -> expr
val eps : ids -> expr
val rep : ids -> rep_kind -> sem -> expr -> expr
val mark : ids -> mark -> expr
val pmark : ids -> Pmark.t -> expr
val erase : ids -> mark -> mark -> expr
val before : ids -> Category.t -> expr
val after : ids -> Category.t -> expr

val rename : ids -> expr -> expr

(****)

module PmarkSet : Set.S with type elt = Pmark.t

(* States of the automata *)

type idx = int
module Marks : sig
  type t =
    { marks: (mark * idx) list
    ; pmarks: PmarkSet.t }
end

module E : sig
  type t
  val pp : Format.formatter -> t -> unit
end

type hash
type mark_infos = int array
type status = Failed | Match of mark_infos * PmarkSet.t | Running

module State : sig
  type t =
    { idx: idx
    ; category: Category.t
    ; desc: E.t list
    ; mutable status: status option
    ; hash: hash }
  val dummy : t
  val create : Category.t -> expr -> t
  module Table : Hashtbl.S with type key = t
end

(****)

(* Computation of the states following a given state *)

type working_area
val create_working_area : unit -> working_area
val index_count : working_area -> int

val delta : working_area -> Category.t -> Re_cset.c -> State.t -> State.t
val deriv :
  working_area -> Re_cset.t -> (Category.t * Re_cset.t) list -> State.t ->
  (Re_cset.t * State.t) list

(****)

val status : State.t -> status

end = struct
#1 "re_automata.ml"
(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

module Cset = Re_cset

type sem = [ `Longest | `Shortest | `First ]

type rep_kind = [ `Greedy | `Non_greedy ]


module Category : sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val to_int : t -> int

  val intersect : t -> t -> bool
  val (++) : t -> t -> t
  val from_char : char -> t

  val dummy : t
  val inexistant : t
  val letter : t
  val not_letter : t
  val newline : t
  val lastnewline : t
  val search_boundary : t
end = struct
  type t = int
  let equal (x : int) (y : int) = x = y
  let compare (x : int) (y : int) = compare x y
  let to_int x = x
  let pp = Format.pp_print_int

  let intersect x y = x land y <> 0
  let (++) x y = x lor y

  let dummy = -1
  let inexistant = 1
  let letter = 2
  let not_letter = 4
  let newline = 8
  let lastnewline = 16
  let search_boundary = 32

  let from_char = function
    (* Should match [cword] definition *)
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '\170' | '\181' | '\186'
    | '\192'..'\214' | '\216'..'\246' | '\248'..'\255' ->
      letter
    | '\n' ->
      not_letter ++ newline
    | _ ->
      not_letter
end

type mark = int
type idx = int

module Pmark : sig
  type t = private int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val gen : unit -> t
  val pp : Format.formatter -> t -> unit
end
= struct
  type t = int
  let equal (x : int) (y : int) = x = y
  let compare (x : int) (y : int) = compare x y
  let r = ref 0
  let gen () = incr r ; !r

  let pp = Format.pp_print_int
end

type expr = { id : int; def : def }

and def =
    Cst of Cset.t
  | Alt of expr list
  | Seq of sem * expr * expr
  | Eps
  | Rep of rep_kind * sem * expr
  | Mark of int
  | Erase of int * int
  | Before of Category.t
  | After of Category.t
  | Pmark of Pmark.t

module PmarkSet = Set.Make(Pmark)

let hash_combine h accu = accu * 65599 + h

module Marks = struct
  type t =
    { marks : (int * int) list
    ; pmarks : PmarkSet.t }

  let empty = { marks = [] ; pmarks = PmarkSet.empty }

  let rec merge_marks_offset old = function
    | [] ->
      old
    | (i, v) :: rem ->
      let nw' = merge_marks_offset (List.remove_assq i old) rem in
      if v = -2 then
        nw'
      else
        (i, v) :: nw'

  let merge old nw =
    { marks = merge_marks_offset old.marks nw.marks
    ; pmarks = PmarkSet.union old.pmarks nw.pmarks }

  let rec hash_marks_offset l accu =
    match l with
      []          -> accu
    | (a, i) :: r -> hash_marks_offset r (hash_combine a (hash_combine i accu))

  let hash m accu =
    hash_marks_offset m.marks (hash_combine (Hashtbl.hash m.pmarks) accu)

  let rec marks_set_idx idx = function
    | (a, -1) :: rem ->
      (a, idx) :: marks_set_idx idx rem
    | marks ->
      marks

  let marks_set_idx marks idx =
    { marks with marks = marks_set_idx idx marks.marks }

  let pp_marks ch t =
    match t.marks with
    | [] ->
      ()
    | (a, i) :: r ->
      Format.fprintf ch "%d-%d" a i;
      List.iter (fun (a, i) -> Format.fprintf ch " %d-%d" a i) r
end

(****)

let pp_sem ch k =
  Format.pp_print_string ch
    (match k with
       `Shortest -> "short"
     | `Longest  -> "long"
     | `First    -> "first")


let pp_rep_kind fmt = function
  | `Greedy -> Format.pp_print_string fmt "Greedy"
  | `Non_greedy -> Format.pp_print_string fmt "Non_greedy"

let rec pp ch e =
  let open Re_fmt in
  match e.def with
    Cst l ->
    sexp ch "cst" Cset.pp l;
  | Alt l ->
    sexp ch "alt" (list pp) l
  | Seq (k, e, e') ->
    sexp ch "seq" (triple pp_sem pp pp) (k, e, e')
  | Eps ->
    str ch "eps"
  | Rep (_rk, k, e) ->
    sexp ch "rep" (pair pp_sem pp) (k, e)
  | Mark i ->
    sexp ch "mark" int i
  | Pmark i ->
    sexp ch "pmark" int (i :> int)
  | Erase (b, e) ->
    sexp ch "erase" (pair int int) (b, e)
  | Before c ->
    sexp ch "before" Category.pp c
  | After c ->
    sexp ch "after" Category.pp c


(****)

let rec first f = function
  | [] ->
    None
  | x :: r ->
    match f x with
      None          -> first f r
    | Some _ as res -> res

(****)

type ids = int ref
let create_ids () = ref 0

let eps_expr = { id = 0; def = Eps }

let mk_expr ids def =
  incr ids;
  { id = !ids; def = def }

let empty ids = mk_expr ids (Alt [])

let cst ids s =
  if Re_cset.is_empty s
  then empty ids
  else mk_expr ids (Cst s)

let alt ids = function
  | []  -> empty ids
  | [c] -> c
  | l   -> mk_expr ids (Alt l)

let seq ids kind x y =
  match x.def, y.def with
    Alt [], _                 -> x
  | _, Alt []                 -> y
  | Eps, _                    -> y
  | _, Eps when kind = `First -> x
  | _                         -> mk_expr ids (Seq (kind, x, y))

let is_eps expr =
  match expr.def with
  | Eps -> true
  | _ -> false

let eps ids = mk_expr ids Eps

let rep ids kind sem x = mk_expr ids (Rep (kind, sem, x))

let mark ids m = mk_expr ids (Mark m)

let pmark ids i = mk_expr ids (Pmark i)

let erase ids m m' = mk_expr ids (Erase (m, m'))

let before ids c = mk_expr ids (Before c)

let after ids c = mk_expr ids (After c)

(****)

let rec rename ids x =
  match x.def with
    Cst _ | Eps | Mark _ | Pmark _ | Erase _ | Before _ | After _ ->
    mk_expr ids x.def
  | Alt l ->
    mk_expr ids (Alt (List.map (rename ids) l))
  | Seq (k, y, z) ->
    mk_expr ids (Seq (k, rename ids y, rename ids z))
  | Rep (g, k, y) ->
    mk_expr ids (Rep (g, k, rename ids y))

(****)

type hash = int
type mark_infos = int array
type status = Failed | Match of mark_infos * PmarkSet.t | Running

module E = struct
  type t =
    | TSeq of t list * expr * sem
    | TExp of Marks.t * expr
    | TMatch of Marks.t

  let rec equal l1 l2 =
    match l1, l2 with
    | [], [] ->
      true
    | TSeq (l1', e1, _) :: r1, TSeq (l2', e2, _) :: r2 ->
      e1.id = e2.id && equal l1' l2' && equal r1 r2
    | TExp (marks1, e1) :: r1, TExp (marks2, e2) :: r2 ->
      e1.id = e2.id && marks1 = marks2 && equal r1 r2
    | TMatch marks1 :: r1, TMatch marks2 :: r2 ->
      marks1 = marks2 && equal r1 r2
    | _ ->
      false

  let rec hash l accu =
    match l with
    | [] ->
      accu
    | TSeq (l', e, _) :: r ->
      hash r (hash_combine 0x172a1bce (hash_combine e.id (hash l' accu)))
    | TExp (marks, e) :: r ->
      hash r
        (hash_combine 0x2b4c0d77 (hash_combine e.id (Marks.hash marks accu)))
    | TMatch marks :: r ->
      hash r (hash_combine 0x1c205ad5 (Marks.hash marks accu))

  let texp marks x = TExp (marks, x)

  let tseq kind x y rem =
    match x with
      []                              -> rem
    | [TExp (marks, {def = Eps ; _})] -> TExp (marks, y) :: rem
    | _                               -> TSeq (x, y, kind) :: rem

  let rec print_state_rec ch e y =
    match e with
    | TMatch marks ->
      Format.fprintf ch "@[<2>(Match@ %a)@]" Marks.pp_marks marks
    | TSeq (l', x, _kind) ->
      Format.fprintf ch "@[<2>(Seq@ ";
      print_state_lst ch l' x;
      Format.fprintf ch " %a)@]" pp x
    | TExp (marks, {def = Eps; _}) ->
      Format.fprintf ch "(Exp %d (%a) (eps))" y.id Marks.pp_marks marks
    | TExp (marks, x) ->
      Format.fprintf ch "(Exp %d (%a) %a)" x.id Marks.pp_marks marks pp x

  and print_state_lst ch l y =
    match l with
      [] ->
      Format.fprintf ch "()"
    | e :: rem ->
      print_state_rec ch e y;
      List.iter
        (fun e ->
           Format.fprintf ch " | ";
           print_state_rec ch e y)
        rem

  let pp ch t = print_state_lst ch [t] { id = 0; def = Eps }
end

module State = struct
  type t =
    { idx: idx
    ; category: Category.t
    ; desc: E.t list
    ; mutable status: status option
    ; hash: hash }

  let dummy =
    { idx = -1
    ; category = Category.dummy
    ; desc = []
    ; status = None
    ; hash = -1 }

  let hash idx cat desc =
    E.hash desc (hash_combine idx (hash_combine (Category.to_int cat) 0)) land 0x3FFFFFFF

  let mk idx cat desc =
    { idx
    ; category = cat
    ; desc
    ; status = None
    ; hash = hash idx cat desc}

  let create cat e = mk 0 cat [E.TExp (Marks.empty, e)]

  let equal x y =
    (x.hash : int) = y.hash && (x.idx : int) = y.idx &&
    Category.equal x.category y.category && E.equal x.desc y.desc

  let compare x y =
    let c = compare (x.hash : int) y.hash in
    if c <> 0 then c else
      let c = Category.compare x.category y.category in
      if c <> 0 then c else
        compare x.desc y.desc

  type t' = t
  module Table = Hashtbl.Make(
    struct
      type t = t'
      let equal = equal
      let hash t = t.hash
    end)
end

(**** Find a free index ****)

type working_area = bool array ref

let create_working_area () = ref [| false |]

let index_count w = Array.length !w

let reset_table a = Array.fill a 0 (Array.length a) false

let rec mark_used_indices tbl =
  List.iter (function
      | E.TSeq (l, _, _) -> mark_used_indices tbl l
      | E.TExp (marks, _)
      | E.TMatch marks ->
        List.iter (fun (_, i) -> if i >= 0 then tbl.(i) <- true)
          marks.Marks.marks)

let rec find_free tbl idx len =
  if idx = len || not tbl.(idx) then idx else find_free tbl (idx + 1) len

let free_index tbl_ref l =
  let tbl = !tbl_ref in
  reset_table tbl;
  mark_used_indices tbl l;
  let len = Array.length tbl in
  let idx = find_free tbl 0 len in
  if idx = len then tbl_ref := Array.make (2 * len) false;
  idx

(**** Computation of the next state ****)

let remove_matches = List.filter (function E.TMatch _ -> false | _ -> true)

let rec split_at_match_rec l' = function
  | []            -> assert false
  | E.TMatch _ :: r -> (List.rev l', remove_matches r)
  | x :: r        -> split_at_match_rec (x :: l') r

let split_at_match l = split_at_match_rec [] l

let rec remove_duplicates prev l y =
  match l with
    [] ->
    ([], prev)
  | E.TMatch _ as x :: _ -> (* Truncate after first match *)
    ([x], prev)
  | E.TSeq (l', x, kind) :: r ->
    let (l'', prev') = remove_duplicates prev l' x in
    let (r', prev'') = remove_duplicates prev' r y in
    (E.tseq kind l'' x r', prev'')
  | E.TExp (_marks, {def = Eps; _}) as e :: r ->
    if List.memq y.id prev then
      remove_duplicates prev r y
    else
      let (r', prev') = remove_duplicates (y.id :: prev) r y in
      (e :: r', prev')
  | E.TExp (_marks, x) as e :: r ->
    if List.memq x.id prev then
      remove_duplicates prev r y
    else
      let (r', prev') = remove_duplicates (x.id :: prev) r y in
      (e :: r', prev')

let rec set_idx idx = function
  | [] ->
    []
  | E.TMatch marks :: r ->
    E.TMatch (Marks.marks_set_idx marks idx) :: set_idx idx r
  | E.TSeq (l', x, kind) :: r ->
    E.TSeq (set_idx idx l', x, kind) :: set_idx idx r
  | E.TExp (marks, x) :: r ->
    E.TExp ((Marks.marks_set_idx marks idx), x) :: set_idx idx r

let filter_marks b e marks =
  {marks with Marks.marks = List.filter (fun (i, _) -> i < b || i > e) marks.Marks.marks }

let rec delta_1 marks c ~next_cat ~prev_cat x rem =
  (*Format.eprintf "%d@." x.id;*)
  match x.def with
    Cst s ->
    if Cset.mem c s then E.texp marks eps_expr :: rem else rem
  | Alt l ->
    delta_2 marks c ~next_cat ~prev_cat l rem
  | Seq (kind, y, z) ->
    let y' = delta_1 marks c ~next_cat ~prev_cat y [] in
    delta_seq c ~next_cat ~prev_cat kind y' z rem
  | Rep (rep_kind, kind, y) ->
    let y' = delta_1 marks c ~next_cat ~prev_cat y [] in
    let (y'', marks') =
      match
        first
          (function E.TMatch marks -> Some marks | _ -> None) y'
      with
        None        -> (y', marks)
      | Some marks' -> (remove_matches y', marks')
    in
    begin match rep_kind with
        `Greedy     -> E.tseq kind y'' x (E.TMatch marks' :: rem)
      | `Non_greedy -> E.TMatch marks :: E.tseq kind y'' x rem
    end
  | Eps ->
    E.TMatch marks :: rem
  | Mark i ->
    let marks = { marks with Marks.marks = (i, -1) :: List.remove_assq i marks.Marks.marks } in
    E.TMatch marks :: rem
  | Pmark i ->
    let marks = { marks with Marks.pmarks = PmarkSet.add i marks.Marks.pmarks } in
    E.TMatch marks :: rem
  | Erase (b, e) ->
    E.TMatch (filter_marks b e marks) :: rem
  | Before cat'' ->
    if Category.intersect next_cat cat'' then E.TMatch marks :: rem else rem
  | After cat'' ->
    if Category.intersect prev_cat cat'' then E.TMatch marks :: rem else rem

and delta_2 marks c ~next_cat ~prev_cat l rem =
  match l with
    []     -> rem
  | y :: r ->
    delta_1 marks c ~next_cat ~prev_cat y
      (delta_2 marks c ~next_cat ~prev_cat r rem)

and delta_seq c ~next_cat ~prev_cat kind y z rem =
  match
    first (function E.TMatch marks -> Some marks | _ -> None) y
  with
    None ->
    E.tseq kind y z rem
  | Some marks ->
    match kind with
      `Longest ->
      E.tseq kind (remove_matches y) z
        (delta_1 marks c ~next_cat ~prev_cat z rem)
    | `Shortest ->
      delta_1 marks c ~next_cat ~prev_cat z
        (E.tseq kind (remove_matches y) z rem)
    | `First ->
      let (y', y'') = split_at_match y in
      E.tseq kind y' z
        (delta_1 marks c ~next_cat ~prev_cat z (E.tseq kind y'' z rem))

let rec delta_3 c ~next_cat ~prev_cat x rem =
  match x with
    E.TSeq (y, z, kind) ->
    let y' = delta_4 c ~next_cat ~prev_cat y [] in
    delta_seq c ~next_cat ~prev_cat kind y' z rem
  | E.TExp (marks, e) ->
    delta_1 marks c ~next_cat ~prev_cat e rem
  | E.TMatch _ ->
    x :: rem

and delta_4 c ~next_cat ~prev_cat l rem =
  match l with
    []     -> rem
  | y :: r ->
    delta_3 c ~next_cat ~prev_cat y
      (delta_4 c ~next_cat ~prev_cat r rem)

let delta tbl_ref next_cat char st =
  let prev_cat = st.State.category in
  let (expr', _) =
    remove_duplicates []
      (delta_4 char ~next_cat ~prev_cat st.State.desc [])
      eps_expr in
  let idx = free_index tbl_ref expr' in
  let expr'' = set_idx idx expr' in
  State.mk idx next_cat expr''

(****)

let rec red_tr = function
  | [] | [_] as l ->
    l
  | ((s1, st1) as tr1) :: ((s2, st2) as tr2) :: rem ->
    if State.equal st1 st2 then
      red_tr ((Cset.union s1 s2, st1) :: rem)
    else
      tr1 :: red_tr (tr2 :: rem)

let simpl_tr l =
  List.sort
    (fun (s1, _) (s2, _) -> compare s1 s2)
    (red_tr (List.sort (fun (_, st1) (_, st2) -> State.compare st1 st2) l))

(****)

let prepend_deriv = List.fold_right (fun (s, x) l -> Cset.prepend s x l)

let rec restrict s = function
  | [] -> []
  | (s', x') :: rem ->
    let s'' = Cset.inter s s' in
    if Cset.is_empty s''
    then restrict s rem
    else (s'', x') :: restrict s rem

let rec remove_marks b e rem =
  if b > e then rem else remove_marks b (e - 1) ((e, -2) :: rem)

let rec prepend_marks_expr m = function
  | E.TSeq (l, e', s) -> E.TSeq (prepend_marks_expr_lst m l, e', s)
  | E.TExp (m', e')   -> E.TExp (Marks.merge m m', e')
  | E.TMatch m'       -> E.TMatch (Marks.merge m m')

and prepend_marks_expr_lst m l =
  List.map (prepend_marks_expr m) l

let prepend_marks m =
  List.map (fun (s, x) -> (s, prepend_marks_expr_lst m x))

let rec deriv_1 all_chars categories marks cat x rem =
  match x.def with
  | Cst s ->
    Cset.prepend s [E.texp marks eps_expr] rem
  | Alt l ->
    deriv_2 all_chars categories marks cat l rem
  | Seq (kind, y, z) ->
    let y' = deriv_1 all_chars categories marks cat y [(all_chars, [])] in
    deriv_seq all_chars categories cat kind y' z rem
  | Rep (rep_kind, kind, y) ->
    let y' = deriv_1 all_chars categories marks cat y [(all_chars, [])] in
    List.fold_right
      (fun (s, z) rem ->
         let (z', marks') =
           match
             first
               (function E.TMatch marks -> Some marks | _ -> None)
               z
           with
             None        -> (z, marks)
           | Some marks' -> (remove_matches z, marks')
         in
         Cset.prepend s
           (match rep_kind with
              `Greedy     -> E.tseq kind z' x [E.TMatch marks']
            | `Non_greedy -> E.TMatch marks :: E.tseq kind z' x [])
           rem)
      y' rem
  | Eps ->
    Cset.prepend all_chars [E.TMatch marks] rem
  | Mark i ->
    Cset.prepend all_chars [E.TMatch {marks with Marks.marks = ((i, -1) :: List.remove_assq i marks.Marks.marks)}] rem
  | Pmark _ ->
    Cset.prepend all_chars [E.TMatch marks] rem
  | Erase (b, e) ->
    Cset.prepend all_chars
      [E.TMatch {marks with Marks.marks = (remove_marks b e (filter_marks b e marks).Marks.marks)}] rem
  | Before cat' ->
    Cset.prepend (List.assq cat' categories) [E.TMatch marks] rem
  | After cat' ->
    if Category.intersect cat cat' then Cset.prepend all_chars [E.TMatch marks] rem else rem

and deriv_2 all_chars categories marks cat l rem =
  match l with
    []     -> rem
  | y :: r -> deriv_1 all_chars categories marks cat y
                (deriv_2 all_chars categories marks cat r rem)

and deriv_seq all_chars categories cat kind y z rem =
  if
    List.exists
      (fun (_s, xl) ->
         List.exists (function E.TMatch _ -> true | _ -> false) xl)
      y
  then
    let z' = deriv_1 all_chars categories Marks.empty cat z [(all_chars, [])] in
    List.fold_right
      (fun (s, y) rem ->
         match
           first (function E.TMatch marks -> Some marks | _ -> None)
             y
         with
           None ->
           Cset.prepend s (E.tseq kind y z []) rem
         | Some marks ->
           let z'' = prepend_marks marks z' in
           match kind with
             `Longest ->
             Cset.prepend s (E.tseq kind (remove_matches y) z []) (
               prepend_deriv (restrict s z'') rem)
           | `Shortest ->
             prepend_deriv (restrict s z'') (
               Cset.prepend s (E.tseq kind (remove_matches y) z []) rem)
           | `First ->
             let (y', y'') = split_at_match y in
             Cset.prepend s (E.tseq kind y' z []) (
               prepend_deriv (restrict s z'') (
                 Cset.prepend s (E.tseq kind y'' z []) rem)))
      y rem
  else
    List.fold_right
      (fun (s, xl) rem -> Cset.prepend s (E.tseq kind xl z []) rem) y rem

let rec deriv_3 all_chars categories cat x rem =
  match x with
    E.TSeq (y, z, kind) ->
    let y' = deriv_4 all_chars categories cat y [(all_chars, [])] in
    deriv_seq all_chars categories cat kind y' z rem
  | E.TExp (marks, e) ->
    deriv_1 all_chars categories marks cat e rem
  | E.TMatch _ ->
    Cset.prepend all_chars [x] rem

and deriv_4 all_chars categories cat l rem =
  match l with
    []     -> rem
  | y :: r -> deriv_3 all_chars categories cat y
                (deriv_4 all_chars categories cat r rem)

let deriv tbl_ref all_chars categories st =
  let der = deriv_4 all_chars categories st.State.category st.State.desc
      [(all_chars, [])] in
  simpl_tr (
    List.fold_right (fun (s, expr) rem ->
        let (expr', _) = remove_duplicates [] expr eps_expr in
(*
Format.eprintf "@[<3>@[%a@]: %a / %a@]@." Cset.print s print_state expr print_state expr';
*)
        let idx = free_index tbl_ref expr' in
        let expr'' = set_idx idx expr' in
        List.fold_right (fun (cat', s') rem ->
            let s'' = Cset.inter s s' in
            if Cset.is_empty s''
            then rem
            else (s'', State.mk idx cat' expr'') :: rem)
          categories rem) der [])

(****)

let flatten_match m =
  let ma = List.fold_left (fun ma (i, _) -> max ma i) (-1) m in
  let res = Array.make (ma + 1) (-1) in
  List.iter (fun (i, v) -> res.(i) <- v) m;
  res

let status s =
  match s.State.status with
    Some st ->
    st
  | None ->
    let st =
      match s.State.desc with
        []              -> Failed
      | E.TMatch m :: _ -> Match (flatten_match m.Marks.marks, m.Marks.pmarks)
      | _               -> Running
    in
    s.State.status <- Some st;
    st

end
module Re : sig 
#1 "re.mli"
(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

(** Module [Re]: regular expressions commons *)

type t
(** Regular expression *)

type re
(** Compiled regular expression *)

type groups
(** Information about groups in a match. *)

(** {2 Compilation and execution of a regular expression} *)

val compile : t -> re
(** Compile a regular expression into an executable version that can be
    used to match strings, e.g. with {!exec}. *)

val exec :
  ?pos:int ->    (* Default: 0 *)
  ?len:int ->    (* Default: -1 (until end of string) *)
  re -> string -> groups
(** [exec re str] matches [str] against the compiled expression [re],
    and returns the matched groups if any.
    @param pos optional beginning of the string (default 0)
    @param len length of the substring of [str] that can be matched (default [-1],
      meaning to the end of the string
    @raise Not_found if the regular expression can't be found in [str]
*)

val exec_opt :
  ?pos:int ->    (* Default: 0 *)
  ?len:int ->    (* Default: -1 (until end of string) *)
  re -> string -> groups option
(** Similar to {!exec}, but returns an option instead of using an exception. *)

val execp :
  ?pos:int ->    (* Default: 0 *)
  ?len:int ->    (* Default: -1 (until end of string) *)
  re -> string -> bool
(** Similar to {!exec}, but returns [true] if the expression matches,
    and [false] if it doesn't *)

val exec_partial :
  ?pos:int ->    (* Default: 0 *)
  ?len:int ->    (* Default: -1 (until end of string) *)
  re -> string -> [ `Full | `Partial | `Mismatch ]
(** More detailed version of {!exec_p} *)

(** Manipulate matching groups. *)
module Group : sig

  type t = groups
  (** Information about groups in a match. *)

  val get : t -> int -> string
  (** Raise [Not_found] if the group did not match *)

  val offset : t -> int -> int * int
  (** Raise [Not_found] if the group did not match *)

  val start : t -> int -> int
  (** Return the start of the match. Raise [Not_found] if the group did not match. *)

  val stop : t -> int -> int
  (** Return the end of the match. Raise [Not_found] if the group did not match. *)

  val all : t -> string array
  (** Return the empty string for each group which did not match *)

  val all_offset : t -> (int * int) array
  (** Return [(-1,-1)] for each group which did not match *)

  val test : t -> int -> bool
  (** Test whether a group matched *)

  val nb_groups : t -> int
  (** Returns the total number of groups defined - matched or not.
      This function is experimental. *)

  val pp : Format.formatter -> t -> unit

end

(** Marks *)
module Mark : sig

  type t
  (** Mark id *)

  val test : Group.t -> t -> bool
  (** Tell if a mark was matched. *)

  module Set : Set.S with type elt = t

  val all : Group.t -> Set.t
  (** Return all the mark matched. *)

  val equal : t -> t -> bool
  val compare : t -> t -> int

end

(** {2 High Level Operations} *)

type 'a gen = unit -> 'a option

val all :
  ?pos:int ->    (** Default: 0 *)
  ?len:int ->
  re -> string -> Group.t list
(** Repeatedly calls {!exec} on the given string, starting at given
    position and length.*)

val all_gen :
  ?pos:int ->    (** Default: 0 *)
  ?len:int ->
  re -> string -> Group.t gen
(** Same as {!all} but returns a generator *)

val matches :
  ?pos:int ->    (** Default: 0 *)
  ?len:int ->
  re -> string -> string list
(** Same as {!all}, but extracts the matched substring rather than
    returning the whole group. This basically iterates over matched
    strings *)

val matches_gen :
  ?pos:int ->    (** Default: 0 *)
  ?len:int ->
  re -> string -> string gen
(** Same as {!matches}, but returns a generator. *)

val split :
  ?pos:int ->    (** Default: 0 *)
  ?len:int ->
  re -> string -> string list
(** [split re s] splits [s] into chunks separated by [re]. It yields
    the chunks themselves, not the separator. For instance
    this can be used with a whitespace-matching re such as ["[\t ]+"]. *)

val split_gen :
  ?pos:int ->    (** Default: 0 *)
  ?len:int ->
  re -> string -> string gen

type split_token =
  [ `Text of string  (** Text between delimiters *)
  | `Delim of Group.t (** Delimiter *)
  ]

val split_full :
  ?pos:int ->    (** Default: 0 *)
  ?len:int ->
  re -> string -> split_token list

val split_full_gen :
  ?pos:int ->    (** Default: 0 *)
  ?len:int ->
  re -> string -> split_token gen

val replace :
  ?pos:int ->    (** Default: 0 *)
  ?len:int ->
  ?all:bool ->   (** Default: true. Otherwise only replace first occurrence *)
  re ->          (** matched groups *)
  f:(Group.t -> string) ->  (* how to replace *)
  string ->     (** string to replace in *)
  string
(** [replace ~all re ~f s] iterates on [s], and replaces every occurrence
    of [re] with [f substring] where [substring] is the current match.
    If [all = false], then only the first occurrence of [re] is replaced. *)

val replace_string :
  ?pos:int ->    (** Default: 0 *)
  ?len:int ->
  ?all:bool ->   (** Default: true. Otherwise only replace first occurrence *)
  re ->          (** matched groups *)
  by:string ->   (** replacement string *)
  string ->      (** string to replace in *)
  string
(** [replace_string ~all re ~by s] iterates on [s], and replaces every
    occurrence of [re] with [by]. If [all = false], then only the first
    occurrence of [re] is replaced. *)

(** {2 String expressions (literal match)} *)

val str : string -> t
val char : char -> t

(** {2 Basic operations on regular expressions} *)

val alt : t list -> t
(** Alternative *)

val seq : t list -> t
(** Sequence *)

val empty : t
(** Match nothing *)

val epsilon : t
(** Empty word *)

val rep : t -> t
(** 0 or more matches *)

val rep1 : t -> t
(** 1 or more matches *)

val repn : t -> int -> int option -> t
(** [repn re i j] matches [re] at least [i] times
    and at most [j] times, bounds included.
    [j = None] means no upper bound.
*)

val opt : t -> t
(** 0 or 1 matches *)

(** {2 String, line, word} *)

val bol : t
(** Beginning of line *)

val eol : t
(** End of line *)

val bow : t
(** Beginning of word *)

val eow : t
(** End of word *)

val bos : t
(** Beginning of string *)

val eos : t
(** End of string *)

val leol : t
(** Last end of line or end of string *)

val start : t
(** Initial position *)

val stop : t
(** Final position *)

val word : t -> t
(** Word *)

val not_boundary : t
(** Not at a word boundary *)

val whole_string : t -> t
(** Only matches the whole string *)

(** {2 Match semantics} *)

val longest : t -> t
(** Longest match *)

val shortest : t -> t
(** Shortest match *)

val first : t -> t
(** First match *)

(** {2 Repeated match modifiers} *)

val greedy : t -> t
(** Greedy *)

val non_greedy : t -> t
(** Non-greedy *)

(** {2 Groups (or submatches)} *)

val group : t -> t
(** Delimit a group *)

val no_group : t -> t
(** Remove all groups *)

val nest : t -> t
(** when matching against [nest e], only the group matching in the
       last match of e will be considered as matching *)



val mark : t -> Mark.t * t
(** Mark a regexp. the markid can then be used to know if this regexp was used. *)

(** {2 Character sets} *)

val set : string -> t
(** Any character of the string *)

val rg : char -> char -> t
(** Character ranges *)

val inter : t list -> t
(** Intersection of character sets *)

val diff : t -> t -> t
(** Difference of character sets *)

val compl : t list -> t
(** Complement of union *)

(** {2 Predefined character sets} *)

val any : t
(** Any character *)

val notnl : t
(** Any character but a newline *)

val alnum : t
val wordc : t
val alpha : t
val ascii : t
val blank : t
val cntrl : t
val digit : t
val graph : t
val lower : t
val print : t
val punct : t
val space : t
val upper : t
val xdigit : t

(** {2 Case modifiers} *)

val case : t -> t
(** Case sensitive matching *)

val no_case : t -> t
(** Case insensitive matching *)

(****)

(** {2 Internal debugging}  *)

val pp : Format.formatter -> t -> unit

val pp_re : Format.formatter -> re -> unit

(** Alias for {!pp_re}. Deprecated *)
val print_re : Format.formatter -> re -> unit

(** {2 Experimental functions}. *)

val witness : t -> string
(** [witness r] generates a string [s] such that [execp (compile r) s] is
    true *)

(** {2 Deprecated functions} *)

type substrings = Group.t
(** Alias for {!Group.t}. Deprecated *)

val get : Group.t -> int -> string
(** Same as {!Group.get}. Deprecated *)

val get_ofs : Group.t -> int -> int * int
(** Same as {!Group.offset}. Deprecated *)

val get_all : Group.t -> string array
(** Same as {!Group.all}. Deprecated *)

val get_all_ofs : Group.t -> (int * int) array
(** Same as {!Group.all_offset}. Deprecated *)

val test : Group.t -> int -> bool
(** Same as {!Group.test}. Deprecated *)

type markid = Mark.t
(** Alias for {!Mark.t}. Deprecated *)

val marked : Group.t -> Mark.t -> bool
(** Same as {!Mark.test}. Deprecated *)

val mark_set : Group.t -> Mark.Set.t
(** Same as {!Mark.all}. Deprecated *)

end = struct
#1 "re.ml"
(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

module Cset = Re_cset
module Automata = Re_automata
module MarkSet = Automata.PmarkSet
module Category = Automata.Category

let rec iter n f v = if n = 0 then v else iter (n - 1) f (f v)

(****)

let unknown = -2
let break = -3

(* Result of a successful match. *)
type groups =
  { s : string
  (* Input string. Matched strings are substrings of s *)

  ; marks : Automata.mark_infos
  (* Mapping from group indices to positions in gpos. group i has positions 2*i
     - 1, 2*i + 1 in gpos. If the group wasn't matched, then its corresponding
     values in marks will be -1,-1 *)

  ; pmarks : MarkSet.t
  (* Marks positions. i.e. those marks created with Re.marks *)

  ; gpos : int array
  (* Group positions. Adjacent elements are (start, stop) of group match.
     indexed by the values in marks. So group i in an re would be the substring:

     start = t.gpos.(marks.(2*i)) - 1
     stop = t.gpos.(marks.(2*i + 1)) - 1 *)

  ; gcount : int
  (* Number of groups the regular expression contains. Matched or not *) }

type match_info =
  | Match of groups
  | Failed
  | Running

type state =
  { idx : int;
    (* Index of the current position in the position table.
       Not yet computed transitions point to a dummy state where
       [idx] is set to [unknown];
       If [idx] is set to [break] for states that either always
       succeed or always fail. *)
    real_idx : int;
    (* The real index, in case [idx] is set to [break] *)
    next : state array;
    (* Transition table, indexed by color *)
    mutable final :
      (Category.t *
       (Automata.idx * Automata.status)) list;
    (* Mapping from the category of the next character to
       - the index where the next position should be saved
       - possibly, the list of marks (and the corresponding indices)
         corresponding to the best match *)
    desc : Automata.State.t
    (* Description of this state of the automata *) }

(* Automata (compiled regular expression) *)
type re =
  { initial : Automata.expr;
    (* The whole regular expression *)
    mutable initial_states : (Category.t * state) list;
    (* Initial states, indexed by initial category *)
    cols : Bytes.t;
    (* Color table *)
    col_repr : Bytes.t;
    (* Table from colors to one character of this color *)
    ncol : int;
    (* Number of colors. *)
    lnl : int;
    (* Color of the last newline *)
    tbl : Automata.working_area;
    (* Temporary table used to compute the first available index
       when computing a new state *)
    states : state Automata.State.Table.t;
    (* States of the deterministic automata *)
    group_count : int
    (* Number of groups in the regular expression *) }

let pp_re ch re = Automata.pp ch re.initial

let print_re = pp_re

(* Information used during matching *)
type info =
  { re : re;
    (* The automata *)
    i_cols : Bytes.t;
    (* Color table ([x.i_cols = x.re.cols])
       Shortcut used for performance reasons *)
    mutable positions : int array;
    (* Array of mark positions
       The mark are off by one for performance reasons *)
    pos : int;
    (* Position where the match is started *)
    last : int
    (* Position where the match should stop *) }


(****)

let category re c =
  if c = -1 then
    Category.inexistant
    (* Special category for the last newline *)
  else if c = re.lnl then
    Category.(lastnewline ++ newline ++ not_letter)
  else
    Category.from_char (Bytes.get re.col_repr c)

(****)

let dummy_next = [||]

let unknown_state =
  { idx = unknown; real_idx = 0;
    next = dummy_next; final = [];
    desc = Automata.State.dummy }

let mk_state ncol desc =
  let break_state =
    match Automata.status desc with
    | Automata.Running -> false
    | Automata.Failed
    | Automata.Match _ -> true
  in
  { idx = if break_state then break else desc.Automata.State.idx;
    real_idx = desc.Automata.State.idx;
    next = if break_state then dummy_next else Array.make ncol unknown_state;
    final = [];
    desc = desc }

let find_state re desc =
  try
    Automata.State.Table.find re.states desc
  with Not_found ->
    let st = mk_state re.ncol desc in
    Automata.State.Table.add re.states desc st;
    st

(**** Match with marks ****)

let delta info cat c st =
  let desc = Automata.delta info.re.tbl cat c st.desc in
  let len = Array.length info.positions in
  if desc.Automata.State.idx = len && len > 0 then begin
    let pos = info.positions in
    info.positions <- Array.make (2 * len) 0;
    Array.blit pos 0 info.positions 0 len
  end;
  desc

let validate info (s:string) pos st =
  let c = Char.code (Bytes.get info.i_cols (Char.code s.[pos])) in
  let cat = category info.re c in
  let desc' = delta info cat c st in
  let st' = find_state info.re desc' in
  st.next.(c) <- st'

(*
let rec loop info s pos st =
  if pos < info.last then
    let st' = st.next.(Char.code info.i_cols.[Char.code s.[pos]]) in
    let idx = st'.idx in
    if idx >= 0 then begin
      info.positions.(idx) <- pos;
      loop info s (pos + 1) st'
    end else if idx = break then begin
      info.positions.(st'.real_idx) <- pos;
      st'
    end else begin (* Unknown *)
      validate info s pos st;
      loop info s pos st
    end
  else
    st
*)

let rec loop info (s:string) pos st =
  if pos < info.last then
    let st' = st.next.(Char.code (Bytes.get info.i_cols (Char.code s.[pos]))) in
    loop2 info s pos st st'
  else
    st

and loop2 info s pos st st' =
  if st'.idx >= 0 then begin
    let pos = pos + 1 in
    if pos < info.last then begin
      (* It is important to place these reads before the write *)
      (* But then, we don't have enough registers left to store the
         right position.  So, we store the position plus one. *)
      let st'' = st'.next.(Char.code (Bytes.get info.i_cols (Char.code s.[pos]))) in
      info.positions.(st'.idx) <- pos;
      loop2 info s pos st' st''
    end else begin
      info.positions.(st'.idx) <- pos;
      st'
    end
  end else if st'.idx = break then begin
    info.positions.(st'.real_idx) <- pos + 1;
    st'
  end else begin (* Unknown *)
    validate info s pos st;
    loop info s pos st
  end

let rec loop_no_mark info s pos last st =
  if pos < last then
    let st' = st.next.(Char.code (Bytes.get info.i_cols (Char.code s.[pos]))) in
    if st'.idx >= 0 then
      loop_no_mark info s (pos + 1) last st'
    else if st'.idx = break then
      st'
    else begin (* Unknown *)
      validate info s pos st;
      loop_no_mark info s pos last st
    end
  else
    st

let final info st cat =
  try
    List.assq cat st.final
  with Not_found ->
    let st' = delta info cat (-1) st in
    let res = (st'.Automata.State.idx, Automata.status st') in
    st.final <- (cat, res) :: st.final;
    res

let find_initial_state re cat =
  try
    List.assq cat re.initial_states
  with Not_found ->
    let st = find_state re (Automata.State.create cat re.initial) in
    re.initial_states <- (cat, st) :: re.initial_states;
    st

let get_color re (s:string) pos =
  if pos < 0 then
    -1
  else
    let slen = String.length s in
    if pos >= slen then
      -1
    else if pos = slen - 1 && re.lnl <> -1 && s.[pos] = '\n' then
      (* Special case for the last newline *)
      re.lnl
    else
      Char.code (Bytes.get re.cols (Char.code s.[pos]))

let rec handle_last_newline info pos st groups =
  let st' = st.next.(info.re.lnl) in
  if st'.idx >= 0 then begin
    if groups then info.positions.(st'.idx) <- pos + 1;
    st'
  end else if st'.idx = break then begin
    if groups then info.positions.(st'.real_idx) <- pos + 1;
    st'
  end else begin (* Unknown *)
    let c = info.re.lnl in
    let real_c = Char.code (Bytes.get info.i_cols (Char.code '\n')) in
    let cat = category info.re c in
    let desc' = delta info cat real_c st in
    let st' = find_state info.re desc' in
    st.next.(c) <- st';
    handle_last_newline info pos st groups
  end

let rec scan_str info (s:string) initial_state groups =
  let pos = info.pos in
  let last = info.last in
  if (last = String.length s
      && info.re.lnl <> -1
      && last > pos
      && String.get s (last - 1) = '\n')
  then begin
    let info = { info with last = last - 1 } in
    let st = scan_str info s initial_state groups in
    if st.idx = break then
      st
    else
      handle_last_newline info (last - 1) st groups
  end else if groups then
    loop info s pos initial_state
  else
    loop_no_mark info s pos last initial_state

let match_str ~groups ~partial re s ~pos ~len =
  let slen = String.length s in
  let last = if len = -1 then slen else pos + len in
  let info =
    { re = re; i_cols = re.cols; pos = pos; last = last;
      positions =
        if groups then begin
          let n = Automata.index_count re.tbl + 1 in
          if n <= 10 then
            [|0;0;0;0;0;0;0;0;0;0|]
          else
            Array.make n 0
        end else
          [||] }
  in
  let initial_cat =
    if pos = 0 then
      Category.(search_boundary ++ inexistant)
    else
      Category.(search_boundary ++ category re (get_color re s (pos - 1))) in
  let initial_state = find_initial_state re initial_cat in
  let st = scan_str info s initial_state groups in
  let res =
    if st.idx = break || partial then
      Automata.status st.desc
    else
      let final_cat =
        if last = slen then
          Category.(search_boundary ++ inexistant)
        else
          Category.(search_boundary ++ category re (get_color re s last)) in
      let (idx, res) = final info st final_cat in
      if groups then info.positions.(idx) <- last + 1;
      res
  in
  match res with
    Automata.Match (marks, pmarks) ->
    Match { s ; marks; pmarks ; gpos = info.positions; gcount = re.group_count}
  | Automata.Failed -> Failed
  | Automata.Running -> Running

let mk_re init cols col_repr ncol lnl group_count =
  { initial = init;
    initial_states = [];
    cols = cols;
    col_repr = col_repr;
    ncol = ncol;
    lnl = lnl;
    tbl = Automata.create_working_area ();
    states = Automata.State.Table.create 97;
    group_count = group_count }

(**** Character sets ****)

let cseq c c' = Cset.seq (Char.code c) (Char.code c')
let cadd c s = Cset.add (Char.code c) s

let trans_set cache cm s =
  match Cset.one_char s with
  | Some i -> Cset.csingle (Bytes.get cm i)
  | None ->
    let v = (Cset.hash_rec s, s) in
    try
      Cset.CSetMap.find v !cache
    with Not_found ->
      let l =
        Cset.fold_right
          s
          ~f:(fun (i, j) l -> Cset.union (cseq (Bytes.get cm i)
                                            (Bytes.get cm j)) l)
          ~init:Cset.empty
      in
      cache := Cset.CSetMap.add v l !cache;
      l

(****)

type regexp =
    Set of Cset.t
  | Sequence of regexp list
  | Alternative of regexp list
  | Repeat of regexp * int * int option
  | Beg_of_line | End_of_line
  | Beg_of_word | End_of_word | Not_bound
  | Beg_of_str | End_of_str
  | Last_end_of_line | Start | Stop
  | Sem of Automata.sem * regexp
  | Sem_greedy of Automata.rep_kind * regexp
  | Group of regexp | No_group of regexp | Nest of regexp
  | Case of regexp | No_case of regexp
  | Intersection of regexp list
  | Complement of regexp list
  | Difference of regexp * regexp
  | Pmark of Automata.Pmark.t * regexp

let rec pp fmt t =
  let open Re_fmt in
  let var s re = sexp fmt s pp re in
  let seq s rel = sexp fmt s (list pp) rel in
  match t with
  | Set s ->  sexp fmt "Set" Cset.pp s
  | Sequence sq -> seq "Sequence" sq
  | Alternative alt -> seq "Alternative" alt
  | Repeat (re, start, stop) ->
    let pp' fmt () = fprintf fmt "%a@ %d%a" pp re   start   optint stop in
    sexp fmt "Repeat" pp' ()
  | Beg_of_line      -> str fmt "Beg_of_line"
  | End_of_line      -> str fmt "End_of_line"
  | Beg_of_word      -> str fmt "Beg_of_word"
  | End_of_word      -> str fmt "End_of_word"
  | Not_bound        -> str fmt "Not_bound"
  | Beg_of_str       -> str fmt "Beg_of_str"
  | End_of_str       -> str fmt "End_of_str"
  | Last_end_of_line -> str fmt "Last_end_of_line"
  | Start            -> str fmt "Start"
  | Stop             -> str fmt "Stop"
  | Sem (sem, re)    ->
    sexp fmt "Sem" (pair Automata.pp_sem pp) (sem, re)
  | Sem_greedy (k, re) ->
    sexp fmt "Sem_greedy" (pair Automata.pp_rep_kind pp) (k, re)
  | Group c        -> var "Group" c
  | No_group c     -> var "No_group" c
  | Nest c         -> var "Nest" c
  | Case c         -> var "Case" c
  | No_case c      -> var "No_case" c
  | Intersection c -> seq "Intersection" c
  | Complement c   -> seq "Complement" c
  | Difference (a, b) -> sexp fmt "Difference" (pair pp pp) (a, b)
  | Pmark (m, r)      -> sexp fmt "Pmark" (pair Automata.Pmark.pp pp) (m, r)

let rec is_charset = function
  | Set _ ->
    true
  | Alternative l | Intersection l | Complement l ->
    List.for_all is_charset l
  | Difference (r, r') ->
    is_charset r && is_charset r'
  | Sem (_, r) | Sem_greedy (_, r)
  | No_group r | Case r | No_case r ->
    is_charset r
  | Sequence _ | Repeat _ | Beg_of_line | End_of_line
  | Beg_of_word | End_of_word | Beg_of_str | End_of_str
  | Not_bound | Last_end_of_line | Start | Stop
  | Group _ | Nest _ | Pmark (_,_)->
    false

(**** Colormap ****)

(*XXX Use a better algorithm allowing non-contiguous regions? *)
let split s cm =
  Re_cset.iter s ~f:(fun i j ->
      Bytes.set cm i '\001';
      Bytes.set cm (j + 1) '\001';
    )

let cupper =
  Cset.union (cseq 'A' 'Z')
    (Cset.union (cseq '\192' '\214') (cseq '\216' '\222'))
let clower = Cset.offset 32 cupper
let calpha =
  List.fold_right cadd ['\170'; '\181'; '\186'; '\223'; '\255']
    (Cset.union clower cupper)
let cdigit = cseq '0' '9'
let calnum = Cset.union calpha cdigit
let cword = cadd '_' calnum

let colorize c regexp =
  let lnl = ref false in
  let rec colorize regexp =
    match regexp with
      Set s                     -> split s c
    | Sequence l                -> List.iter colorize l
    | Alternative l             -> List.iter colorize l
    | Repeat (r, _, _)          -> colorize r
    | Beg_of_line | End_of_line -> split (Cset.csingle '\n') c
    | Beg_of_word | End_of_word
    | Not_bound                 -> split cword c
    | Beg_of_str | End_of_str
    | Start | Stop              -> ()
    | Last_end_of_line          -> lnl := true
    | Sem (_, r)
    | Sem_greedy (_, r)
    | Group r | No_group r
    | Nest r | Pmark (_,r)     -> colorize r
    | Case _ | No_case _
    | Intersection _
    | Complement _
    | Difference _              -> assert false
  in
  colorize regexp;
  !lnl

let make_cmap () = Bytes.make 257 '\000'

let flatten_cmap cm =
  let c = Bytes.create 256 in
  let col_repr = Bytes.create 256 in
  let v = ref 0 in
  Bytes.set c 0 '\000';
  Bytes.set col_repr 0 '\000';
  for i = 1 to 255 do
    if Bytes.get cm i <> '\000' then incr v;
    Bytes.set c i (Char.chr !v);
    Bytes.set col_repr !v (Char.chr i)
  done;
  (c, Bytes.sub col_repr 0 (!v + 1), !v + 1)

(**** Compilation ****)

let rec equal x1 x2 =
  match x1, x2 with
    Set s1, Set s2 ->
    s1 = s2
  | Sequence l1, Sequence l2 ->
    eq_list l1 l2
  | Alternative l1, Alternative l2 ->
    eq_list l1 l2
  | Repeat (x1', i1, j1), Repeat (x2', i2, j2) ->
    i1 = i2 && j1 = j2 && equal x1' x2'
  | Beg_of_line, Beg_of_line
  | End_of_line, End_of_line
  | Beg_of_word, Beg_of_word
  | End_of_word, End_of_word
  | Not_bound, Not_bound
  | Beg_of_str, Beg_of_str
  | End_of_str, End_of_str
  | Last_end_of_line, Last_end_of_line
  | Start, Start
  | Stop, Stop ->
    true
  | Sem (sem1, x1'), Sem (sem2, x2') ->
    sem1 = sem2 && equal x1' x2'
  | Sem_greedy (k1, x1'), Sem_greedy (k2, x2') ->
    k1 = k2 && equal x1' x2'
  | Group _, Group _ -> (* Do not merge groups! *)
    false
  | No_group x1', No_group x2' ->
    equal x1' x2'
  | Nest x1', Nest x2' ->
    equal x1' x2'
  | Case x1', Case x2' ->
    equal x1' x2'
  | No_case x1', No_case x2' ->
    equal x1' x2'
  | Intersection l1, Intersection l2 ->
    eq_list l1 l2
  | Complement l1, Complement l2 ->
    eq_list l1 l2
  | Difference (x1', x1''), Difference (x2', x2'') ->
    equal x1' x2' && equal x1'' x2''
  | Pmark (m1, r1), Pmark (m2, r2) ->
    Automata.Pmark.equal m1 m2 && equal r1 r2
  | _ ->
    false

and eq_list l1 l2 =
  match l1, l2 with
    [], [] ->
    true
  | x1 :: r1, x2 :: r2 ->
    equal x1 x2 && eq_list r1 r2
  | _ ->
    false

let sequence = function
  | [x] -> x
  | l   -> Sequence l

let rec merge_sequences = function
  | [] ->
    []
  | Alternative l' :: r ->
    merge_sequences (l' @ r)
  | Sequence (x :: y) :: r ->
    begin match merge_sequences r with
        Sequence (x' :: y') :: r' when equal x x' ->
        Sequence [x; Alternative [sequence y; sequence y']] :: r'
      | r' ->
        Sequence (x :: y) :: r'
    end
  | x :: r ->
    x :: merge_sequences r

module A = Automata

let enforce_kind ids kind kind' cr =
  match kind, kind' with
    `First, `First -> cr
  | `First, k       -> A.seq ids k cr (A.eps ids)
  |  _               -> cr

(* XXX should probably compute a category mask *)
let rec translate ids kind ign_group ign_case greedy pos cache c = function
  | Set s ->
    (A.cst ids (trans_set cache c s), kind)
  | Sequence l ->
    (trans_seq ids kind ign_group ign_case greedy pos cache c l, kind)
  | Alternative l ->
    begin match merge_sequences l with
        [r'] ->
        let (cr, kind') =
          translate ids kind ign_group ign_case greedy pos cache c r' in
        (enforce_kind ids kind kind' cr, kind)
      | merged_sequences ->
        (A.alt ids
           (List.map
              (fun r' ->
                 let (cr, kind') =
                   translate ids kind ign_group ign_case greedy
                     pos cache c r' in
                 enforce_kind ids kind kind' cr)
              merged_sequences),
         kind)
    end
  | Repeat (r', i, j) ->
    let (cr, kind') =
      translate ids kind ign_group ign_case greedy pos cache c r' in
    let rem =
      match j with
        None ->
        A.rep ids greedy kind' cr
      | Some j ->
        let f =
          match greedy with
            `Greedy ->
            fun rem ->
              A.alt ids
                [A.seq ids kind' (A.rename ids cr) rem; A.eps ids]
          | `Non_greedy ->
            fun rem ->
              A.alt ids
                [A.eps ids; A.seq ids kind' (A.rename ids cr) rem]
        in
        iter (j - i) f (A.eps ids)
    in
    (iter i (fun rem -> A.seq ids kind' (A.rename ids cr) rem) rem, kind)
  | Beg_of_line ->
    (A.after ids Category.(inexistant ++ newline), kind)
  | End_of_line ->
    (A.before ids Category.(inexistant ++ newline), kind)
  | Beg_of_word ->
    (A.seq ids `First
       (A.after ids Category.(inexistant ++ not_letter))
       (A.before ids Category.(inexistant ++ letter)),
     kind)
  | End_of_word ->
    (A.seq ids `First
       (A.after ids Category.(inexistant ++ letter))
       (A.before ids Category.(inexistant ++ not_letter)),
     kind)
  | Not_bound ->
    (A.alt ids [A.seq ids `First
                  (A.after ids Category.letter)
                  (A.before ids Category.letter);
                A.seq ids `First
                  (A.after ids Category.letter)
                  (A.before ids Category.letter)],
     kind)
  | Beg_of_str ->
    (A.after ids Category.inexistant, kind)
  | End_of_str ->
    (A.before ids Category.inexistant, kind)
  | Last_end_of_line ->
    (A.before ids Category.(inexistant ++ lastnewline), kind)
  | Start ->
    (A.after ids Category.search_boundary, kind)
  | Stop ->
    (A.before ids Category.search_boundary, kind)
  | Sem (kind', r') ->
    let (cr, kind'') =
      translate ids kind' ign_group ign_case greedy pos cache c r' in
    (enforce_kind ids kind' kind'' cr,
     kind')
  | Sem_greedy (greedy', r') ->
    translate ids kind ign_group ign_case greedy' pos cache c r'
  | Group r' ->
    if ign_group then
      translate ids kind ign_group ign_case greedy pos cache c r'
    else
      let p = !pos in
      pos := !pos + 2;
      let (cr, kind') =
        translate ids kind ign_group ign_case greedy pos cache c r' in
      (A.seq ids `First (A.mark ids p) (
          A.seq ids `First cr (A.mark ids (p + 1))),
       kind')
  | No_group r' ->
    translate ids kind true ign_case greedy pos cache c r'
  | Nest r' ->
    let b = !pos in
    let (cr, kind') =
      translate ids kind ign_group ign_case greedy pos cache c r'
    in
    let e = !pos - 1 in
    if e < b then
      (cr, kind')
    else
      (A.seq ids `First (A.erase ids b e) cr, kind')
  | Difference _ | Complement _ | Intersection _ | No_case _ | Case _ ->
    assert false
  | Pmark (i, r') ->
    let (cr, kind') =
      translate ids kind ign_group ign_case greedy pos cache c r' in
    (A.seq ids `First (A.pmark ids i) cr, kind')

and trans_seq ids kind ign_group ign_case greedy pos cache c = function
  | [] ->
    A.eps ids
  | [r] ->
    let (cr', kind') =
      translate ids kind ign_group ign_case greedy pos cache c r in
    enforce_kind ids kind kind' cr'
  | r :: rem ->
    let (cr', kind') =
      translate ids kind ign_group ign_case greedy pos cache c r in
    let cr'' =
      trans_seq ids kind ign_group ign_case greedy pos cache c rem in
    if A.is_eps cr'' then
      cr'
    else if A.is_eps cr' then
      cr''
    else
      A.seq ids kind' cr' cr''

(**** Case ****)

let case_insens s =
  Cset.union s (Cset.union (Cset.offset 32 (Cset.inter s cupper))
                  (Cset.offset (-32) (Cset.inter s clower)))

let as_set = function
  | Set s -> s
  | _     -> assert false

(* XXX Should split alternatives into (1) charsets and (2) more
   complex regular expressions; alternative should therefore probably
   be flatten here *)
let rec handle_case ign_case = function
  | Set s ->
    Set (if ign_case then case_insens s else s)
  | Sequence l ->
    Sequence (List.map (handle_case ign_case) l)
  | Alternative l ->
    let l' = List.map (handle_case ign_case) l in
    if is_charset (Alternative l') then
      Set (List.fold_left (fun s r -> Cset.union s (as_set r)) Cset.empty l')
    else
      Alternative l'
  | Repeat (r, i, j) ->
    Repeat (handle_case ign_case r, i, j)
  | Beg_of_line | End_of_line | Beg_of_word | End_of_word | Not_bound
  | Beg_of_str | End_of_str | Last_end_of_line | Start | Stop as r ->
    r
  | Sem (k, r) ->
    let r' = handle_case ign_case r in
    if is_charset r' then r' else Sem (k, r')
  | Sem_greedy (k, r) ->
    let r' = handle_case ign_case r in
    if is_charset r' then r' else Sem_greedy (k, r')
  | Group r ->
    Group (handle_case ign_case r)
  | No_group r ->
    let r' = handle_case ign_case r in
    if is_charset r' then r' else No_group r'
  | Nest r ->
    let r' = handle_case ign_case r in
    if is_charset r' then r' else Nest r'
  | Case r ->
    handle_case false r
  | No_case r ->
    handle_case true r
  | Intersection l ->
    let l' = List.map (fun r -> handle_case ign_case r) l in
    Set (List.fold_left (fun s r -> Cset.inter s (as_set r)) Cset.cany l')
  | Complement l ->
    let l' = List.map (fun r -> handle_case ign_case r) l in
    Set (Cset.diff Cset.cany
           (List.fold_left (fun s r -> Cset.union s (as_set r))
              Cset.empty l'))
  | Difference (r, r') ->
    Set (Cset.inter (as_set (handle_case ign_case r))
           (Cset.diff Cset.cany (as_set (handle_case ign_case r'))))
  | Pmark (i,r) -> Pmark (i,handle_case ign_case r)

(****)

let compile_1 regexp =
  let regexp = handle_case false regexp in
  let c = make_cmap () in
  let need_lnl = colorize c regexp in
  let (col, col_repr, ncol) = flatten_cmap c in
  let lnl = if need_lnl then ncol else -1 in
  let ncol = if need_lnl then ncol + 1 else ncol in
  let ids = A.create_ids () in
  let pos = ref 0 in
  let (r, kind) =
    translate ids
      `First false false `Greedy pos (ref Cset.CSetMap.empty) col regexp in
  let r = enforce_kind ids `First kind r in
  (*Format.eprintf "<%d %d>@." !ids ncol;*)
  mk_re r col col_repr ncol lnl (!pos / 2)

(****)

let rec anchored = function
  | Sequence l ->
    List.exists anchored l
  | Alternative l ->
    List.for_all anchored l
  | Repeat (r, i, _) ->
    i > 0 && anchored r
  | Set _ | Beg_of_line | End_of_line | Beg_of_word | End_of_word
  | Not_bound | End_of_str | Last_end_of_line | Stop
  | Intersection _ | Complement _ | Difference _ ->
    false
  | Beg_of_str | Start ->
    true
  | Sem (_, r) | Sem_greedy (_, r) | Group r | No_group r | Nest r
  | Case r | No_case r | Pmark (_, r) ->
    anchored r

(****)

type t = regexp

let str s =
  let l = ref [] in
  for i = String.length s - 1 downto 0 do
    l := Set (Cset.csingle s.[i]) :: !l
  done;
  Sequence !l
let char c = Set (Cset.csingle c)

let alt = function
  | [r] -> r
  | l   -> Alternative l
let seq = function
  | [r] -> r
  | l   -> Sequence l

let empty = alt []
let epsilon = seq []
let repn r i j =
  if i < 0 then invalid_arg "Re.repn";
  begin match j with
    | Some j when j < i -> invalid_arg "Re.repn"
    | _ -> ()
  end;
  Repeat (r, i, j)
let rep r = repn r 0 None
let rep1 r = repn r 1 None
let opt r = repn r 0 (Some 1)
let bol = Beg_of_line
let eol = End_of_line
let bow = Beg_of_word
let eow = End_of_word
let word r = seq [bow; r; eow]
let not_boundary = Not_bound
let bos = Beg_of_str
let eos = End_of_str
let whole_string r = seq [bos; r; eos]
let leol = Last_end_of_line
let start = Start
let stop = Stop
let longest r = Sem (`Longest, r)
let shortest r = Sem (`Shortest, r)
let first r = Sem (`First, r)
let greedy r = Sem_greedy (`Greedy, r)
let non_greedy r = Sem_greedy (`Non_greedy, r)
let group r = Group r
let no_group r = No_group r
let nest r = Nest r
let mark r = let i = Automata.Pmark.gen () in (i,Pmark (i,r))

let set str =
  let s = ref Cset.empty in
  for i = 0 to String.length str - 1 do
    s := Cset.union (Cset.csingle str.[i]) !s
  done;
  Set !s

let rg c c' = Set (cseq c c')

let inter l =
  let r = Intersection l in
  if is_charset r then
    r
  else
    invalid_arg "Re.inter"

let compl l =
  let r = Complement l in
  if is_charset r then
    r
  else
    invalid_arg "Re.compl"

let diff r r' =
  let r'' = Difference (r, r') in
  if is_charset r'' then
    r''
  else
    invalid_arg "Re.diff"

let any = Set Cset.cany
let notnl = Set (Cset.diff Cset.cany (Cset.csingle '\n'))

let lower = alt [rg 'a' 'z'; char '\181'; rg '\223' '\246'; rg '\248' '\255']
let upper = alt [rg 'A' 'Z'; rg '\192' '\214'; rg '\216' '\222']
let alpha = alt [lower; upper; char '\170'; char '\186']
let digit = rg '0' '9'
let alnum = alt [alpha; digit]
let wordc = alt [alnum; char '_']
let ascii = rg '\000' '\127'
let blank = set "\t "
let cntrl = alt [rg '\000' '\031'; rg '\127' '\159']
let graph = alt [rg '\033' '\126'; rg '\160' '\255']
let print = alt [rg '\032' '\126'; rg '\160' '\255']
let punct =
  alt [rg '\033' '\047'; rg '\058' '\064'; rg '\091' '\096';
       rg '\123' '\126'; rg '\160' '\169'; rg '\171' '\180';
       rg '\182' '\185'; rg '\187' '\191'; char '\215'; char '\247']
let space = alt [char ' '; rg '\009' '\013']
let xdigit = alt [digit; rg 'a' 'f'; rg 'A' 'F']

let case r = Case r
let no_case r = No_case r

(****)

let compile r =
  compile_1 (
    if anchored r then
      group r
    else
      seq [shortest (rep any); group r]
  )

let exec_internal name ?(pos=0) ?(len = -1) ~groups re s =
  if pos < 0 || len < -1 || pos + len > String.length s then
    invalid_arg name;
  match_str ~groups ~partial:false re s ~pos ~len

let exec ?pos ?len re s =
  match exec_internal "Re.exec" ?pos ?len ~groups:true re s with
    Match substr -> substr
  | _            -> raise Not_found

let exec_opt ?pos ?len re s =
  match exec_internal "Re.exec_opt" ?pos ?len ~groups:true re s with
    Match substr -> Some substr
  | _            -> None

let execp ?pos ?len re s =
  match exec_internal ~groups:false "Re.execp" ?pos ?len re s with
    Match _substr -> true
  | _             -> false

let exec_partial ?pos ?len re s =
  match exec_internal ~groups:false "Re.exec_partial" ?pos ?len re s with
    Match _ -> `Full
  | Running -> `Partial
  | Failed  -> `Mismatch

module Group = struct

  type t = groups

  let offset t i =
    if 2 * i + 1 >= Array.length t.marks then raise Not_found;
    let m1 = t.marks.(2 * i) in
    if m1 = -1 then raise Not_found;
    let p1 = t.gpos.(m1) - 1 in
    let p2 = t.gpos.(t.marks.(2 * i + 1)) - 1 in
    (p1, p2)

  let get t i =
    let (p1, p2) = offset t i in
    String.sub t.s p1 (p2 - p1)

  let start subs i = fst (offset subs i)

  let stop subs i = snd (offset subs i)

  let test t i =
    if 2 * i >= Array.length t.marks then
      false
    else
      let idx = t.marks.(2 * i) in
      idx <> -1

  let dummy_offset = (-1, -1)

  let all_offset t =
    let res = Array.make t.gcount dummy_offset in
    for i = 0 to Array.length t.marks / 2 - 1 do
      let m1 = t.marks.(2 * i) in
      if m1 <> -1 then begin
        let p1 = t.gpos.(m1) in
        let p2 = t.gpos.(t.marks.(2 * i + 1)) in
        res.(i) <- (p1 - 1, p2 - 1)
      end
    done;
    res

  let dummy_string = ""

  let all t =
    let res = Array.make t.gcount dummy_string in
    for i = 0 to Array.length t.marks / 2 - 1 do
      let m1 = t.marks.(2 * i) in
      if m1 <> -1 then begin
        let p1 = t.gpos.(m1) in
        let p2 = t.gpos.(t.marks.(2 * i + 1)) in
        res.(i) <- String.sub t.s (p1 - 1) (p2 - p1)
      end
    done;
    res

  let pp fmt t =
    let matches =
      let offsets = all_offset t in
      let strs = all t in
      Array.to_list (
        Array.init (Array.length strs) (fun i -> strs.(i), offsets.(i))
      ) in
    let open Re_fmt in
    let pp_match fmt (str, (start, stop)) =
      fprintf fmt "@[(%s (%d %d))@]" str start stop in
    sexp fmt "Group" (list pp_match) matches

  let nb_groups t = t.gcount
end

module Mark = struct

  type t = Automata.Pmark.t

  let test {pmarks ; _} p =
    Automata.PmarkSet.mem p pmarks

  let all s = s.pmarks

  module Set = MarkSet

  let equal = Automata.Pmark.equal

  let compare = Automata.Pmark.compare

end

type 'a gen = unit -> 'a option

let all_gen ?(pos=0) ?len re s =
  if pos < 0 then invalid_arg "Re.all";
  (* index of the first position we do not consider.
     !pos < limit is an invariant *)
  let limit = match len with
    | None -> String.length s
    | Some l ->
      if l<0 || pos+l > String.length s then invalid_arg "Re.all";
      pos+l
  in
  (* iterate on matches. When a match is found, search for the next
     one just after its end *)
  let pos = ref pos in
  fun () ->
    if !pos >= limit
    then None  (* no more matches *)
    else
      match match_str ~groups:true ~partial:false re s
              ~pos:!pos ~len:(limit - !pos) with
      | Match substr ->
        let p1, p2 = Group.offset substr 0 in
        pos := if p1=p2 then p2+1 else p2;
        Some substr
      | Running
      | Failed -> None

let all ?pos ?len re s =
  let l = ref [] in
  let g = all_gen ?pos ?len re s in
  let rec iter () = match g() with
    | None -> List.rev !l
    | Some sub -> l := sub :: !l; iter ()
  in iter ()

let matches_gen ?pos ?len re s =
  let g = all_gen ?pos ?len re s in
  fun () ->
    match g() with
    | None -> None
    | Some sub -> Some (Group.get sub 0)

let matches ?pos ?len re s =
  let l = ref [] in
  let g = all_gen ?pos ?len re s in
  let rec iter () = match g() with
    | None -> List.rev !l
    | Some sub -> l := Group.get sub 0 :: !l; iter ()
  in iter ()

type split_token =
  [ `Text of string
  | `Delim of groups
  ]

let split_full_gen ?(pos=0) ?len re s =
  if pos < 0 then invalid_arg "Re.split";
  let limit = match len with
    | None -> String.length s
    | Some l ->
      if l<0 || pos+l > String.length s then invalid_arg "Re.split";
      pos+l
  in
  (* i: start of delimited string
     pos: first position after last match of [re]
     limit: first index we ignore (!pos < limit is an invariant) *)
  let pos0 = pos in
  let state = ref `Idle in
  let i = ref pos and pos = ref pos in
  let next () = match !state with
    | `Idle when !pos >= limit ->
      if !i < limit then (
        let sub = String.sub s !i (limit - !i) in
        incr i;
        Some (`Text sub)
      ) else None
    | `Idle ->
      begin match match_str ~groups:true ~partial:false re s ~pos:!pos
                    ~len:(limit - !pos) with
      | Match substr ->
        let p1, p2 = Group.offset substr 0 in
        pos := if p1=p2 then p2+1 else p2;
        let old_i = !i in
        i := p2;
        if p1 > pos0 then (
          (* string does not start by a delimiter *)
          let text = String.sub s old_i (p1 - old_i) in
          state := `Yield (`Delim substr);
          Some (`Text text)
        ) else Some (`Delim substr)
      | Running -> None
      | Failed ->
        if !i < limit
        then (
          let text = String.sub s !i (limit - !i) in
          i := limit;
          Some (`Text text)  (* yield last string *)
        ) else
          None
      end
    | `Yield x ->
      state := `Idle;
      Some x
  in next

let split_full ?pos ?len re s =
  let l = ref [] in
  let g = split_full_gen ?pos ?len re s in
  let rec iter () = match g() with
    | None -> List.rev !l
    | Some s -> l := s :: !l; iter ()
  in iter ()

let split_gen ?pos ?len re s =
  let g = split_full_gen ?pos ?len re s in
  let rec next() = match g()  with
    | None -> None
    | Some (`Delim _) -> next()
    | Some (`Text s) -> Some s
  in next

let split ?pos ?len re s =
  let l = ref [] in
  let g = split_full_gen ?pos ?len re s in
  let rec iter () = match g() with
    | None -> List.rev !l
    | Some (`Delim _) -> iter()
    | Some (`Text s) -> l := s :: !l; iter ()
  in iter ()

let replace ?(pos=0) ?len ?(all=true) re ~f s =
  if pos < 0 then invalid_arg "Re.replace";
  let limit = match len with
    | None -> String.length s
    | Some l ->
      if l<0 || pos+l > String.length s then invalid_arg "Re.replace";
      pos+l
  in
  (* buffer into which we write the result *)
  let buf = Buffer.create (String.length s) in
  (* iterate on matched substrings. *)
  let rec iter pos =
    if pos < limit
    then
      match match_str ~groups:true ~partial:false re s ~pos ~len:(limit-pos) with
      | Match substr ->
        let p1, p2 = Group.offset substr 0 in
        (* add string between previous match and current match *)
        Buffer.add_substring buf s pos (p1-pos);
        (* what should we replace the matched group with? *)
        let replacing = f substr in
        Buffer.add_string buf replacing;
        if all then
          (* if we matched a non-char e.g. ^ we must manually advance by 1 *)
          iter (
            if p1=p2 then (
              (* a non char could be past the end of string. e.g. $ *)
              if p2 < limit then Buffer.add_char buf s.[p2];
              p2+1
            ) else
              p2)
        else
          Buffer.add_substring buf s p2 (limit-p2)
      | Running -> ()
      | Failed ->
        Buffer.add_substring buf s pos (limit-pos)
  in
  iter pos;
  Buffer.contents buf

let replace_string ?pos ?len ?all re ~by s =
  replace ?pos ?len ?all re s ~f:(fun _ -> by)

let witness t =
  let rec witness = function
    | Set c -> String.make 1 (Char.chr (Cset.pick c))
    | Sequence xs -> String.concat "" (List.map witness xs)
    | Alternative (x :: _) -> witness x
    | Alternative [] -> assert false
    | Repeat (r, from, _to) ->
      let w = witness r in
      let b = Buffer.create (String.length w * from) in
      for _i=1 to from do
        Buffer.add_string b w
      done;
      Buffer.contents b
    | No_case r -> witness r
    | Intersection _
    | Complement _
    | Difference (_, _) -> assert false
    | Group r
    | No_group r
    | Nest r
    | Sem (_, r)
    | Pmark (_, r)
    | Case r
    | Sem_greedy (_, r) -> witness r
    | Beg_of_line
    | End_of_line
    | Beg_of_word
    | End_of_word
    | Not_bound
    | Beg_of_str
    | Last_end_of_line
    | Start
    | Stop
    | End_of_str -> "" in
  witness (handle_case false t)

(** {2 Deprecated functions} *)

type substrings = groups

let get = Group.get
let get_ofs = Group.offset
let get_all = Group.all
let get_all_ofs = Group.all_offset
let test = Group.test

type markid = Mark.t

let marked = Mark.test
let mark_set = Mark.all

(**********************************)

(*
Information about the previous character:
- does not exists
- is a letter
- is not a letter
- is a newline
- is last newline

Beginning of word:
- previous is not a letter or does not exist
- current is a letter or does not exist

End of word:
- previous is a letter or does not exist
- current is not a letter or does not exist

Beginning of line:
- previous is a newline or does not exist

Beginning of buffer:
- previous does not exist

End of buffer
- current does not exist

End of line
- current is a newline or does not exist
*)

(*
Rep: e = T,e | ()
  - semantics of the comma (shortest/longest/first)
  - semantics of the union (greedy/non-greedy)

Bounded repetition
  a{0,3} = (a,(a,a?)?)?
*)

end
module Re_perl : sig 
#1 "re_perl.mli"
(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

(** Perl-style regular expressions *)

exception Parse_error
exception Not_supported
(** Errors that can be raised during the parsing of the regular expression *)


type opt =
  [ `Ungreedy | `Dotall | `Dollar_endonly
  | `Multiline | `Anchored | `Caseless ]

val re : ?opts:opt list -> string -> Re.t
(** Parsing of a Perl-style regular expression *)

val compile : Re.t -> Re.re
(** Regular expression compilation *)

val compile_pat : ?opts:opt list -> string -> Re.re
(** (Same as [Re.compile]) *)

end = struct
#1 "re_perl.ml"
(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

exception Parse_error
exception Not_supported

let posix_class_of_string = function
  | "alnum"  -> Re.alnum
  | "ascii"  -> Re.ascii
  | "blank"  -> Re.blank
  | "cntrl"  -> Re.cntrl
  | "digit"  -> Re.digit
  | "lower"  -> Re.lower
  | "print"  -> Re.print
  | "space"  -> Re.space
  | "upper"  -> Re.upper
  | "word"   -> Re.wordc
  | "punct"  -> Re.punct
  | "graph"  -> Re.graph
  | "xdigit" -> Re.xdigit
  | class_   -> invalid_arg ("Invalid pcre class: " ^ class_)

let posix_class_strings =
  [ "alnum" ; "ascii" ; "blank"
  ; "cntrl" ; "digit" ; "lower"
  ; "print" ; "space" ; "upper"
  ; "word"  ; "punct" ; "graph"
  ; "xdigit" ]

let parse multiline dollar_endonly dotall ungreedy s =
  let i = ref 0 in
  let l = String.length s in
  let eos () = !i = l in
  let test c = not (eos ()) && s.[!i] = c in
  let accept c = let r = test c in if r then incr i; r in
  let accept_s s' =
    let len = String.length s' in
    try
      for j = 0 to len - 1 do
        try if s'.[j] <> s.[!i + j] then raise Exit
        with _ -> raise Exit
      done;
      i := !i + len;
      true
    with Exit -> false in
  let get () = let r = s.[!i] in incr i; r in
  let unget () = decr i in
  let greedy_mod r =
    let gr = accept '?' in
    let gr = if ungreedy then not gr else gr in
    if gr then Re.non_greedy r else Re.greedy r
  in
  let rec regexp () = regexp' (branch ())
  and regexp' left =
    if accept '|' then regexp' (Re.alt [left; branch ()]) else left
  and branch () = branch' []
  and branch' left =
    if eos () || test '|' || test ')' then Re.seq (List.rev left)
    else branch' (piece () :: left)
  and piece () =
    let r = atom () in
    if accept '*' then greedy_mod (Re.rep r) else
    if accept '+' then greedy_mod (Re.rep1 r) else
    if accept '?' then greedy_mod (Re.opt r) else
    if accept '{' then
      match integer () with
        Some i ->
          let j = if accept ',' then integer () else Some i in
          if not (accept '}') then raise Parse_error;
          begin match j with
            Some j when j < i -> raise Parse_error | _ -> ()
          end;
          greedy_mod (Re.repn r i j)
      | None ->
          unget (); r
    else
      r
  and atom () =
    if accept '.' then begin
      if dotall then Re.any else Re.notnl
    end else if accept '(' then begin
      if accept '?' then begin
        if accept ':' then begin
          let r = regexp () in
          if not (accept ')') then raise Parse_error;
          r
        end else if accept '#' then begin
          comment ()
        end else
          raise Parse_error
      end else begin
        let r = regexp () in
        if not (accept ')') then raise Parse_error;
        Re.group r
      end
    end else
    if accept '^' then begin
      if multiline then Re.bol else Re.bos
    end else if accept '$' then begin
      if multiline then Re.eol else if dollar_endonly then Re.leol else Re.eos
    end else if accept '[' then begin
      if accept '^' then
        Re.compl (bracket [])
      else
        Re.alt (bracket [])
    end else if accept '\\' then begin
(* XXX
   - Back-references
   - \cx (control-x), \e, \f, \n, \r, \t, \xhh, \ddd
*)
      if eos () then raise Parse_error;
      match get () with
        'w' ->
          Re.alt [Re.alnum; Re.char '_']
      | 'W' ->
          Re.compl [Re.alnum; Re.char '_']
      | 's' ->
          Re.space
      | 'S' ->
          Re.compl [Re.space]
      | 'd' ->
          Re.digit
      | 'D' ->
          Re.compl [Re.digit]
      | 'b' ->
          Re.alt [Re.bow; Re.eow]
      | 'B' ->
          Re.not_boundary
      | 'A' ->
          Re.bos
      | 'Z' ->
          Re.leol
      | 'z' ->
          Re.eos
      | 'G' ->
          Re.start
      | 'a'..'z' | 'A'..'Z' ->
          raise Parse_error
      | '0'..'9' ->
          raise Not_supported
      | c ->
          Re.char c
    end else begin
      if eos () then raise Parse_error;
      match get () with
        '*' | '+' | '?' | '{' | '\\' -> raise Parse_error
      |                 c            -> Re.char c
    end
  and integer () =
    if eos () then None else
    match get () with
      '0'..'9' as d -> integer' (Char.code d - Char.code '0')
    |     _        -> unget (); None
  and integer' i =
    if eos () then Some i else
    match get () with
      '0'..'9' as d ->
        let i' = 10 * i + (Char.code d - Char.code '0') in
        if i' < i then raise Parse_error;
        integer' i'
    | _ ->
        unget (); Some i
  and bracket s =
    if s <> [] && accept ']' then s else begin
      match char () with
      | `Char c ->
        if accept '-' then begin
          if accept ']' then Re.char c :: Re.char '-' :: s else begin
            match char () with
              `Char c' ->
              bracket (Re.rg c c' :: s)
            | `Set st' ->
              Re.char c :: Re.char '-' :: st' :: s
          end
        end else
          bracket (Re.char c :: s)
      | `Set st -> bracket (st :: s)
    end
  and char () =
    if eos () then raise Parse_error;
    let c = get () in
    if c = '[' then begin
      if accept '=' then raise Not_supported;
      if accept ':' then
        let compl = accept '^' in
        let cls =
          try List.find accept_s posix_class_strings
          with Not_found -> raise Parse_error in
        if not (accept_s ":]") then raise Parse_error;
        let re =
          let posix_class = posix_class_of_string cls in
          if compl then Re.compl [posix_class] else posix_class in
        `Set (re)
      else if accept '.' then begin
        if eos () then raise Parse_error;
        let c = get () in
        if not (accept '.') then raise Not_supported;
        if not (accept ']') then raise Parse_error;
        `Char c
      end else
        `Char c
    end else if c = '\\' then begin
      let c = get () in
(* XXX
   \127, ...
*)
      match c with
        'b' -> `Char '\008'
      | 'n' -> `Char '\n' (*XXX*)
      | 'r' -> `Char '\r' (*XXX*)
      | 't' -> `Char '\t' (*XXX*)
      | 'w' -> `Set (Re.alt [Re.alnum; Re.char '_'])
      | 'W' -> `Set (Re.compl [Re.alnum; Re.char '_'])
      | 's' -> `Set (Re.space)
      | 'S' -> `Set (Re.compl [Re.space])
      | 'd' -> `Set (Re.digit)
      | 'D' -> `Set (Re.compl [Re.digit])
      | 'a'..'z' | 'A'..'Z' ->
          raise Parse_error
      | '0'..'9' ->
          raise Not_supported
      | _ ->
          `Char c
    end else
      `Char c
  and comment () =
    if accept ')' then Re.epsilon else begin incr i; comment () end
  in
  let res = regexp () in
  if not (eos ()) then raise Parse_error;
  res

type opt =
  [ `Ungreedy | `Dotall | `Dollar_endonly
  | `Multiline | `Anchored | `Caseless ]

let re  ?(opts = []) s =
  let r =
    parse
      (List.memq `Multiline opts) (List.memq `Dollar_endonly opts)
      (List.memq `Dotall opts) (List.memq `Ungreedy opts)
      s
  in
  let r = if List.memq `Anchored opts then Re.seq [Re.start; r] else r in
  let r = if List.memq `Caseless opts then Re.no_case r else r in
  r

let compile = Re.compile
let compile_pat ?(opts = []) s = compile (re ~opts s)

end
module Re_pcre : sig 
#1 "re_pcre.mli"
type regexp = Re.re

type flag = [ `CASELESS | `MULTILINE | `ANCHORED ]

type groups = Re.groups

(** Result of a {!Pcre.full_split} *)
type split_result =
  | Text  of string       (** Text part of splitted string *)
  | Delim of string       (** Delimiter part of splitted string *)
  | Group of int * string (** Subgroup of matched delimiter (subgroup_nr, subgroup_str) *)
  | NoGroup               (** Unmatched subgroup *)

val re : ?flags:(flag list) -> string -> Re.t
(** [re ~flags s] creates the regexp [s] using the pcre syntax. *)

val regexp : ?flags:(flag list) -> string -> regexp
(** [re ~flags s] compiles the regexp [s] using the pcre syntax. *)

val extract : rex:regexp -> string -> string array
(** [extract ~rex s] executes [rex] on [s] and returns the matching groups. *)

val exec : rex:regexp -> ?pos:int -> string -> groups
(** Equivalent to {!Re.exec}. *)

val get_substring : groups -> int -> string
(** Equivalent to {!Re.Group.get}. *)

val get_substring_ofs : groups -> int -> int * int
(** Equivalent to {!Re.Group.offset}. *)

val pmatch : rex:regexp -> string -> bool
(** Equivalent to {!Re.execp}. *)

val substitute : rex:Re.re -> subst:(string -> string) -> string -> string

val full_split : ?max:int -> rex:regexp -> string -> split_result list

val split : rex:regexp -> string -> string list

val quote : string -> string

(** {2 Deprecated} *)

type substrings = Re.groups

end = struct
#1 "re_pcre.ml"
type regexp = Re.re

type flag = [ `CASELESS | `MULTILINE | `ANCHORED ]

type split_result =
  | Text  of string
  | Delim of string
  | Group of int * string
  | NoGroup

type groups = Re.groups

let re ?(flags = []) pat =
  let opts = List.map (function
    | `CASELESS -> `Caseless
    | `MULTILINE -> `Multiline
    | `ANCHORED -> `Anchored
  ) flags in
  Re_perl.re ~opts pat

let regexp ?flags pat = Re.compile (re ?flags pat)

let extract ~rex s =
  Re.Group.all (Re.exec rex s)

let exec ~rex ?pos s =
  Re.exec rex ?pos s

let get_substring s i =
  Re.Group.get s i

let get_substring_ofs s i =
  Re.Group.offset s i

let pmatch ~rex s =
  Re.execp rex s

let substitute ~rex ~subst str =
  let b = Buffer.create 1024 in
  let rec loop pos =
    if pos >= String.length str then
      Buffer.contents b
    else if Re.execp ~pos rex str then (
      let ss = Re.exec ~pos rex str in
      let start, fin = Re.Group.offset ss 0 in
      let pat = Re.Group.get ss 0 in
      Buffer.add_substring b str pos (start - pos);
      Buffer.add_string b (subst pat);
      loop fin
    ) else (
      Buffer.add_substring b str pos (String.length str - pos);
      loop (String.length str)
    )
  in
  loop 0

let split ~rex str =
  let rec loop accu pos =
    if pos >= String.length str then
      List.rev accu
    else if Re.execp ~pos rex str then (
      let ss = Re.exec ~pos rex str in
      let start, fin = Re.Group.offset ss 0 in
      let s = String.sub str pos (start - pos) in
      loop (s :: accu) fin
    ) else (
      let s = String.sub str pos (String.length str - pos) in
      loop (s :: accu) (String.length str)
    ) in
  loop [] 0

(* From PCRE *)
let string_unsafe_sub s ofs len =
  let r = Bytes.create len in
  Bytes.unsafe_blit s ofs r 0 len;
  Bytes.unsafe_to_string r

let quote s =
  let len = String.length s in
  let buf = Bytes.create (len lsl 1) in
  let pos = ref 0 in
  for i = 0 to len - 1 do
    match String.unsafe_get s i with
    | '\\' | '^' | '$' | '.' | '[' | '|'
    | '('  | ')' | '?' | '*' | '+' | '{' as c ->
      Bytes.unsafe_set buf !pos '\\';
      incr pos;
      Bytes.unsafe_set buf !pos c; incr pos
    | c -> Bytes.unsafe_set buf !pos c; incr pos
  done;
  string_unsafe_sub buf 0 !pos

let full_split ?(max=0) ~rex s =
  if String.length s = 0 then []
  else if max = 1 then [Text s]
  else
    let results = Re.split_full rex s in
    let matches =
      List.map (function
        | `Text s -> [Text s]
        | `Delim d ->
          let matches = Re.Group.all_offset d in
          let delim = Re.Group.get d 0 in
          (Delim delim)::(
            let l = ref [] in
            for i = 1 to Array.length matches - 1 do
              l :=
                (if matches.(i) = (-1, -1)
                 then NoGroup
                 else Group (i, Re.Group.get d i))
                ::(!l)
            done;
            List.rev !l)) results in
    List.concat matches


type substrings = Re.groups

end
module Xx
= struct
#1 "xx.ml"

let _ =
  let s =
     String.make (1024*1024 - 1) 'a'
     ^ "b"
 in
eq __LOC__ 
  (Re.get (Re_pcre.exec ~rex:(Re_pcre.regexp "aa?b") s)  0) "aab"
 

end

;; Mt.from_pair_suites __FILE__ !suites
