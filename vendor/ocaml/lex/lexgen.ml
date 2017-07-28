(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal,                            *)
(*            Luc Maranget, projet Moscova,                            *)
(*                  INRIA Rocquencourt                                 *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Compiling a lexer definition *)

open Syntax
open Printf

exception Memory_overflow

(* Deep abstract syntax for regular expressions *)

type ident = string *  Syntax.location

type tag_info = {id : string ; start : bool ; action : int}

type regexp =
    Empty
  | Chars of int * bool
  | Action of int
  | Tag of tag_info
  | Seq of regexp * regexp
  | Alt of regexp * regexp
  | Star of regexp

type tag_base = Start | End | Mem of int
type tag_addr = Sum of (tag_base * int)
type ident_info =
  | Ident_string of bool * tag_addr * tag_addr
  | Ident_char of bool * tag_addr
type t_env = (ident * ident_info) list

type ('args,'action) lexer_entry =
  { lex_name: string;
    lex_regexp: regexp;
    lex_mem_tags: int ;
    lex_actions: (int *  t_env * 'action) list }


type automata =
    Perform of int * tag_action list
  | Shift of automata_trans * (automata_move * memory_action list) array

and automata_trans =
    No_remember
  | Remember of int * tag_action list

and automata_move =
    Backtrack
  | Goto of int

and memory_action =
  | Copy of int * int
  | Set of int

and tag_action = SetTag of int * int | EraseTag of int

(* Representation of entry points *)

type ('args,'action) automata_entry =
  { auto_name: string;
    auto_args: 'args ;
    auto_mem_size : int ;
    auto_initial_state: int * memory_action list;
    auto_actions: (int * t_env * 'action) list }


(* A lot of sets and map structures *)

module Ints =
  Set.Make(struct type t = int let compare (x:t) y = compare x y end)

let id_compare (id1,_) (id2,_) = String.compare id1 id2

let tag_compare t1 t2 = Pervasives.compare t1 t2

module Tags = Set.Make(struct type t = tag_info let compare = tag_compare end)

module TagMap =
  Map.Make (struct type t = tag_info let compare = tag_compare end)

module IdSet =
  Set.Make (struct type t = ident let compare = id_compare end)

module IdMap =
  Map.Make (struct type t =  ident let compare = id_compare end)

(*********************)
(* Variable cleaning *)
(*********************)

(* Silently eliminate nested variables *)

let rec do_remove_nested to_remove = function
  | Bind (e,x) ->
      if IdSet.mem x to_remove then
        do_remove_nested to_remove e
      else
        Bind (do_remove_nested (IdSet.add x to_remove) e, x)
  | Epsilon|Eof|Characters _ as e -> e
  | Sequence (e1, e2) ->
      Sequence
        (do_remove_nested to_remove  e1, do_remove_nested to_remove  e2)
  | Alternative (e1, e2) ->
      Alternative
        (do_remove_nested to_remove  e1, do_remove_nested to_remove  e2)
  | Repetition e ->
      Repetition (do_remove_nested to_remove  e)

let remove_nested_as e = do_remove_nested IdSet.empty e

(*********************)
(* Variable analysis *)
(*********************)

(*
  Optional variables.
   A variable is optional when matching of regexp does not
   implies it binds.
     The typical case is:
       ("" | 'a' as x) -> optional
       ("" as x | 'a' as x) -> non-optional
*)

let stringset_delta s1 s2 =
  IdSet.union
    (IdSet.diff s1 s2)
    (IdSet.diff s2 s1)

let rec find_all_vars = function
  | Characters _|Epsilon|Eof ->
      IdSet.empty
  | Bind (e,x) ->
      IdSet.add x (find_all_vars e)
  | Sequence (e1,e2)|Alternative (e1,e2) ->
      IdSet.union (find_all_vars e1) (find_all_vars e2)
  | Repetition e -> find_all_vars e


let rec do_find_opt = function
  | Characters _|Epsilon|Eof -> IdSet.empty, IdSet.empty
  | Bind (e,x) ->
      let opt,all = do_find_opt e in
      opt, IdSet.add x all
  | Sequence (e1,e2) ->
      let opt1,all1 = do_find_opt e1
      and opt2,all2 = do_find_opt e2 in
      IdSet.union opt1 opt2, IdSet.union all1 all2
  | Alternative (e1,e2) ->
      let opt1,all1 = do_find_opt e1
      and opt2,all2 = do_find_opt e2 in
      IdSet.union
        (IdSet.union opt1 opt2)
        (stringset_delta all1 all2),
      IdSet.union all1 all2
  | Repetition e  ->
      let r = find_all_vars e in
      r,r

let find_optional e =
  let r,_ = do_find_opt e in r

(*
   Double variables
   A variable is double when it can be bound more than once
   in a single matching
     The typical case is:
       (e1 as x) (e2 as x)

*)

let rec do_find_double = function
  | Characters _|Epsilon|Eof -> IdSet.empty, IdSet.empty
  | Bind (e,x) ->
      let dbl,all = do_find_double e in
      (if IdSet.mem x all then
        IdSet.add x dbl
      else
        dbl),
      IdSet.add x all
  | Sequence (e1,e2) ->
      let dbl1, all1 = do_find_double e1
      and dbl2, all2 = do_find_double e2 in
      IdSet.union
        (IdSet.inter all1 all2)
        (IdSet.union dbl1 dbl2),
      IdSet.union all1 all2
  | Alternative (e1,e2) ->
      let dbl1, all1 = do_find_double e1
      and dbl2, all2 = do_find_double e2 in
      IdSet.union dbl1 dbl2,
      IdSet.union all1 all2
  | Repetition e ->
      let r = find_all_vars e in
      r,r

let find_double e = do_find_double e

(*
   Type of variables:
    A variable is bound to a char when all its occurences
    bind a pattern of length 1.
     The typical case is:
       (_ as x) -> char
*)

let add_some x = function
  | Some i -> Some (x+i)
  | None   -> None

let add_some_some x y = match x,y with
| Some i, Some j -> Some (i+j)
| _,_            -> None

let rec do_find_chars sz = function
  | Epsilon|Eof    -> IdSet.empty, IdSet.empty, sz
  | Characters _ -> IdSet.empty, IdSet.empty, add_some 1 sz
  | Bind (e,x)   ->
      let c,s,e_sz = do_find_chars (Some 0) e in
      begin match e_sz  with
      | Some 1 ->
          IdSet.add x c,s,add_some 1 sz
      | _ ->
          c, IdSet.add x s, add_some_some sz e_sz
      end
  | Sequence (e1,e2) ->
      let c1,s1,sz1 = do_find_chars sz e1 in
      let c2,s2,sz2 = do_find_chars sz1 e2 in
      IdSet.union c1 c2,
      IdSet.union s1 s2,
      sz2
  | Alternative (e1,e2) ->
      let c1,s1,sz1 = do_find_chars sz e1
      and c2,s2,sz2 = do_find_chars sz e2 in
      IdSet.union c1 c2,
      IdSet.union s1 s2,
      (if sz1 = sz2 then sz1 else None)
  | Repetition e -> do_find_chars None e



let find_chars e =
  let c,s,_ = do_find_chars (Some 0) e in
  IdSet.diff c s

(*******************************)
(* From shallow to deep syntax *)
(*******************************)

let chars = ref ([] : Cset.t list)
let chars_count = ref 0


let rec encode_regexp char_vars act = function
    Epsilon -> Empty
  | Characters cl ->
      let n = !chars_count in
      chars := cl :: !chars;
      incr chars_count;
      Chars(n,false)
  | Eof ->
      let n = !chars_count in
      chars := Cset.eof :: !chars;
      incr chars_count;
      Chars(n,true)
  | Sequence(r1,r2) ->
      let r1 = encode_regexp char_vars act r1 in
      let r2 = encode_regexp char_vars act r2 in
      Seq (r1, r2)
  | Alternative(r1,r2) ->
      let r1 = encode_regexp char_vars act r1 in
      let r2 = encode_regexp char_vars act r2 in
      Alt(r1, r2)
  | Repetition r ->
      let r = encode_regexp char_vars act r in
      Star r
  | Bind (r,((name,_) as x)) ->
      let r = encode_regexp char_vars act r in
      if IdSet.mem x char_vars then
        Seq (Tag {id=name ; start=true ; action=act},r)
      else
        Seq (Tag {id=name ; start=true ; action=act},
          Seq (r, Tag {id=name ; start=false ; action=act}))


(* Optimisation,
    Static optimization :
      Replace tags by offsets relative to the beginning
      or end of matched string.
    Dynamic optimization:
      Replace some non-optional, non-double tags by offsets w.r.t
      a previous similar tag.
*)

let incr_pos = function
  | None   -> None
  | Some i -> Some (i+1)

let decr_pos = function
  | None -> None
  | Some i -> Some (i-1)


let opt = true

let mk_seq r1 r2 = match r1,r2  with
| Empty,_ -> r2
| _,Empty -> r1
| _,_     -> Seq (r1,r2)

let add_pos p i = match p with
| Some (Sum (a,n)) -> Some (Sum (a,n+i))
| None -> None

let mem_name name id_set =
  IdSet.exists (fun (id_name,_) -> name = id_name) id_set

let opt_regexp all_vars char_vars optional_vars double_vars r =

(* From removed tags to their addresses *)
  let env = Hashtbl.create 17 in

(* First static optimizations, from start position *)
  let rec size_forward pos = function
    | Empty|Chars (_,true)|Tag _ -> Some pos
    | Chars (_,false) -> Some (pos+1)
    | Seq (r1,r2) ->
        begin match size_forward pos r1 with
        | None -> None
        | Some pos  -> size_forward pos r2
        end
    | Alt (r1,r2) ->
        let pos1 = size_forward pos r1
        and pos2 = size_forward pos r2 in
        if pos1=pos2 then pos1 else None
    | Star _ -> None
    | Action _ -> assert false in

  let rec simple_forward pos r = match r with
    | Tag n ->
        if mem_name n.id double_vars then
          r,Some pos
        else begin
          Hashtbl.add env (n.id,n.start) (Sum (Start, pos)) ;
          Empty,Some pos
        end
    | Empty -> r, Some pos
    | Chars (_,is_eof) ->
        r,Some (if is_eof then  pos else pos+1)
    | Seq (r1,r2) ->
        let r1,pos = simple_forward pos r1 in
        begin match pos with
        | None -> mk_seq r1 r2,None
        | Some pos ->
            let r2,pos = simple_forward pos r2 in
            mk_seq r1 r2,pos
        end
    | Alt (r1,r2) ->
        let pos1 = size_forward pos r1
        and pos2 = size_forward pos r2 in
        r,(if pos1=pos2 then pos1 else None)
    | Star _ -> r,None
    | Action _ -> assert false in

(* Then static optimizations, from end position *)
  let rec size_backward pos = function
    | Empty|Chars (_,true)|Tag _ -> Some pos
    | Chars (_,false) -> Some (pos-1)
    | Seq (r1,r2) ->
        begin match size_backward pos r2 with
        | None -> None
        | Some pos  -> size_backward pos r1
        end
    | Alt (r1,r2) ->
        let pos1 = size_backward pos r1
        and pos2 = size_backward pos r2 in
        if pos1=pos2 then pos1 else None
    | Star _ -> None
    | Action _ -> assert false in


  let rec simple_backward pos r = match r with
    | Tag n ->
        if mem_name n.id double_vars then
          r,Some pos
        else begin
          Hashtbl.add env (n.id,n.start) (Sum (End, pos)) ;
          Empty,Some pos
        end
    | Empty -> r,Some pos
    | Chars (_,is_eof) ->
        r,Some (if is_eof then pos else pos-1)
    | Seq (r1,r2) ->
        let r2,pos = simple_backward pos r2 in
        begin match pos with
        | None -> mk_seq r1 r2,None
        | Some pos ->
            let r1,pos = simple_backward pos r1 in
            mk_seq r1 r2,pos
        end
    | Alt (r1,r2) ->
        let pos1 = size_backward pos r1
        and pos2 = size_backward pos r2 in
        r,(if pos1=pos2 then pos1 else None)
    | Star _ -> r,None
    | Action _ -> assert false in

  let r =
    if opt then
      let r,_ = simple_forward 0 r in
      let r,_ = simple_backward 0 r in
      r
    else
      r in

  let loc_count = ref 0 in
  let get_tag_addr t =
    try
     Hashtbl.find env t
    with
    | Not_found ->
        let n = !loc_count in
        incr loc_count ;
        Hashtbl.add env t (Sum (Mem n,0)) ;
        Sum (Mem n,0) in

  let rec alloc_exp pos r = match r with
    | Tag n ->
        if mem_name n.id double_vars then
          r,pos
        else begin match pos with
        | Some a ->
            Hashtbl.add env (n.id,n.start) a ;
            Empty,pos
        | None ->
            let a = get_tag_addr (n.id,n.start) in
            r,Some a
        end

    | Empty -> r,pos
    | Chars (_,is_eof) -> r,(if is_eof then pos else add_pos pos 1)
    | Seq (r1,r2) ->
        let r1,pos = alloc_exp pos r1 in
        let r2,pos = alloc_exp pos r2 in
        mk_seq r1 r2,pos
    | Alt (_,_) ->
        let off = size_forward 0 r in
        begin match off with
        | Some i -> r,add_pos pos i
        | None -> r,None
        end
    | Star _ -> r,None
    | Action _ -> assert false in

  let r,_ = alloc_exp None r in
  let m =
    IdSet.fold
      (fun ((name,_) as x) r ->

        let v =
          if IdSet.mem x char_vars then
            Ident_char
              (IdSet.mem x optional_vars, get_tag_addr (name,true))
          else
            Ident_string
              (IdSet.mem x optional_vars,
               get_tag_addr (name,true),
               get_tag_addr (name,false)) in
        (x,v)::r)
      all_vars [] in
  m,r, !loc_count



let encode_casedef casedef =
  let r =
    List.fold_left
      (fun (reg,actions,count,ntags) (expr, act) ->
        let expr = remove_nested_as expr in
        let char_vars = find_chars expr in
        let r = encode_regexp char_vars count expr
        and opt_vars = find_optional expr
        and double_vars,all_vars = find_double expr in
        let m,r,loc_ntags =
          opt_regexp all_vars char_vars opt_vars double_vars r in
        Alt(reg, Seq(r, Action count)),
        (count, m ,act) :: actions,
        (succ count),
        max loc_ntags ntags)
      (Empty, [], 0, 0)
      casedef in
  r

let encode_lexdef def =
  chars := [];
  chars_count := 0;
  let entry_list =
    List.map
      (fun {name=entry_name; args=args; shortest=shortest; clauses=casedef} ->
        let (re,actions,_,ntags) = encode_casedef casedef in
        { lex_name = entry_name;
          lex_regexp = re;
          lex_mem_tags = ntags ;
          lex_actions = List.rev actions },args,shortest)
      def in
  let chr = Array.of_list (List.rev !chars) in
  chars := [];
  (chr, entry_list)

(* To generate directly a NFA from a regular expression.
     Confer Aho-Sethi-Ullman, dragon book, chap. 3
   Extension to tagged automata.
     Confer
       Ville Larikari
       'NFAs with Tagged Transitions, their Conversion to Deterministic
        Automata and Application to Regular Expressions'.
       Symposium on String Processing and Information Retrieval (SPIRE 2000),
     http://kouli.iki.fi/~vlaurika/spire2000-tnfa.ps
(See also)
     http://kouli.iki.fi/~vlaurika/regex-submatch.ps.gz
*)

type t_transition =
    OnChars of int
  | ToAction of int

type transition = t_transition * Tags.t

let trans_compare (t1,tags1) (t2,tags2) =
  match Pervasives.compare  t1 t2 with
  | 0 -> Tags.compare tags1 tags2
  | r -> r


module TransSet =
  Set.Make(struct type t = transition let compare = trans_compare end)

let rec nullable = function
  | Empty|Tag _ -> true
  | Chars (_,_)|Action _ -> false
  | Seq(r1,r2) -> nullable r1 && nullable r2
  | Alt(r1,r2) -> nullable r1 || nullable r2
  | Star r     -> true

let rec emptymatch = function
  | Empty | Chars (_,_) | Action _ -> Tags.empty
  | Tag t       -> Tags.add t Tags.empty
  | Seq (r1,r2) -> Tags.union (emptymatch r1) (emptymatch r2)
  | Alt(r1,r2)  ->
      if nullable r1 then
        emptymatch r1
      else
        emptymatch r2
  | Star r ->
      if nullable r then
        emptymatch r
      else
        Tags.empty

let addtags transs tags =
  TransSet.fold
    (fun (t,tags_t) r -> TransSet.add (t, Tags.union tags tags_t) r)
    transs TransSet.empty


let rec firstpos = function
    Empty|Tag _ -> TransSet.empty
  | Chars (pos,_) -> TransSet.add (OnChars pos,Tags.empty) TransSet.empty
  | Action act -> TransSet.add (ToAction act,Tags.empty) TransSet.empty
  | Seq(r1,r2) ->
      if nullable r1 then
        TransSet.union (firstpos r1) (addtags (firstpos r2) (emptymatch r1))
      else
        firstpos r1
  | Alt(r1,r2) -> TransSet.union (firstpos r1) (firstpos r2)
  | Star r     -> firstpos r


(* Berry-sethi followpos *)
let followpos size entry_list =
  let v = Array.make size TransSet.empty in
  let rec fill s = function
    | Empty|Action _|Tag _ -> ()
    | Chars (n,_) -> v.(n) <- s
    | Alt (r1,r2) ->
        fill s r1 ; fill s r2
    | Seq (r1,r2) ->
        fill
          (if nullable r2 then
            TransSet.union (firstpos r2) (addtags s (emptymatch r2))
          else
            (firstpos r2))
          r1 ;
        fill s r2
    | Star r ->
        fill (TransSet.union (firstpos r) s) r in
  List.iter (fun (entry,_,_) -> fill TransSet.empty entry.lex_regexp)
            entry_list;
  v

(************************)
(* The algorithm itself *)
(************************)

let no_action = max_int

module StateSet =
  Set.Make (struct type t = t_transition let compare = Pervasives.compare end)


module MemMap =
  Map.Make (struct type t = int
                   let compare (x:t) y = Pervasives.compare x y end)

type 'a dfa_state =
  {final : int * ('a * int TagMap.t) ;
   others : ('a * int TagMap.t) MemMap.t}


let dtag oc t =
  fprintf oc "%s<%s>" t.id (if t.start then "s" else "e")

let dmem_map dp ds m =
  MemMap.iter
    (fun k x ->
      eprintf "%d -> " k ; dp x ; ds ())
    m

and dtag_map dp ds m =
  TagMap.iter
    (fun t x ->
      dtag stderr t ; eprintf " -> " ; dp x ; ds ())
    m

let dstate {final=(act,(_,m)) ; others=o} =
  if act <> no_action then begin
    eprintf "final=%d " act ;
    dtag_map (fun x -> eprintf "%d" x) (fun () -> prerr_string " ,") m ;
    prerr_endline ""
  end ;
  dmem_map
    (fun (_,m) ->
      dtag_map (fun x -> eprintf "%d" x) (fun () -> prerr_string " ,") m)
    (fun () -> prerr_endline "")
    o


let dfa_state_empty =
  {final=(no_action, (max_int,TagMap.empty)) ;
   others=MemMap.empty}

and dfa_state_is_empty {final=(act,_) ; others=o} =
  act = no_action &&
  o = MemMap.empty


(* A key is an abstraction on a dfa state,
   two states with the same key can be made the same by
   copying some memory cells into others *)


module StateSetSet =
  Set.Make (struct type t = StateSet.t let compare = StateSet.compare end)

type t_equiv = {tag:tag_info ; equiv:StateSetSet.t}

module MemKey =
  Set.Make
   (struct
     type t = t_equiv

     let compare e1 e2 = match Pervasives.compare e1.tag e2.tag with
     | 0 -> StateSetSet.compare e1.equiv e2.equiv
     | r -> r
   end)

type dfa_key = {kstate : StateSet.t ; kmem : MemKey.t}

(* Map a state to its key *)
let env_to_class m =
  let env1 =
    MemMap.fold
      (fun _ (tag,s) r ->
        try
          let ss = TagMap.find tag r in
          let r = TagMap.remove tag r in
          TagMap.add tag (StateSetSet.add s ss) r
        with
        | Not_found ->
            TagMap.add tag (StateSetSet.add s StateSetSet.empty) r)
      m TagMap.empty in
  TagMap.fold
    (fun tag ss r -> MemKey.add {tag=tag ; equiv=ss} r)
    env1 MemKey.empty


(* trans is nfa_state, m is associated memory map *)
let inverse_mem_map trans m r =
  TagMap.fold
    (fun tag addr r ->
      try
        let otag,s = MemMap.find addr r in
        assert (tag = otag) ;
        let r = MemMap.remove addr r in
        MemMap.add addr (tag,StateSet.add trans s) r
      with
      | Not_found ->
          MemMap.add addr (tag,StateSet.add trans StateSet.empty) r)
    m r

let inverse_mem_map_other n (_,m) r = inverse_mem_map (OnChars n) m r

let get_key {final=(act,(_,m_act)) ; others=o} =
  let env =
    MemMap.fold inverse_mem_map_other
      o
      (if act = no_action then MemMap.empty
      else inverse_mem_map (ToAction act) m_act MemMap.empty) in
  let state_key =
    MemMap.fold (fun n _ r -> StateSet.add (OnChars n) r) o
      (if act=no_action then StateSet.empty
      else StateSet.add (ToAction act) StateSet.empty) in
  let mem_key = env_to_class  env in
  {kstate = state_key ; kmem = mem_key}


let key_compare k1 k2 = match StateSet.compare k1.kstate k2.kstate with
| 0 -> MemKey.compare k1.kmem k2.kmem
| r -> r

(* Association dfa_state -> state_num *)

module StateMap =
  Map.Make(struct type t = dfa_key let compare = key_compare end)

let state_map = ref (StateMap.empty : int StateMap.t)
let todo = Stack.create()
let next_state_num = ref 0
let next_mem_cell = ref 0
let temp_pending = ref false
let tag_cells = Hashtbl.create 17
let state_table = Table.create dfa_state_empty


(* Initial reset of state *)
let reset_state () =
  Stack.clear todo;
  next_state_num := 0 ;
  let _ = Table.trim state_table in
  ()

(* Reset state before processing a given automata.
   We clear both the memory mapping and
   the state mapping, as state sharing beetween different
   automata may lead to incorret estimation of the cell memory size
   BUG ID 0004517 *)


let reset_state_partial ntags =
  next_mem_cell := ntags ;
  Hashtbl.clear tag_cells ;
  temp_pending := false ;
  state_map := StateMap.empty

let do_alloc_temp () =
  temp_pending := true ;
  let n = !next_mem_cell in
  n

let do_alloc_cell used t =
  let available =
    try Hashtbl.find tag_cells t with Not_found -> Ints.empty in
  try
    Ints.choose (Ints.diff available used)
  with
  | Not_found ->
      temp_pending := false ;
      let n = !next_mem_cell in
      if n >= 255 then raise Memory_overflow ;
      Hashtbl.replace tag_cells t (Ints.add n available) ;
      incr next_mem_cell ;
      n

let is_old_addr a = a >= 0
and is_new_addr a = a < 0

let old_in_map m r =
  TagMap.fold
    (fun _ addr r ->
      if is_old_addr addr then
        Ints.add addr r
      else
        r)
    m r

let alloc_map used m mvs =
  TagMap.fold
    (fun tag a (r,mvs) ->
      let a,mvs =
        if is_new_addr a then
          let a = do_alloc_cell used tag in
          a,Ints.add a mvs
        else a,mvs in
      TagMap.add tag a r,mvs)
    m (TagMap.empty,mvs)

let create_new_state {final=(act,(_,m_act)) ; others=o} =
  let used =
    MemMap.fold (fun _ (_,m) r -> old_in_map m r)
      o (old_in_map m_act Ints.empty) in

  let new_m_act,mvs  = alloc_map used m_act Ints.empty in
  let new_o,mvs =
    MemMap.fold (fun k (x,m) (r,mvs) ->
      let m,mvs = alloc_map used m mvs in
      MemMap.add k (x,m) r,mvs)
      o (MemMap.empty,mvs) in
  {final=(act,(0,new_m_act)) ; others=new_o},
  Ints.fold (fun x r -> Set x::r) mvs []

type new_addr_gen = {mutable count : int ; mutable env : int TagMap.t}

let create_new_addr_gen () = {count = -1 ; env = TagMap.empty}

let alloc_new_addr tag r =
  try
    TagMap.find tag r.env
  with
  | Not_found ->
      let a = r.count in
      r.count <- a-1 ;
      r.env <- TagMap.add tag a r.env ;
      a


let create_mem_map tags gen =
  Tags.fold
    (fun tag r -> TagMap.add tag (alloc_new_addr tag gen) r)
    tags TagMap.empty

let create_init_state pos =
  let gen = create_new_addr_gen () in
  let st =
    TransSet.fold
      (fun (t,tags) st ->
        match t with
        | ToAction n ->
            let on,otags = st.final in
            if n < on then
              {st with final = (n, (0,create_mem_map tags gen))}
            else
              st
        | OnChars n ->
            try
              let _ = MemMap.find n st.others in assert false
            with
            | Not_found ->
                {st with others =
                  MemMap.add n (0,create_mem_map tags gen) st.others})
      pos dfa_state_empty in
  st


let get_map t st = match t with
| ToAction _ -> let _,(_,m) = st.final in m
| OnChars n  ->
    let (_,m) = MemMap.find n st.others in
    m

let dest = function | Copy (d,_) | Set d  -> d
and orig = function | Copy (_,o) -> o | Set _ -> -1

let pmv oc mv = fprintf oc "%d <- %d" (dest mv) (orig mv)
let pmvs oc mvs =
  List.iter (fun mv -> fprintf oc "%a " pmv  mv) mvs ;
  output_char oc '\n' ; flush oc


(* Topological sort << a la louche >> *)
let sort_mvs mvs =
  let rec do_rec r mvs = match mvs with
  | [] -> r
  | _  ->
      let dests =
        List.fold_left
          (fun r mv -> Ints.add (dest mv) r)
          Ints.empty mvs in
      let rem,here =
        List.partition
          (fun mv -> Ints.mem (orig mv) dests)
          mvs in
      match here with
      | [] ->
          begin match rem with
          | Copy (d,_)::_ ->
              let d' = do_alloc_temp () in
              Copy (d',d)::
              do_rec r
                (List.map
                   (fun mv ->
                     if orig mv = d then
                       Copy (dest mv,d')
                     else
                       mv)
                   rem)
          | _ -> assert false
          end
      | _  -> do_rec (here@r) rem  in
  do_rec [] mvs

let move_to mem_key src tgt =
  let mvs =
    MemKey.fold
      (fun {tag=tag ; equiv=m} r ->
        StateSetSet.fold
          (fun s r ->
            try
              let t = StateSet.choose s  in
              let src = TagMap.find tag (get_map t src)
              and tgt = TagMap.find tag (get_map t tgt) in
              if src <> tgt then begin
                if is_new_addr src then
                  Set tgt::r
                else
                  Copy (tgt, src)::r
              end else
                r
            with
            | Not_found -> assert false)
          m r)
      mem_key [] in
(* Moves are topologically sorted *)
  sort_mvs mvs


let get_state st =
  let key = get_key st in
  try
    let num = StateMap.find key !state_map in
    num,move_to key.kmem st (Table.get state_table num)
  with Not_found ->
    let num = !next_state_num in
    incr next_state_num;
    let st,mvs = create_new_state st in
    Table.emit state_table st ;
    state_map := StateMap.add key num !state_map;
    Stack.push (st, num) todo;
    num,mvs

let map_on_all_states f old_res =
  let res = ref old_res in
  begin try
    while true do
      let (st, i) = Stack.pop todo in
      let r = f st in
      res := (r, i) :: !res
    done
  with Stack.Empty -> ()
  end;
  !res

let goto_state st =
  if
    dfa_state_is_empty st
  then
    Backtrack,[]
  else
    let n,moves = get_state st in
    Goto n,moves

(****************************)
(* compute reachable states *)
(****************************)

let add_tags_to_map gen tags m =
  Tags.fold
    (fun tag m ->
      let m = TagMap.remove tag m in
      TagMap.add tag (alloc_new_addr tag gen) m)
    tags m

let apply_transition gen r pri m = function
  | ToAction n,tags ->
      let on,(opri,_) = r.final in
      if n < on || (on=n && pri < opri) then
        let m = add_tags_to_map gen tags m in
        {r with final=n,(pri,m)}
      else r
  |  OnChars n,tags ->
      try
        let (opri,_) = MemMap.find n r.others in
        if pri < opri then
          let m = add_tags_to_map gen tags m in
          {r with others=MemMap.add n (pri,m) (MemMap.remove n r.others)}
        else
          r
      with
      | Not_found ->
          let m = add_tags_to_map gen tags m in
          {r with others=MemMap.add n (pri,m) r.others}

(* add transitions ts to new state r
   transitions in ts start from state pri and memory map m
*)
let apply_transitions gen r pri m ts =
  TransSet.fold
    (fun t r -> apply_transition gen r pri m t)
    ts r


(* For a given nfa_state pos, refine char partition *)
let rec split_env gen follow pos m s = function
  | [] -> (* Can occur ! because of non-matching regexp ([^'\000'-'\255']) *)
      []
  | (s1,st1) as p::rem ->
      let here = Cset.inter s s1 in
      if Cset.is_empty here then
        p::split_env gen follow pos m s rem
      else
        let rest = Cset.diff s here in
        let rem =
          if Cset.is_empty rest then
            rem
          else
            split_env gen follow pos m rest rem
        and new_st = apply_transitions gen st1 pos m follow in
        let stay = Cset.diff s1 here in
        if Cset.is_empty stay then
          (here, new_st)::rem
        else
          (stay, st1)::(here, new_st)::rem


(* For all nfa_state pos in a dfa state st *)
let comp_shift gen chars follow st =
  MemMap.fold
    (fun pos (_,m) env -> split_env gen follow.(pos) pos m chars.(pos) env)
    st [Cset.all_chars_eof,dfa_state_empty]


let reachs chars follow st =
  let gen = create_new_addr_gen () in
(* build a association list (char set -> new state) *)
  let env = comp_shift gen chars follow st in
(* change it into (char set -> new state_num) *)
  let env =
    List.map
      (fun (s,dfa_state) -> s,goto_state dfa_state) env in
(* finally build the char indexed array -> new state num *)
  let shift = Cset.env_to_array env in
  shift


let get_tag_mem n env t =
  try
    TagMap.find t env.(n)
  with
  | Not_found -> assert false

let do_tag_actions n env  m =

  let used,r =
    TagMap.fold (fun t m (used,r) ->
      let a = get_tag_mem n env t in
      Ints.add a used,SetTag (a,m)::r) m (Ints.empty,[]) in
  let _,r =
    TagMap.fold
      (fun tag m (used,r) ->
        if not (Ints.mem m used) && tag.start then
          Ints.add m used, EraseTag m::r
        else
          used,r)
      env.(n) (used,r) in
  r


let translate_state shortest_match tags chars follow st =
  let (n,(_,m)) = st.final in
  if MemMap.empty = st.others then
    Perform (n,do_tag_actions n tags m)
  else if shortest_match then begin
    if n=no_action then
      Shift (No_remember,reachs chars follow st.others)
    else
      Perform(n, do_tag_actions n tags m)
  end else begin
    Shift (
    (if n = no_action then
      No_remember
    else
      Remember (n,do_tag_actions n tags m)),
    reachs chars follow st.others)
  end

let dtags chan tags =
  Tags.iter
    (fun t -> fprintf chan " %a" dtag t)
    tags

let dtransset s =
  TransSet.iter
    (fun trans -> match trans with
    | OnChars i,tags ->
        eprintf " (-> %d,%a)" i dtags tags
    | ToAction i,tags ->
        eprintf " ([%d],%a)" i dtags tags)
    s

let dfollow t =
  eprintf "follow=[" ;
  for i = 0 to Array.length t-1 do
    eprintf "%d:" i ;
    dtransset t.(i)
  done ;
  prerr_endline "]"


let make_tag_entry id start act a r = match a with
  | Sum (Mem m,0) ->
      TagMap.add {id=id ; start=start ; action=act} m r
  | _ -> r

let extract_tags l =
  let envs = Array.make (List.length l) TagMap.empty in
  List.iter
    (fun (act,m,_) ->
      envs.(act) <-
         List.fold_right
           (fun ((name,_),v) r -> match v with
           | Ident_char (_,t) -> make_tag_entry name true act t r
           | Ident_string (_,t1,t2) ->
               make_tag_entry name true act t1
               (make_tag_entry name false act t2 r))
           m TagMap.empty)
    l ;
  envs


let make_dfa lexdef =
  let (chars, entry_list) = encode_lexdef lexdef in
  let follow = followpos (Array.length chars) entry_list in
(*
  dfollow follow ;
*)
  reset_state () ;
  let r_states = ref [] in
  let initial_states =
    List.map
      (fun (le,args,shortest) ->
        let tags = extract_tags le.lex_actions in
        reset_state_partial le.lex_mem_tags ;
        let pos_set = firstpos le.lex_regexp in
(*
        prerr_string "trans={" ; dtransset pos_set ; prerr_endline "}" ;
*)
        let init_state = create_init_state pos_set in
        let init_num = get_state init_state in
        r_states :=
           map_on_all_states
             (translate_state shortest tags chars follow) !r_states ;
        { auto_name = le.lex_name;
          auto_args = args ;
          auto_mem_size =
            (if !temp_pending then !next_mem_cell+1 else !next_mem_cell) ;
          auto_initial_state = init_num ;
          auto_actions = le.lex_actions })
      entry_list in
  let states = !r_states in
(*
  prerr_endline "** states **" ;
  for i = 0 to !next_state_num-1 do
    eprintf "+++ %d +++\n" i ;
    dstate (Table.get state_table i) ;
    prerr_endline ""
  done ;
  eprintf "%d states\n" !next_state_num ;
*)
  let actions = Array.make !next_state_num (Perform (0,[])) in
  List.iter (fun (act, i) -> actions.(i) <- act) states;
(* Useless state reset, so as to restrict GC roots *)
  reset_state  () ;
  reset_state_partial  0 ;
  (initial_states, actions)
