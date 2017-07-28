(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

(** String utilities *)

let string_before s n = String.sub s 0 n

let string_after s n = String.sub s n (String.length s - n)

let first_chars s n = String.sub s 0 n

let last_chars s n = String.sub s (String.length s - n) n

(** Representation of character sets **)

module Charset =
  struct
    type t = bytes (* of length 32 *)

    (*let empty = Bytes.make 32 '\000'*)
    let full = Bytes.make 32 '\255'

    let make_empty () = Bytes.make 32 '\000'

    let add s c =
      let i = Char.code c in
      Bytes.set s (i lsr 3)
                (Char.chr (Char.code (Bytes.get s (i lsr 3))
                           lor (1 lsl (i land 7))))

    let add_range s c1 c2 =
      for i = Char.code c1 to Char.code c2 do add s (Char.chr i) done

    let singleton c =
      let s = make_empty () in add s c; s

    (*let range c1 c2 =
      let s = make_empty () in add_range s c1 c2; s
    *)
    let complement s =
      let r = Bytes.create 32 in
      for i = 0 to 31 do
        Bytes.set r i (Char.chr(Char.code (Bytes.get s i) lxor 0xFF))
      done;
      r

    let union s1 s2 =
      let r = Bytes.create 32 in
      for i = 0 to 31 do
        Bytes.set r i (Char.chr(Char.code (Bytes.get s1 i)
                       lor Char.code (Bytes.get s2 i)))
      done;
      r

    let disjoint s1 s2 =
      try
        for i = 0 to 31 do
          if Char.code (Bytes.get s1 i) land Char.code (Bytes.get s2 i)
             <> 0
          then raise Exit
        done;
        true
      with Exit ->
        false

    let iter fn s =
      for i = 0 to 31 do
        let c = Char.code (Bytes.get s i) in
        if c <> 0 then
          for j = 0 to 7 do
            if c land (1 lsl j) <> 0 then fn (Char.chr ((i lsl 3) + j))
          done
      done

    let expand s =
      let r = Bytes.make 256 '\000' in
      iter (fun c -> Bytes.set r (Char.code c) '\001') s;
      r

    let fold_case s =
      let r = make_empty() in
      iter (fun c -> add r (Char.lowercase c); add r (Char.uppercase c)) s;
      r

  end

(** Abstract syntax tree for regular expressions *)

type re_syntax =
    Char of char
  | String of string
  | CharClass of Charset.t * bool  (* true = complemented, false = normal *)
  | Seq of re_syntax list
  | Alt of re_syntax * re_syntax
  | Star of re_syntax
  | Plus of re_syntax
  | Option of re_syntax
  | Group of int * re_syntax
  | Refgroup of int
  | Bol
  | Eol
  | Wordboundary

(** Representation of compiled regular expressions *)

type regexp = {
  prog: int array;         (* bytecode instructions *)
  cpool: string array;     (* constant pool (string literals) *)
  normtable: string;       (* case folding table (if any) *)
  numgroups: int;          (* number of \(...\) groups *)
  numregisters: int;       (* number of nullable Star or Plus *)
  startchars: int          (* index of set of starting chars, or -1 if none *)
}

(** Opcodes for bytecode instructions; see strstubs.c for description *)

let op_CHAR = 0
let op_CHARNORM = 1
let op_STRING = 2
let op_STRINGNORM = 3
let op_CHARCLASS = 4
let op_BOL = 5
let op_EOL = 6
let op_WORDBOUNDARY = 7
let op_BEGGROUP = 8
let op_ENDGROUP = 9
let op_REFGROUP = 10
let op_ACCEPT = 11
let op_SIMPLEOPT = 12
let op_SIMPLESTAR = 13
let op_SIMPLEPLUS = 14
let op_GOTO = 15
let op_PUSHBACK = 16
let op_SETMARK = 17
let op_CHECKPROGRESS = 18

(* Encoding of bytecode instructions *)

let instr opc arg = opc lor (arg lsl 8)

(* Computing relative displacements for GOTO and PUSHBACK instructions *)

let displ dest from = dest - from - 1

(** Compilation of a regular expression *)

(* Determine if a regexp can match the empty string *)

let rec is_nullable = function
    Char c -> false
  | String s -> s = ""
  | CharClass(cl, cmpl) -> false
  | Seq rl -> List.for_all is_nullable rl
  | Alt (r1, r2) -> is_nullable r1 || is_nullable r2
  | Star r -> true
  | Plus r -> is_nullable r
  | Option r -> true
  | Group(n, r) -> is_nullable r
  | Refgroup n -> true
  | Bol -> true
  | Eol -> true
  | Wordboundary -> true

(* first r returns a set of characters C such that:
     for all string s, s matches r => the first character of s is in C.
   For convenience, return Charset.full if r is nullable. *)

let rec first = function
    Char c -> Charset.singleton c
  | String s -> if s = "" then Charset.full else Charset.singleton s.[0]
  | CharClass(cl, cmpl) -> if cmpl then Charset.complement cl else cl
  | Seq rl -> first_seq rl
  | Alt (r1, r2) -> Charset.union (first r1) (first r2)
  | Star r -> Charset.full
  | Plus r -> first r
  | Option r -> Charset.full
  | Group(n, r) -> first r
  | Refgroup n -> Charset.full
  | Bol -> Charset.full
  | Eol -> Charset.full
  | Wordboundary -> Charset.full

and first_seq = function
    [] -> Charset.full
  | (Bol | Eol | Wordboundary) :: rl -> first_seq rl
  | Star r :: rl -> Charset.union (first r) (first_seq rl)
  | Option r :: rl -> Charset.union (first r) (first_seq rl)
  | r :: rl -> first r

(* Transform a Char or CharClass regexp into a character class *)

let charclass_of_regexp fold_case re =
  let (cl1, compl) =
    match re with
    | Char c -> (Charset.singleton c, false)
    | CharClass(cl, compl) -> (cl, compl)
    | _ -> assert false in
  let cl2 = if fold_case then Charset.fold_case cl1 else cl1 in
  Bytes.to_string (if compl then Charset.complement cl2 else cl2)

(* The case fold table: maps characters to their lowercase equivalent *)

let fold_case_table =
  let t = Bytes.create 256 in
  for i = 0 to 255 do Bytes.set t i (Char.lowercase(Char.chr i)) done;
  Bytes.to_string t

module StringMap =
  Map.Make(struct type t = string let compare (x:t) y = compare x y end)

(* Compilation of a regular expression *)

let compile fold_case re =

  (* Instruction buffering *)
  let prog = ref (Array.make 32 0)
  and progpos = ref 0
  and cpool = ref StringMap.empty
  and cpoolpos = ref 0
  and numgroups = ref 1
  and numregs = ref 0 in
  (* Add a new instruction *)
  let emit_instr opc arg =
    if !progpos >= Array.length !prog then begin
      let newlen = ref (Array.length !prog) in
      while !progpos >= !newlen do newlen := !newlen * 2 done;
      let nprog = Array.make !newlen 0 in
      Array.blit !prog 0 nprog 0 (Array.length !prog);
      prog := nprog
    end;
    (!prog).(!progpos) <- (instr opc arg);
    incr progpos in
  (* Reserve an instruction slot and return its position *)
  let emit_hole () =
    let p = !progpos in incr progpos; p in
  (* Fill a reserved instruction slot with a GOTO or PUSHBACK instruction *)
  let patch_instr pos opc dest =
    (!prog).(pos) <- (instr opc (displ dest pos)) in
  (* Return the cpool index for the given string, adding it if not
     already there *)
  let cpool_index s =
    try
      StringMap.find s !cpool
    with Not_found ->
      let p = !cpoolpos in
      cpool := StringMap.add s p !cpool;
      incr cpoolpos;
      p in
  (* Allocate fresh register if regexp is nullable *)
  let allocate_register_if_nullable r =
    if is_nullable r then begin
      let n = !numregs in
      if n >= 64 then failwith "too many r* or r+ where r is nullable";
      incr numregs;
      n
    end else
      -1 in
  (* Main recursive compilation function *)
  let rec emit_code = function
    Char c ->
      if fold_case then
        emit_instr op_CHARNORM (Char.code (Char.lowercase c))
      else
        emit_instr op_CHAR (Char.code c)
  | String s ->
      begin match String.length s with
        0 -> ()
      | 1 ->
        if fold_case then
          emit_instr op_CHARNORM (Char.code (Char.lowercase s.[0]))
        else
          emit_instr op_CHAR (Char.code s.[0])
      | _ ->
        try
          (* null characters are not accepted by the STRING* instructions;
             if one is found, split string at null character *)
          let i = String.index s '\000' in
          emit_code (String (string_before s i));
          emit_instr op_CHAR 0;
          emit_code (String (string_after s (i+1)))
        with Not_found ->
          if fold_case then
            emit_instr op_STRINGNORM (cpool_index (String.lowercase s))
          else
            emit_instr op_STRING (cpool_index s)
      end
  | CharClass(cl, compl) ->
      let cl1 = if fold_case then Charset.fold_case cl else cl in
      let cl2 = if compl then Charset.complement cl1 else cl1 in
      emit_instr op_CHARCLASS (cpool_index (Bytes.to_string cl2))
  | Seq rl ->
      emit_seq_code rl
  | Alt(r1, r2) ->
      (*      PUSHBACK lbl1
              <match r1>
              GOTO lbl2
        lbl1: <match r2>
        lbl2: ... *)
      let pos_pushback = emit_hole() in
      emit_code r1;
      let pos_goto_end = emit_hole() in
      let lbl1 = !progpos in
      emit_code r2;
      let lbl2 = !progpos in
      patch_instr pos_pushback op_PUSHBACK lbl1;
      patch_instr pos_goto_end op_GOTO lbl2
  | Star r ->
      (* Implement longest match semantics for compatibility with old Str *)
      (* General translation:
           lbl1: PUSHBACK lbl2
                 SETMARK regno
                 <match r>
                 CHECKPROGRESS regno
                 GOTO lbl1
           lbl2:
         If r cannot match the empty string, code can be simplified:
           lbl1: PUSHBACK lbl2
                 <match r>
                 GOTO lbl1
           lbl2:
        *)
      let regno = allocate_register_if_nullable r in
      let lbl1 = emit_hole() in
      if regno >= 0 then emit_instr op_SETMARK regno;
      emit_code r;
      if regno >= 0 then emit_instr op_CHECKPROGRESS regno;
      emit_instr op_GOTO (displ lbl1 !progpos);
      let lbl2 = !progpos in
      patch_instr lbl1 op_PUSHBACK lbl2
  | Plus r ->
      (* Implement longest match semantics for compatibility with old Str *)
      (* General translation:
           lbl1: <match r>
                 CHECKPROGRESS regno
                 PUSHBACK lbl2
                 SETMARK regno
                 GOTO lbl1
           lbl2:
         If r cannot match the empty string, code can be simplified:
           lbl1: <match r>
                 PUSHBACK lbl2
                 GOTO_PLUS lbl1
           lbl2:
      *)
      let regno = allocate_register_if_nullable r in
      let lbl1 = !progpos in
      emit_code r;
      if regno >= 0 then emit_instr op_CHECKPROGRESS regno;
      let pos_pushback = emit_hole() in
      if regno >= 0 then emit_instr op_SETMARK regno;
      emit_instr op_GOTO (displ lbl1 !progpos);
      let lbl2 = !progpos in
      patch_instr pos_pushback op_PUSHBACK lbl2
  | Option r ->
      (* Implement longest match semantics for compatibility with old Str *)
      (*      PUSHBACK lbl
              <match r>
         lbl:
      *)
      let pos_pushback = emit_hole() in
      emit_code r;
      let lbl = !progpos in
      patch_instr pos_pushback op_PUSHBACK lbl
  | Group(n, r) ->
      if n >= 32 then failwith "too many \\(...\\) groups";
      emit_instr op_BEGGROUP n;
      emit_code r;
      emit_instr op_ENDGROUP n;
      numgroups := max !numgroups (n+1)
  | Refgroup n ->
      emit_instr op_REFGROUP n
  | Bol ->
      emit_instr op_BOL 0
  | Eol ->
      emit_instr op_EOL 0
  | Wordboundary ->
      emit_instr op_WORDBOUNDARY 0

  and emit_seq_code = function
    [] -> ()
  | Star(Char _ | CharClass _ as r) :: rl
    when disjoint_modulo_case (first r) (first_seq rl) ->
      emit_instr op_SIMPLESTAR (cpool_index (charclass_of_regexp fold_case r));
      emit_seq_code rl
  | Plus(Char _ | CharClass _ as r) :: rl
    when disjoint_modulo_case (first r) (first_seq rl) ->
      emit_instr op_SIMPLEPLUS (cpool_index (charclass_of_regexp fold_case r));
      emit_seq_code rl
  | Option(Char _ | CharClass _ as r) :: rl
    when disjoint_modulo_case (first r) (first_seq rl) ->
      emit_instr op_SIMPLEOPT (cpool_index (charclass_of_regexp fold_case r));
      emit_seq_code rl
  | r :: rl ->
      emit_code r;
      emit_seq_code rl

  and disjoint_modulo_case c1 c2 =
    if fold_case
    then Charset.disjoint (Charset.fold_case c1) (Charset.fold_case c2)
    else Charset.disjoint c1 c2
  in

  emit_code re;
  emit_instr op_ACCEPT 0;
  let start = first re in
  let start' = if fold_case then Charset.fold_case start else start in
  let start_pos =
    if start = Charset.full
    then -1
    else cpool_index (Bytes.to_string (Charset.expand start')) in
  let constantpool = Array.make !cpoolpos "" in
  StringMap.iter (fun str idx -> constantpool.(idx) <- str) !cpool;
  { prog = Array.sub !prog 0 !progpos;
    cpool = constantpool;
    normtable = if fold_case then fold_case_table else "";
    numgroups = !numgroups;
    numregisters = !numregs;
    startchars = start_pos }

(** Parsing of a regular expression *)

(* Efficient buffering of sequences *)

module SeqBuffer = struct

  type t = { sb_chars: Buffer.t; mutable sb_next: re_syntax list }

  let create() = { sb_chars = Buffer.create 16; sb_next = [] }

  let flush buf =
    let s = Buffer.contents buf.sb_chars in
    Buffer.clear buf.sb_chars;
    match String.length s with
      0 -> ()
    | 1 -> buf.sb_next <- Char s.[0] :: buf.sb_next
    | _ -> buf.sb_next <- String s :: buf.sb_next

  let add buf re =
    match re with
      Char c -> Buffer.add_char buf.sb_chars c
    | _ -> flush buf; buf.sb_next <- re :: buf.sb_next

  let extract buf =
    flush buf; Seq(List.rev buf.sb_next)

end

(* The character class corresponding to `.' *)

let dotclass = Charset.complement (Charset.singleton '\n')

(* Parse a regular expression *)

let parse s =
  let len = String.length s in
  let group_counter = ref 1 in

  let rec regexp0 i =
    let (r, j) = regexp1 i in
    regexp0cont r j
  and regexp0cont r1 i =
    if i + 2 <= len && s.[i] = '\\' && s.[i+1] = '|' then
      let (r2, j) = regexp1 (i+2) in
      regexp0cont (Alt(r1, r2)) j
    else
      (r1, i)
  and regexp1 i =
    regexp1cont (SeqBuffer.create()) i
  and regexp1cont sb i =
    if i >= len
    || i + 2 <= len && s.[i] = '\\' && (let c = s.[i+1] in c = '|' || c = ')')
    then
      (SeqBuffer.extract sb, i)
    else
      let (r, j) = regexp2 i in
      SeqBuffer.add sb r;
      regexp1cont sb j
  and regexp2 i =
    let (r, j) = regexp3 i in
    regexp2cont r j
  and regexp2cont r i =
    if i >= len then (r, i) else
      match s.[i] with
        '?' -> regexp2cont (Option r) (i+1)
      | '*' -> regexp2cont (Star r) (i+1)
      | '+' -> regexp2cont (Plus r) (i+1)
      |  _  -> (r, i)
  and regexp3 i =
    match s.[i] with
      '\\' -> regexpbackslash (i+1)
    | '['  -> let (c, compl, j) = regexpclass0 (i+1) in
              (CharClass(c, compl), j)
    | '^'  -> (Bol, i+1)
    | '$'  -> (Eol, i+1)
    | '.'  -> (CharClass(dotclass, false), i+1)
    | c    -> (Char c, i+1)
  and regexpbackslash i =
    if i >= len then (Char '\\', i) else
      match s.[i] with
        '|' | ')' ->
          assert false
      | '(' ->
          let group_no = !group_counter in
          if group_no < 32 then incr group_counter;
          let (r, j) = regexp0 (i+1) in
          if j + 1 < len && s.[j] = '\\' && s.[j+1] = ')' then
            if group_no < 32
            then (Group(group_no, r), j + 2)
            else (r, j + 2)
          else
            failwith "\\( group not closed by \\)"
      | '1' .. '9' as c ->
          (Refgroup(Char.code c - 48), i + 1)
      | 'b' ->
          (Wordboundary, i + 1)
      | c ->
          (Char c, i + 1)
  and regexpclass0 i =
    if i < len && s.[i] = '^'
    then let (c, j) = regexpclass1 (i+1) in (c, true, j)
    else let (c, j) = regexpclass1 i in (c, false, j)
  and regexpclass1 i =
    let c = Charset.make_empty() in
    let j = regexpclass2 c i i in
    (c, j)
  and regexpclass2 c start i =
    if i >= len then failwith "[ class not closed by ]";
    if s.[i] = ']' && i > start then i+1 else begin
      let c1 = s.[i] in
      if i+2 < len && s.[i+1] = '-' && s.[i+2] <> ']' then begin
        let c2 = s.[i+2] in
        Charset.add_range c c1 c2;
        regexpclass2 c start (i+3)
      end else begin
        Charset.add c c1;
        regexpclass2 c start (i+1)
      end
    end in

  let (r, j) = regexp0 0 in
  if j = len then r else failwith "spurious \\) in regular expression"

(** Parsing and compilation *)

let regexp e = compile false (parse e)

let regexp_case_fold e = compile true (parse e)

let quote s =
  let len = String.length s in
  let buf = Bytes.create (2 * len) in
  let pos = ref 0 in
  for i = 0 to len - 1 do
    match s.[i] with
      '[' | ']' | '*' | '.' | '\\' | '?' | '+' | '^' | '$' as c ->
        Bytes.set buf !pos '\\';
        Bytes.set buf (!pos + 1) c;
        pos := !pos + 2
    | c ->
        Bytes.set buf !pos c;
        pos := !pos + 1
  done;
  Bytes.sub_string buf 0 !pos

let regexp_string s = compile false (String s)

let regexp_string_case_fold s = compile true (String s)

(** Matching functions **)

external re_string_match: regexp -> string -> int -> int array
     = "re_string_match"
external re_partial_match: regexp -> string -> int -> int array
     = "re_partial_match"
external re_search_forward: regexp -> string -> int -> int array
     = "re_search_forward"
external re_search_backward: regexp -> string -> int -> int array
     = "re_search_backward"

let last_search_result = ref [||]

let string_match re s pos =
  let res = re_string_match re s pos in
  last_search_result := res;
  Array.length res > 0

let string_partial_match re s pos =
  let res = re_partial_match re s pos in
  last_search_result := res;
  Array.length res > 0

let search_forward re s pos =
  let res = re_search_forward re s pos in
  last_search_result := res;
  if Array.length res = 0 then raise Not_found else res.(0)

let search_backward re s pos =
  let res = re_search_backward re s pos in
  last_search_result := res;
  if Array.length res = 0 then raise Not_found else res.(0)

let group_beginning n =
  let n2 = n + n in
  if n < 0 || n2 >= Array.length !last_search_result then
    invalid_arg "Str.group_beginning"
  else
    let pos = !last_search_result.(n2) in
    if pos = -1 then raise Not_found else pos

let group_end n =
  let n2 = n + n in
  if n < 0 || n2 >= Array.length !last_search_result then
    invalid_arg "Str.group_end"
  else
    let pos = !last_search_result.(n2 + 1) in
    if pos = -1 then raise Not_found else pos

let matched_group n txt =
  let n2 = n + n in
  if n < 0 || n2 >= Array.length !last_search_result then
    invalid_arg "Str.matched_group"
  else
    let b = !last_search_result.(n2)
    and e = !last_search_result.(n2 + 1) in
    if b = -1 then raise Not_found else String.sub txt b (e - b)

let match_beginning () = group_beginning 0
and match_end () = group_end 0
and matched_string txt = matched_group 0 txt

(** Replacement **)

external re_replacement_text: string -> int array -> string -> string
    = "re_replacement_text"

let replace_matched repl matched =
  re_replacement_text repl !last_search_result matched

let substitute_first expr repl_fun text =
  try
    let pos = search_forward expr text 0 in
    String.concat "" [string_before text pos;
                      repl_fun text;
                      string_after text (match_end())]
  with Not_found ->
    text

let opt_search_forward re s pos =
  try Some(search_forward re s pos) with Not_found -> None

let global_substitute expr repl_fun text =
  let rec replace accu start last_was_empty =
    let startpos = if last_was_empty then start + 1 else start in
    if startpos > String.length text then
      string_after text start :: accu
    else
      match opt_search_forward expr text startpos with
      | None ->
          string_after text start :: accu
      | Some pos ->
          let end_pos = match_end() in
          let repl_text = repl_fun text in
          replace (repl_text :: String.sub text start (pos-start) :: accu)
                  end_pos (end_pos = pos)
  in
    String.concat "" (List.rev (replace [] 0 false))

let global_replace expr repl text =
  global_substitute expr (replace_matched repl) text
and replace_first expr repl text =
  substitute_first expr (replace_matched repl) text

(** Splitting *)

let opt_search_forward_progress expr text start =
  match opt_search_forward expr text start with
  | None -> None
  | Some pos ->
      if match_end() > start then
        Some pos
      else if start < String.length text then
        opt_search_forward expr text (start + 1)
      else None

let bounded_split expr text num =
  let start =
    if string_match expr text 0 then match_end() else 0 in
  let rec split accu start n =
    if start >= String.length text then accu else
    if n = 1 then string_after text start :: accu else
      match opt_search_forward_progress expr text start with
      | None ->
          string_after text start :: accu
      | Some pos ->
          split (String.sub text start (pos-start) :: accu)
                (match_end()) (n-1)
  in
    List.rev (split [] start num)

let split expr text = bounded_split expr text 0

let bounded_split_delim expr text num =
  let rec split accu start n =
    if start > String.length text then accu else
    if n = 1 then string_after text start :: accu else
      match opt_search_forward_progress expr text start with
      | None ->
          string_after text start :: accu
      | Some pos ->
          split (String.sub text start (pos-start) :: accu)
                (match_end()) (n-1)
  in
    if text = "" then [] else List.rev (split [] 0 num)

let split_delim expr text = bounded_split_delim expr text 0

type split_result = Text of string | Delim of string

let bounded_full_split expr text num =
  let rec split accu start n =
    if start >= String.length text then accu else
    if n = 1 then Text(string_after text start) :: accu else
      match opt_search_forward_progress expr text start with
      | None ->
          Text(string_after text start) :: accu
      | Some pos ->
          let s = matched_string text in
          if pos > start then
            split (Delim(s) :: Text(String.sub text start (pos-start)) :: accu)
                  (match_end()) (n-1)
          else
            split (Delim(s) :: accu)
                  (match_end()) (n-1)
  in
    List.rev (split [] 0 num)

let full_split expr text = bounded_full_split expr text 0
