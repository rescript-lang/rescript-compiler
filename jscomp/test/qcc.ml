let dbg = ref true
and inch = ref stdin

type tok = Op of string | ILit of int | SLit of int * string | Sym of int

let bufferize f =
  let buf = ref None in
  ( (fun () ->
      match !buf with
      | Some x ->
          buf := None ;
          x
      | None -> f ())
  , fun x ->
      assert (!buf = None) ;
      buf := Some x )

let getch, ungetch = bufferize (fun () -> input_char !inch)

let peekch () =
  let ch = getch () in
  ungetch ch ; ch

let addsym, symstr, symitr =
  let symtab = Array.make 100 "" and syms = ref 0 in
  let rec find s n =
    if n >= !syms then (incr syms ; n)
    else if symtab.(n) = s then n
    else find s (n + 1) in
  ( (fun s ->
      let sid = find s 0 in
      symtab.(sid) <- s ; sid)
  , (fun n ->
      assert (n < !syms) ;
      symtab.(n))
  , fun f ->
      for i = 0 to !syms - 1 do
        f i symtab.(i)
      done )

let glo = Bytes.make 0x1000 '\x00'
and gpos = ref 0

let base = 0x400000
and textoff = 0xe8

let next =
  let s = Bytes.create 100 in
  let getq () =
    match getch () with
    | '\\' when peekch () = 'n' ->
        ignore (getch ()) ;
        '\n'
    | c -> c in
  let isid = function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false in
  let rec id n ch =
    Bytes.set s n ch ;
    if isid (peekch ()) then id (n + 1) (getch ())
    else Sym (addsym (Bytes.to_string (Bytes.sub s 0 (n + 1)))) in
  let rec ilit n =
    match peekch () with
    | '0' .. '9' -> ilit ((10 * n) + Char.code (getch ()) - 48)
    | _ -> ILit n in
  let rec slit b e =
    match peekch () with
    | '"' ->
        ignore (getch ()) ;
        gpos := (e + 8) land -8 ;
        SLit (b + textoff + base, Bytes.to_string (Bytes.sub glo b (e - b)))
    | _ ->
        Bytes.set glo e (getq ()) ;
        slit b (e + 1) in
  let longops = ["++"; "--"; "&&"; "||"; "=="; "<="; ">="; "!="; ">>"; "<<"] in
  let rec op ch = function
    | lop :: l ->
        if lop.[0] = ch && lop.[1] = peekch () then (
          ignore (getch ()) ;
          Op lop )
        else op ch l
    | [] -> Op (String.make 1 ch) in
  let cconst () =
    let ch = getq () in
    let qt = getch () in
    if qt <> '\'' then failwith "syntax error" else ILit (Char.code ch) in
  let rec skip () =
    match getch () with
    | '\t' | ' ' | '\r' | '\n' -> skip ()
    | '/' when peekch () = '*' -> com (ignore (getch ()))
    | ch -> ch
  and com () =
    match getch () with
    | '*' when peekch () = '/' -> skip (ignore (getch ()))
    | _ -> com () in
  fun () ->
    (* next token *)
    match try Some (skip ()) with End_of_file -> None with
    | Some ('0' .. '9' as c) -> ilit (Char.code c - 48)
    | Some '"' -> slit !gpos !gpos
    | Some '\'' -> cconst ()
    | Some c when isid c -> id 0 c
    | Some c -> op c longops
    | None -> Op "EOF!"

let next, unnext = bufferize next

let nextis t =
  let nt = next () in
  unnext nt ; t = nt

let obuf = Bytes.make 0x100000 '\x00'
and opos = ref 0

let rec out x =
  if x <> 0 then (
    out (x / 0x100) ;
    Bytes.set obuf !opos (Char.chr (x land 0xff)) ;
    incr opos )

let le n x =
  for i = 0 to (n / 8) - 1 do
    let byte = (x lsr (i * 8)) land 0xff in
    Bytes.set obuf !opos (Char.chr byte) ;
    incr opos
  done

let get32 l =
  Char.code (Bytes.get obuf l)
  + (Char.code (Bytes.get obuf (l + 1)) * 0x100)
  + (Char.code (Bytes.get obuf (l + 2)) * 0x10000)
  + (Char.code (Bytes.get obuf (l + 3)) * 0x1000000)

let rec patch rel loc n =
  assert (n < 0x100000000) ;
  if loc <> 0 then (
    let i = !opos in
    let loc' = get32 loc in
    let x = if rel then n - (loc + 4) else n in
    if !dbg then Printf.eprintf "patching at %d to %d (n=%d)\n" loc x n ;
    opos := loc ;
    le 32 x ;
    patch rel loc' n ;
    opos := i )

let load r n =
  out (0xb8 + r) ;
  le 32 n

let cmp n =
  load 0 0 ;
  out (0x0f92c0 + (n lsl 8))

let test n l =
  out 0x4885c0 ;
  out (0x0f84 + n) ;
  let loc = !opos in
  le 32 l ; loc

let align = ref 0

let push r =
  incr align ;
  if r < 8 then out (0x50 + r) else out (0x4150 + r - 8)

and pop r =
  decr align ;
  if r < 8 then out (0x58 + r) else out (0x4158 + r - 8)

type lvpatch = Mov of int | Del of int
type lvty = Int | Chr

let lval = ref (Mov 0, Int)

let patchlval () =
  match fst !lval with
  | Mov n -> Bytes.set obuf (!opos - n) '\x8d'
  | Del n -> opos := !opos - n

let read = function
  | Int ->
      out 0x488b ;
      le 8 0 ;
      lval := (Del 3, Int)
  | Chr ->
      out 0x480fb6 ;
      le 8 0 ;
      lval := (Del 4, Chr)

type globref = {loc: int; va: int}

let globs = Array.make 100 {loc= 0; va= -1}

let lvls =
  [ ("*", 0); ("/", 0); ("%", 0); ("+", 1); ("-", 1); ("<<", 2); (">>", 2)
  ; ("<", 3); ("<=", 3); (">", 3); (">=", 3); ("==", 4); ("!=", 4); ("&", 5)
  ; ("^", 6); ("|", 7); ("&&", 8); ("||", 9) ]

type ins = Bin of int list | Cmp of int

let inss =
  [ ("*", Bin [0x480fafc1])
  ; ("/", Bin [0x4891; 0x4899; 0x48f7f9])
  ; ("%", Bin [0x4891; 0x4899; 0x48f7f9; 0x4892])
  ; ("+", Bin [0x4801c8])
  ; ("-", Bin [0x4891; 0x4829c8])
  ; ("<<", Bin [0x4891; 0x48d3e0])
  ; (">>", Bin [0x4891; 0x48d3f8])
  ; ("<", Cmp 10); ("<=", Cmp 12); (">", Cmp 13); (">=", Cmp 11); ("==", Cmp 2)
  ; ("!=", Cmp 3)
  ; ("&", Bin [0x4821c8])
  ; ("^", Bin [0x4831c8])
  ; ("|", Bin [0x4809c8]) ]

let tokint = Sym (addsym "int")
and tokchar = Sym (addsym "char")
and tokret = Sym (addsym "return")
and tokif = Sym (addsym "if")
and tokelse = Sym (addsym "else")
and tokwhile = Sym (addsym "while")
and tokfor = Sym (addsym "for")
and tokbreak = Sym (addsym "break")

let rec binary stk lvl =
  if lvl = -1 then unary stk
  else
    let lvlof o =
      if not (List.mem_assoc o lvls) then -1 else List.assoc o lvls in
    let rec fold () =
      match next () with
      | Op o when lvlof o = lvl ->
          push 0 ;
          (* push %rax *)
          binary stk (lvl - 1) ;
          pop 1 ;
          (* pop %rcx *)
          ( match List.assoc o inss with
          | Bin ops -> List.iter out ops
          | Cmp c -> out 0x4839c1 ; cmp c ) ;
          fold ()
      | t -> unnext t in
    let rec foldtst loc =
      match next () with
      | Op o when lvlof o = lvl ->
          let loc' = test (lvl - 8) loc in
          binary stk (lvl - 1) ;
          foldtst loc'
      | t -> unnext t ; loc in
    binary stk (lvl - 1) ;
    if lvl < 8 then fold ()
    else
      let loc = foldtst 0 in
      patch true loc !opos

and unary stk =
  match next () with
  | Sym i ->
      if List.mem_assoc i stk then (
        let l = List.assoc i stk in
        assert (l > -256) ;
        out 0x488b45 ;
        out (l land 255) ;
        (* mov l(%rbp), %rax *)
        lval := (Mov 3, Int) )
      else (
        out 0x48b8 ;
        let g = globs.(i) and loc = !opos in
        le 64 g.loc ;
        (* mov $g.loc, %rax *)
        globs.(i) <- {g with loc} ;
        read Int ) ;
      postfix stk
  | SLit (l, _) -> out 0x48b8 ; le 64 l (* mov $l, %rax *)
  | ILit i -> load 0 i (* mov $i, %eax *)
  | Op "(" ->
      expr stk ;
      ignore (next ()) ;
      (* XXX use expect here *)
      postfix stk
  | Op "*" ->
      let ty, i =
        ignore (next ()) ;
        match next () with
        | t when t = tokint -> if next () = Op "*" then (Int, 1) else (Int, 5)
        | t when t = tokchar -> (Chr, 2)
        | _ -> failwith "[cast] expected" in
      for k = 1 to i do
        ignore (next ())
      done ;
      unary stk ;
      read ty
  | Op "&" -> unary stk ; patchlval ()
  | Op o ->
      let unops =
        [("+", 0); ("-", 0x48f7d8); ("~", 0x48f7d0); ("!", 0x4885c0)] in
      unary stk ;
      if not (List.mem_assoc o unops) then
        failwith (Printf.sprintf "unknown operator %s" o) ;
      out (List.assoc o unops) ;
      if o = "!" then cmp 2

(* setz %al *)
and postfix stk =
  match next () with
  | Op (("++" | "--") as op) ->
      let ol =
        [ (("++", Int), 0x48ff01)
        ; (* incq (%rcx) *) (("--", Int), 0x48ff09)
        ; (* decq (%rcx) *) (("++", Chr), 0xfe01)
        ; (* incb (%rcx) *) (("--", Chr), 0xfe09)
          (* decb (%rcx) *)
         ] in
      patchlval () ;
      out 0x4889c1 ;
      (* mov %rax, %rcx *)
      read (snd !lval) ;
      out (List.assoc (op, snd !lval) ol)
  | Op "(" ->
      let regs = [7; 6; 2; 1; 8; 9] in
      let rec emitargs l rl =
        if nextis (Op ")") then (
          ignore (next ()) ;
          List.iter pop l )
        else (
          expr stk ;
          push 0 ;
          if nextis (Op ",") then ignore (next ()) ;
          emitargs (List.hd rl :: l) (List.tl rl) ) in
      patchlval () ;
      push 0 ;
      (* push %rax *)
      emitargs [] regs ;
      pop 0 ;
      (* pop %rax *)
      if !align mod 2 <> 0 then out 0x4883ec08 ;
      (* sub 8, %rsp *)
      out 0xffd0 ;
      (* call *%rax *)
      if !align mod 2 <> 0 then out 0x4883c408
      (* add 8, %rsp *)
  | t -> unnext t

and expr stk =
  let rec eqexpr () =
    match next () with
    | Op "=" ->
        patchlval () ;
        let ty = snd !lval in
        push 0 ;
        (* push %rax *)
        expr stk ;
        pop 1 ;
        (* pop %rcx *)
        if ty = Int then out 0x488901 (* mov %rax, (%rcx) *) else out 0x8801 ;
        (* mov %al, (%rcx) *)
        eqexpr ()
    | t -> unnext t in
  binary stk 10 ; eqexpr ()

let rec decl g n stk =
  match next () with
  | t when t = tokint ->
      let top = match stk with (_, i) :: _ -> i | _ -> 0 in
      let rec vars n stk =
        while nextis (Op "*") do
          ignore (next ())
        done ;
        if nextis (Op ";") then (n, stk)
        else
          match next () with
          | Sym s ->
              let n' = n + 1 in
              let stk' =
                if g then (
                  let glo = globs.(s) in
                  if glo.va >= 0 then failwith "symbol defined twice" ;
                  let va = !gpos + textoff + base in
                  globs.(s) <- {glo with va} ;
                  gpos := !gpos + 8 ;
                  stk )
                else (s, top - (8 * n')) :: stk in
              if not (nextis (Op ",")) then (n', stk')
              else (
                ignore (next ()) ;
                vars n' stk' )
          | _ -> failwith "[var] expected in [decl]" in
      let m, stk' = vars 0 stk in
      ignore (next ()) ;
      if !dbg then Printf.eprintf "end of decl (%d vars)\n" n ;
      decl g (n + m) stk'
  | t ->
      unnext t ;
      if (not g) && n <> 0 then (
        assert (n * 8 < 256) ;
        out 0x4883ec ;
        out (n * 8) ;
        (* sub $n*8, %rsp *)
        align := !align + n ) ;
      if !dbg && not g then prerr_endline "end of blk decls" ;
      (n, stk)

let retl = ref 0

let rec stmt brk stk =
  let pexpr stk =
    ignore (next ()) ;
    (* XXX expect ( *)
    expr stk ;
    ignore (next ()) in
  (* XXX expect ) *)
  match next () with
  | t when t = tokif ->
      pexpr stk ;
      let loc = test 0 0 in
      stmt brk stk ;
      let loc =
        if not (nextis tokelse) then loc
        else (
          ignore (next ()) ;
          out 0xe9 ;
          (* jmp *)
          let l = !opos in
          le 32 0 ; patch true loc !opos ; stmt brk stk ; l ) in
      patch true loc !opos
  | t when t = tokwhile || t = tokfor ->
      let bl, ba = (ref 0, !align) in
      let bdy, itr =
        if t = tokwhile then (
          let loc = !opos in
          pexpr stk ;
          bl := test 0 0 ;
          (0, loc) )
        else (
          ignore (next ()) ;
          if not (nextis (Op ";")) then expr stk ;
          ignore (next ()) ;
          let top = !opos in
          if not (nextis (Op ";")) then (
            expr stk ;
            bl := test 0 0 )
          else bl := 0 ;
          ignore (next ()) ;
          out 0xe9 ;
          let bdy = !opos in
          le 32 0 ;
          let itr = !opos in
          expr stk ;
          ignore (next ()) ;
          out 0xe9 ;
          le 32 (top - !opos - 4) ;
          (bdy, itr) ) in
      patch true bdy !opos ;
      stmt (bl, ba) stk ;
      out 0xe9 ;
      (* jmp *)
      le 32 (itr - !opos - 4) ;
      patch true !bl !opos
  | t when t = tokret ->
      if not (nextis (Op ";")) then expr stk ;
      ignore (next ()) ;
      (* XXX expect here *)
      out 0xe9 ;
      (* jmp *)
      let loc = !opos in
      le 32 !retl ;
      retl := loc
  | t when t = tokbreak ->
      ignore (next ()) ;
      let brkl, brka = brk in
      let n = !align - brka in
      assert (n >= 0) ;
      if n <> 0 then (
        out 0x4883c4 ;
        (* add $n*8, %rsp *)
        out (n * 8) ) ;
      out 0xe9 ;
      let loc = !opos in
      le 32 !brkl ;
      brkl := loc
  | Op "{" -> block brk stk
  | Op ";" -> ()
  | t ->
      unnext t ;
      expr stk ;
      ignore (next ())

(* use expect XXX *)
and block brk stk =
  let n, stk' = decl false 0 stk in
  while not (nextis (Op "}")) do
    stmt brk stk'
  done ;
  ignore (next ()) ;
  if n <> 0 then (
    out 0x4883c4 ;
    out (n * 8) ;
    (* add $n*8, %rsp *)
    align := !align - n )

let rec top () =
  if not (nextis (Op "EOF!")) then
    if nextis tokint then (
      ignore (decl true 0 []) ;
      top () )
    else
      match next () with
      | Sym f ->
          let g = globs.(f) in
          if g.va >= 0 then failwith "symbol defined twice" ;
          globs.(f) <- {g with va= !opos} ;
          let regs = [7; 6; 2; 1; 8; 9] in
          let rec emitargs regs n stk =
            match next () with
            | Sym i ->
                let r = List.hd regs in
                push r ;
                if nextis (Op ",") then ignore (next ()) ;
                let stk' = (i, -n * 8) :: stk in
                emitargs (List.tl regs) (n + 1) stk'
            | Op ")" -> stk
            | _ -> failwith "[var] or ) expected" in
          ignore (next ()) ;
          (* expect here XXX *)
          align := 0 ;
          out 0x55 ;
          (* push %rbp NO push! *)
          out 0x4889e5 ;
          (* mov %rsp, %rbp *)
          let stk = emitargs regs 1 [] in
          while next () <> Op "{" do
            ()
          done ;
          retl := 0 ;
          block (ref 0, 0) stk ;
          patch true !retl !opos ;
          out 0xc9c3 ;
          (* leave; ret *)
          if !dbg then Printf.eprintf "done with function %s\n" (symstr f) ;
          top ()
      | _ -> failwith "[decl] or [fun] expected"

let elfhdr =
  Bytes.of_string
  @@ String.concat ""
       [ "\x7fELF\x02\x01\x01\x00"; (* e_ident, 64bits, little endian *)
         "\x00\x00\x00\x00\x00\x00\x00\x00"; "\x02\x00"; (* e_type, ET_EXEC *)
         "\x3e\x00"; (* e_machine, EM_X86_64 *) "\x01\x00\x00\x00"
       ; (* e_version, EV_CURRENT*) "\x00\x00\x00\x00\x00\x00\x00\x00"
       ; (* e_entry *) "\x40\x00\x00\x00\x00\x00\x00\x00"; (* e_phoff *)
         "\x00\x00\x00\x00\x00\x00\x00\x00"; (* e_shoff *) "\x00\x00\x00\x00"
       ; (* e_flags *) "\x40\x00"; (* e_hsize *) "\x38\x00"; (* e_phentsize *)
         "\x03\x00"; (* e_phnum *) "\x40\x00"; (* e_shentsize *) "\x00\x00"
       ; (* e_shnum *) "\x00\x00"
         (* e_shstrndx *)
        ]

let elfphdr ty off sz align =
  le 32 ty ;
  (* p_type *)
  le 32 7 ;
  (* p_flags, RWX *)
  le 64 off ;
  (* p_offset *)
  le 64 (off + base) ;
  (* p_vaddr *)
  le 64 (off + base) ;
  (* p_paddr *)
  le 64 sz ;
  (* p_filesz *)
  le 64 sz ;
  (* p_memsz *)
  le 64 align

(* p_align *)

let elfgen outf =
  let entry = !opos in
  let main = addsym "main" in
  let gmain = globs.(main) in
  out 0x488b3c24 ;
  (* mov (%rsp), %rdi *)
  out 0x488d742408 ;
  (* lea 8(%rsp), %rsi *)
  out 0x48b8 ;
  le 64 gmain.loc ;
  (* mov main, %rax *)
  globs.(main) <- {gmain with loc= !opos - 8} ;
  out 0xffd0 ;
  (* call *%rax *)
  out 0x89c7 ;
  (* mov %eax, %edi *)
  load 0 0x3c ;
  out 0x0f05 ;
  (* syscall *)
  let off = textoff + !gpos in
  let itr f =
    symitr (fun i s ->
        let g = globs.(i) in
        if g.va < 0 && g.loc <> 0 then f s (String.length s) g.loc) in
  let va x = x + off + base in
  let patchloc i _ =
    let g = globs.(i) in
    if g.va >= 0 && g.va < base then patch false g.loc (va g.va)
    else if g.va >= 0 then patch false g.loc g.va in
  symitr patchloc ;
  let strtab = !opos in
  incr opos ;
  (* initial 0 *)
  let dl = "/lib64/ld-linux-x86-64.so.2\x00libc.so.6" and dllen = 27 + 1 + 9 in
  String.blit dl 0 obuf !opos dllen ;
  opos := !opos + dllen + 1 ;
  itr (fun s sl _ ->
      String.blit s 0 obuf !opos sl ;
      opos := !opos + sl + 1) ;
  opos := (!opos + 7) land -8 ;
  let symtab = !opos and n = ref (dllen + 2) in
  opos := !opos + 24 ;
  (* first is reserved *)
  itr (fun _ sl _ ->
      le 32 !n ;
      (* st_name *)
      le 32 0x10 ;
      (* global | notype *)
      le 64 0 ;
      (* st_value *)
      le 64 0 ;
      (* st_size *)
      n := !n + sl + 1) ;
  let rel = !opos and n = ref 1 in
  itr (fun _ _ l ->
      let rec genrel l =
        if l <> 0 then (
          le 64 (va l) ;
          le 64 (1 + (!n lsl 32)) ;
          (* R_X86_64_64 *)
          le 64 0 ;
          genrel (get32 l) ) in
      genrel l ; incr n) ;
  let hash = !opos in
  let n = ((rel - symtab) / 24) - 1 in
  le 32 1 ;
  (* nbucket *)
  le 32 (n + 1) ;
  (* nchain *)
  le 32 (if n > 0 then 1 else 0) ;
  for i = 1 to n do
    le 32 i
  done ;
  le 32 0 ;
  let dyn = !opos in
  List.iter (le 64)
    [ 1; 29; (* DT_NEEDED libc.so.6 *) 4; va hash; (* DT_HASH *) 5; va strtab
    ; (* DT_STRTAB *) 6; va symtab; (* DT_SYMTAB *) 7; va rel; (* DT_RELA *) 8
    ; hash - rel; (* DT_RELASZ *) 9; 24; (* DT_RELAENT *) 10; symtab - strtab
    ; (* DT_STRSZ *) 11; 24; (* DT_SYMENT *) 0
      (* 0; *)
      (* DT_NULL *)
     ] ;
  let tend = !opos in
  Bytes.blit obuf 0 obuf off tend ;
  Bytes.blit glo 0 obuf textoff !gpos ;
  Bytes.blit elfhdr 0 obuf 0 64 ;
  opos := 64 ;
  elfphdr 3 (strtab + 1 + off) 28 1 ;
  (* PT_INTERP *)
  elfphdr 1 0 (tend + off) 0x200000 ;
  (* PT_LOAD *)
  elfphdr 2 (dyn + off) (tend - dyn) 8 ;
  (* PT_DYNAMIC *)
  assert (!opos = textoff) ;
  patch false 24 (va entry) ;
  output_bytes outf (Bytes.sub obuf 0 (tend + off))

let main () =
  let doone c stk =
    opos := 0 ;
    c stk ;
    print_bytes (Bytes.sub obuf 0 !opos) in
  let ppsym = function
    | Op s -> Printf.printf "Operator '%s'\n" s
    | ILit i -> Printf.printf "Int literal %d\n" i
    | SLit (_, s) -> Printf.printf "Str literal %S\n" s
    | Sym i -> Printf.printf "Symbol '%s' (%d)\n" (symstr i) i in
  let rec pptoks () =
    match next () with
    | Op "EOF!" -> Printf.printf "End of input stream\n"
    | tok -> ppsym tok ; pptoks () in
  match if Array.length Sys.argv < 2 then "-blk" else Sys.argv.(1) with
  | "-lex" -> pptoks ()
  | "-blk" -> doone (block (ref 0, 0)) []
  | f ->
      let oc = open_out "a.out" in
      inch := open_in_bin f ;
      top () ;
      elfgen oc ;
      close_out oc

let _ = main ()

(* local variables: *)
(* compile-command: "ocamlc.opt -safe-string qcc.ml -o qcc" *)
(* end: *)
