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

(* Translation from closed lambda to C-- *)

open Misc
open Arch
open Asttypes
open Primitive
open Types
open Lambda
open Clambda
open Cmm
open Cmx_format

(* Local binding of complex expressions *)

let bind name arg fn =
  match arg with
    Cvar _ | Cconst_int _ | Cconst_natint _ | Cconst_symbol _
  | Cconst_pointer _ | Cconst_natpointer _
  | Cconst_blockheader _ -> fn arg
  | _ -> let id = Ident.create name in Clet(id, arg, fn (Cvar id))

let bind_nonvar name arg fn =
  match arg with
    Cconst_int _ | Cconst_natint _ | Cconst_symbol _
  | Cconst_pointer _ | Cconst_natpointer _
  | Cconst_blockheader _ -> fn arg
  | _ -> let id = Ident.create name in Clet(id, arg, fn (Cvar id))

let caml_black = Nativeint.shift_left (Nativeint.of_int 3) 8
    (* cf. byterun/gc.h *)

(* Block headers. Meaning of the tag field: see stdlib/obj.ml *)

let floatarray_tag = Cconst_int Obj.double_array_tag

let block_header tag sz =
  Nativeint.add (Nativeint.shift_left (Nativeint.of_int sz) 10)
                (Nativeint.of_int tag)
(* Static data corresponding to "value"s must be marked black in case we are
   in no-naked-pointers mode.  See [caml_darken] and the code below that emits
   structured constants and static module definitions. *)
let black_block_header tag sz = Nativeint.logor (block_header tag sz) caml_black
let white_closure_header sz = block_header Obj.closure_tag sz
let black_closure_header sz = black_block_header Obj.closure_tag sz
let infix_header ofs = block_header Obj.infix_tag ofs
let float_header = block_header Obj.double_tag (size_float / size_addr)
let floatarray_header len =
      block_header Obj.double_array_tag (len * size_float / size_addr)
let string_header len =
      block_header Obj.string_tag ((len + size_addr) / size_addr)
let boxedint32_header = block_header Obj.custom_tag 2
let boxedint64_header = block_header Obj.custom_tag (1 + 8 / size_addr)
let boxedintnat_header = block_header Obj.custom_tag 2

let alloc_block_header tag sz = Cconst_blockheader(block_header tag sz)
let alloc_float_header = Cconst_blockheader(float_header)
let alloc_floatarray_header len = Cconst_blockheader(floatarray_header len)
let alloc_closure_header sz = Cconst_blockheader(white_closure_header sz)
let alloc_infix_header ofs = Cconst_blockheader(infix_header ofs)
let alloc_boxedint32_header = Cconst_blockheader(boxedint32_header)
let alloc_boxedint64_header = Cconst_blockheader(boxedint64_header)
let alloc_boxedintnat_header = Cconst_blockheader(boxedintnat_header)

(* Integers *)

let max_repr_int = max_int asr 1
let min_repr_int = min_int asr 1

let int_const n =
  if n <= max_repr_int && n >= min_repr_int
  then Cconst_int((n lsl 1) + 1)
  else Cconst_natint
          (Nativeint.add (Nativeint.shift_left (Nativeint.of_int n) 1) 1n)

let rec add_const c n =
  if n = 0 then c
  else match c with
  | Cconst_int x when no_overflow_add x n -> Cconst_int (x + n)
  | Cop(Csubi, [Cconst_int x; c]) when no_overflow_add n x ->
      Cop(Csubi, [Cconst_int (n + x); c])
  | Cop(Csubi, [c; Cconst_int x]) when no_overflow_sub n x ->
      add_const c (n - x)
  | c -> Cop(Caddi, [c; Cconst_int n])

let incr_int = function
    Cconst_int n when n < max_int -> Cconst_int(n+1)
  | Cop(Caddi, [c; Cconst_int n]) when n < max_int -> add_const c (n + 1)
  | c -> add_const c 1

let decr_int = function
    Cconst_int n when n > min_int -> Cconst_int(n-1)
  | Cop(Caddi, [c; Cconst_int n]) when n > min_int -> add_const c (n - 1)
  | c -> add_const c (-1)

let add_int c1 c2 =
  match (c1, c2) with
    (Cop(Caddi, [c1; Cconst_int n1]),
     Cop(Caddi, [c2; Cconst_int n2])) when no_overflow_add n1 n2 ->
      add_const (Cop(Caddi, [c1; c2])) (n1 + n2)
  | (Cop(Caddi, [c1; Cconst_int n1]), c2) ->
      add_const (Cop(Caddi, [c1; c2])) n1
  | (c1, Cop(Caddi, [c2; Cconst_int n2])) ->
      add_const (Cop(Caddi, [c1; c2])) n2
  | (Cconst_int _, _) ->
      Cop(Caddi, [c2; c1])
  | (_, _) ->
      Cop(Caddi, [c1; c2])

let sub_int c1 c2 =
  match (c1, c2) with
    (Cop(Caddi, [c1; Cconst_int n1]),
     Cop(Caddi, [c2; Cconst_int n2])) when no_overflow_sub n1 n2 ->
      add_const (Cop(Csubi, [c1; c2])) (n1 - n2)
  | (Cop(Caddi, [c1; Cconst_int n1]), c2) ->
      add_const (Cop(Csubi, [c1; c2])) n1
  | (c1, Cop(Caddi, [c2; Cconst_int n2])) when n2 <> min_int ->
      add_const (Cop(Csubi, [c1; c2])) (-n2)
  | (c1, Cconst_int n) when n <> min_int ->
      add_const c1 (-n)
  | (c1, c2) ->
      Cop(Csubi, [c1; c2])

let mul_int c1 c2 =
  match (c1, c2) with
    (c, Cconst_int 0) | (Cconst_int 0, c) ->
      Cconst_int 0
  | (c, Cconst_int 1) | (Cconst_int 1, c) ->
      c
  | (c, Cconst_int(-1)) | (Cconst_int(-1), c) ->
      sub_int (Cconst_int 0) c
  | (c, Cconst_int n) | (Cconst_int n, c) when n = 1 lsl Misc.log2 n->
      Cop(Clsl, [c; Cconst_int(Misc.log2 n)])
  | (c1, c2) ->
      Cop(Cmuli, [c1; c2])

let lsl_int c1 c2 =
  match (c1, c2) with
    (Cop(Clsl, [c; Cconst_int n1]), Cconst_int n2)
    when n1 > 0 && n2 > 0 && n1 + n2 < size_int * 8 ->
      Cop(Clsl, [c; Cconst_int (n1 + n2)])
  | (_, _) ->
      Cop(Clsl, [c1; c2])

let ignore_low_bit_int = function
    Cop(Caddi, [(Cop(Clsl, [_; Cconst_int n]) as c); Cconst_int 1]) when n > 0
      -> c
  | Cop(Cor, [c; Cconst_int 1]) -> c
  | c -> c

let lsr_int c1 c2 =
  match c2 with
    Cconst_int 0 ->
      c1
  | Cconst_int n when n > 0 ->
      Cop(Clsr, [ignore_low_bit_int c1; c2])
  | _ ->
      Cop(Clsr, [c1; c2])

let asr_int c1 c2 =
  match c2 with
    Cconst_int 0 ->
      c1
  | Cconst_int n when n > 0 ->
      Cop(Casr, [ignore_low_bit_int c1; c2])
  | _ ->
      Cop(Casr, [c1; c2])

let tag_int = function
    Cconst_int n ->
      int_const n
  | Cop(Casr, [c; Cconst_int n]) when n > 0 ->
      Cop(Cor, [asr_int c (Cconst_int (n - 1)); Cconst_int 1])
  | c ->
      incr_int (lsl_int c (Cconst_int 1))

let force_tag_int = function
    Cconst_int n ->
      int_const n
  | Cop(Casr, [c; Cconst_int n]) when n > 0 ->
      Cop(Cor, [asr_int c (Cconst_int (n - 1)); Cconst_int 1])
  | c ->
      Cop(Cor, [lsl_int c (Cconst_int 1); Cconst_int 1])

let untag_int = function
    Cconst_int n -> Cconst_int(n asr 1)
  | Cop(Caddi, [Cop(Clsl, [c; Cconst_int 1]); Cconst_int 1]) -> c
  | Cop(Cor, [Cop(Casr, [c; Cconst_int n]); Cconst_int 1])
    when n > 0 && n < size_int * 8 ->
      Cop(Casr, [c; Cconst_int (n+1)])
  | Cop(Cor, [Cop(Clsr, [c; Cconst_int n]); Cconst_int 1])
    when n > 0 && n < size_int * 8 ->
      Cop(Clsr, [c; Cconst_int (n+1)])
  | Cop(Cor, [c; Cconst_int 1]) -> Cop(Casr, [c; Cconst_int 1])
  | c -> Cop(Casr, [c; Cconst_int 1])

(* Turning integer divisions into multiply-high then shift.
   The [division_parameters] function is used in module Emit for
   those target platforms that support this optimization. *)

(* Unsigned comparison between native integers. *)

let ucompare x y = Nativeint.(compare (add x min_int) (add y min_int))

(* Unsigned division and modulus at type nativeint.
   Algorithm: Hacker's Delight section 9.3 *)

let udivmod n d = Nativeint.(
  if d < 0n then
    if ucompare n d < 0 then (0n, n) else (1n, sub n d)
  else begin
    let q = shift_left (div (shift_right_logical n 1) d) 1 in
    let r = sub n (mul q d) in
    if ucompare r d >= 0 then (succ q, sub r d) else (q, r)
  end)

(* Compute division parameters.
   Algorithm: Hacker's Delight chapter 10, fig 10-1. *)

let divimm_parameters d = Nativeint.(
  assert (d > 0n);
  let twopsm1 = min_int in (* 2^31 for 32-bit archs, 2^63 for 64-bit archs *)
  let nc = sub (pred twopsm1) (snd (udivmod twopsm1 d)) in
  let rec loop p (q1, r1) (q2, r2) =
    let p = p + 1 in
    let q1 = shift_left q1 1 and r1 = shift_left r1 1 in
    let (q1, r1) =
      if ucompare r1 nc >= 0 then (succ q1, sub r1 nc) else (q1, r1) in
    let q2 = shift_left q2 1 and r2 = shift_left r2 1 in
    let (q2, r2) =
      if ucompare r2 d >= 0 then (succ q2, sub r2 d) else (q2, r2) in
    let delta = sub d r2 in
    if ucompare q1 delta < 0 || (q1 = delta && r1 = 0n)
    then loop p (q1, r1) (q2, r2)
    else (succ q2, p - size)
  in loop (size - 1) (udivmod twopsm1 nc) (udivmod twopsm1 d))

(* The result [(m, p)] of [divimm_parameters d] satisfies the following
   inequality:

      2^(wordsize + p) < m * d <= 2^(wordsize + p) + 2^(p + 1)    (i)

   from which it follows that

      floor(n / d) = floor(n * m / 2^(wordsize+p))
                              if 0 <= n < 2^(wordsize-1)
      ceil(n / d) = floor(n * m / 2^(wordsize+p)) + 1
                              if -2^(wordsize-1) <= n < 0

   The correctness condition (i) above can be checked by the code below.
   It was exhaustively tested for values of d from 2 to 10^9 in the
   wordsize = 64 case.

let add2 (xh, xl) (yh, yl) =
  let zl = add xl yl and zh = add xh yh in
  ((if ucompare zl xl < 0 then succ zh else zh), zl)

let shl2 (xh, xl) n =
  assert (0 < n && n < size + size);
  if n < size
  then (logor (shift_left xh n) (shift_right_logical xl (size - n)),
        shift_left xl n)
  else (shift_left xl (n - size), 0n)

let mul2 x y =
  let halfsize = size / 2 in
  let halfmask = pred (shift_left 1n halfsize) in
  let xl = logand x halfmask and xh = shift_right_logical x halfsize in
  let yl = logand y halfmask and yh = shift_right_logical y halfsize in
  add2 (mul xh yh, 0n)
    (add2 (shl2 (0n, mul xl yh) halfsize)
       (add2 (shl2 (0n, mul xh yl) halfsize)
          (0n, mul xl yl)))

let ucompare2 (xh, xl) (yh, yl) =
  let c = ucompare xh yh in if c = 0 then ucompare xl yl else c

let validate d m p =
  let md = mul2 m d in
  let one2 = (0n, 1n) in
  let twoszp = shl2 one2 (size + p) in
  let twop1 = shl2 one2 (p + 1) in
  ucompare2 twoszp md < 0 && ucompare2 md (add2 twoszp twop1) <= 0
*)

let rec div_int c1 c2 dbg =
  match (c1, c2) with
    (c1, Cconst_int 0) ->
      Csequence(c1, Cop(Craise (Raise_regular, dbg),
                        [Cconst_symbol "caml_exn_Division_by_zero"]))
  | (c1, Cconst_int 1) ->
      c1
  | (Cconst_int 0 as c1, c2) ->
      Csequence(c2, c1)
  | (Cconst_int n1, Cconst_int n2) ->
      Cconst_int (n1 / n2)
  | (c1, Cconst_int n) when n <> min_int ->
      let l = Misc.log2 n in
      if n = 1 lsl l then
        (* Algorithm:
              t = shift-right-signed(c1, l - 1)
              t = shift-right(t, W - l)
              t = c1 + t
              res = shift-right-signed(c1 + t, l)
        *)
        Cop(Casr, [bind "dividend" c1 (fun c1 ->
                     let t = asr_int c1 (Cconst_int (l - 1)) in
                     let t = lsr_int t (Cconst_int (Nativeint.size - l)) in
                     add_int c1 t);
                   Cconst_int l])
      else if n < 0 then
        sub_int (Cconst_int 0) (div_int c1 (Cconst_int (-n)) dbg)
      else begin
        let (m, p) = divimm_parameters (Nativeint.of_int n) in
        (* Algorithm:
              t = multiply-high-signed(c1, m)
              if m < 0, t = t + c1
              if p > 0, t = shift-right-signed(t, p)
              res = t + sign-bit(c1)
        *)
        bind "dividend" c1 (fun c1 ->
          let t = Cop(Cmulhi, [c1; Cconst_natint m]) in
          let t = if m < 0n then Cop(Caddi, [t; c1]) else t in
          let t = if p > 0 then Cop(Casr, [t; Cconst_int p]) else t in
          add_int t (lsr_int c1 (Cconst_int (Nativeint.size - 1))))
      end
  | (c1, c2) when !Clflags.fast ->
      Cop(Cdivi, [c1; c2])
  | (c1, c2) ->
      bind "divisor" c2 (fun c2 ->
        Cifthenelse(c2,
                    Cop(Cdivi, [c1; c2]),
                    Cop(Craise (Raise_regular, dbg),
                        [Cconst_symbol "caml_exn_Division_by_zero"])))

let mod_int c1 c2 dbg =
  match (c1, c2) with
    (c1, Cconst_int 0) ->
      Csequence(c1, Cop(Craise (Raise_regular, dbg),
                        [Cconst_symbol "caml_exn_Division_by_zero"]))
  | (c1, Cconst_int (1 | (-1))) ->
      Csequence(c1, Cconst_int 0)
  | (Cconst_int 0, c2) ->
      Csequence(c2, Cconst_int 0)
  | (Cconst_int n1, Cconst_int n2) ->
      Cconst_int (n1 mod n2)
  | (c1, (Cconst_int n as c2)) when n <> min_int ->
      let l = Misc.log2 n in
      if n = 1 lsl l then
        (* Algorithm:
              t = shift-right-signed(c1, l - 1)
              t = shift-right(t, W - l)
              t = c1 + t
              t = bit-and(t, -n)
              res = c1 - t
         *)
        bind "dividend" c1 (fun c1 ->
          let t = asr_int c1 (Cconst_int (l - 1)) in
          let t = lsr_int t (Cconst_int (Nativeint.size - l)) in
          let t = add_int c1 t in
          let t = Cop(Cand, [t; Cconst_int (-n)]) in
          sub_int c1 t)
      else
        bind "dividend" c1 (fun c1 ->
          sub_int c1 (mul_int (div_int c1 c2 dbg) c2))
  | (c1, c2) when !Clflags.fast ->
      Cop(Cmodi, [c1; c2])
  | (c1, c2) ->
      bind "divisor" c2 (fun c2 ->
        Cifthenelse(c2,
                    Cop(Cmodi, [c1; c2]),
                    Cop(Craise (Raise_regular, dbg),
                        [Cconst_symbol "caml_exn_Division_by_zero"])))

(* Division or modulo on boxed integers.  The overflow case min_int / -1
   can occur, in which case we force x / -1 = -x and x mod -1 = 0. (PR#5513). *)

let is_different_from x = function
    Cconst_int n -> n <> x
  | Cconst_natint n -> n <> Nativeint.of_int x
  | _ -> false

let safe_divmod_bi mkop mkm1 c1 c2 bi dbg =
  bind "dividend" c1 (fun c1 ->
  bind "divisor" c2 (fun c2 ->
    let c = mkop c1 c2 dbg in
    if Arch.division_crashes_on_overflow
    && (size_int = 4 || bi <> Pint32)
    && not (is_different_from (-1) c2)
    then Cifthenelse(Cop(Ccmpi Cne, [c2; Cconst_int(-1)]), c, mkm1 c1)
    else c))

let safe_div_bi =
  safe_divmod_bi div_int (fun c1 -> Cop(Csubi, [Cconst_int 0; c1]))

let safe_mod_bi =
  safe_divmod_bi mod_int (fun c1 -> Cconst_int 0)

(* Bool *)

let test_bool = function
    Cop(Caddi, [Cop(Clsl, [c; Cconst_int 1]); Cconst_int 1]) -> c
  | Cop(Clsl, [c; Cconst_int 1]) -> c
  | c -> Cop(Ccmpi Cne, [c; Cconst_int 1])

(* Float *)

let box_float c = Cop(Calloc, [alloc_float_header; c])

let rec unbox_float = function
    Cop(Calloc, [header; c]) -> c
  | Clet(id, exp, body) -> Clet(id, exp, unbox_float body)
  | Cifthenelse(cond, e1, e2) ->
      Cifthenelse(cond, unbox_float e1, unbox_float e2)
  | Csequence(e1, e2) -> Csequence(e1, unbox_float e2)
  | Cswitch(e, tbl, el) -> Cswitch(e, tbl, Array.map unbox_float el)
  | Ccatch(n, ids, e1, e2) -> Ccatch(n, ids, unbox_float e1, unbox_float e2)
  | Ctrywith(e1, id, e2) -> Ctrywith(unbox_float e1, id, unbox_float e2)
  | c -> Cop(Cload Double_u, [c])

(* Complex *)

let box_complex c_re c_im =
  Cop(Calloc, [alloc_floatarray_header 2; c_re; c_im])

let complex_re c = Cop(Cload Double_u, [c])
let complex_im c = Cop(Cload Double_u,
                       [Cop(Cadda, [c; Cconst_int size_float])])

(* Unit *)

let return_unit c = Csequence(c, Cconst_pointer 1)

let rec remove_unit = function
    Cconst_pointer 1 -> Ctuple []
  | Csequence(c, Cconst_pointer 1) -> c
  | Csequence(c1, c2) ->
      Csequence(c1, remove_unit c2)
  | Cifthenelse(cond, ifso, ifnot) ->
      Cifthenelse(cond, remove_unit ifso, remove_unit ifnot)
  | Cswitch(sel, index, cases) ->
      Cswitch(sel, index, Array.map remove_unit cases)
  | Ccatch(io, ids, body, handler) ->
      Ccatch(io, ids, remove_unit body, remove_unit handler)
  | Ctrywith(body, exn, handler) ->
      Ctrywith(remove_unit body, exn, remove_unit handler)
  | Clet(id, c1, c2) ->
      Clet(id, c1, remove_unit c2)
  | Cop(Capply (mty, dbg), args) ->
      Cop(Capply (typ_void, dbg), args)
  | Cop(Cextcall(proc, mty, alloc, dbg), args) ->
      Cop(Cextcall(proc, typ_void, alloc, dbg), args)
  | Cexit (_,_) as c -> c
  | Ctuple [] as c -> c
  | c -> Csequence(c, Ctuple [])

(* Access to block fields *)

let field_address ptr n =
  if n = 0
  then ptr
  else Cop(Cadda, [ptr; Cconst_int(n * size_addr)])

let get_field ptr n =
  Cop(Cload Word, [field_address ptr n])

let set_field ptr n newval =
  Cop(Cstore Word, [field_address ptr n; newval])

let header ptr =
  Cop(Cload Word, [Cop(Cadda, [ptr; Cconst_int(-size_int)])])

let tag_offset =
  if big_endian then -1 else -size_int

let get_tag ptr =
  if Proc.word_addressed then           (* If byte loads are slow *)
    Cop(Cand, [header ptr; Cconst_int 255])
  else                                  (* If byte loads are efficient *)
    Cop(Cload Byte_unsigned,
        [Cop(Cadda, [ptr; Cconst_int(tag_offset)])])

let get_size ptr =
  Cop(Clsr, [header ptr; Cconst_int 10])

(* Array indexing *)

let log2_size_addr = Misc.log2 size_addr
let log2_size_float = Misc.log2 size_float

let wordsize_shift = 9
let numfloat_shift = 9 + log2_size_float - log2_size_addr

let is_addr_array_hdr hdr =
  Cop(Ccmpi Cne, [Cop(Cand, [hdr; Cconst_int 255]); floatarray_tag])

let is_addr_array_ptr ptr =
  Cop(Ccmpi Cne, [get_tag ptr; floatarray_tag])

let addr_array_length hdr = Cop(Clsr, [hdr; Cconst_int wordsize_shift])
let float_array_length hdr = Cop(Clsr, [hdr; Cconst_int numfloat_shift])

let lsl_const c n =
  Cop(Clsl, [c; Cconst_int n])

let array_indexing log2size ptr ofs =
  match ofs with
    Cconst_int n ->
      let i = n asr 1 in
      if i = 0 then ptr else Cop(Cadda, [ptr; Cconst_int(i lsl log2size)])
  | Cop(Caddi, [Cop(Clsl, [c; Cconst_int 1]); Cconst_int 1]) ->
      Cop(Cadda, [ptr; lsl_const c log2size])
  | Cop(Caddi, [c; Cconst_int n]) ->
      Cop(Cadda, [Cop(Cadda, [ptr; lsl_const c (log2size - 1)]);
                   Cconst_int((n-1) lsl (log2size - 1))])
  | _ ->
      Cop(Cadda, [Cop(Cadda, [ptr; lsl_const ofs (log2size - 1)]);
                   Cconst_int((-1) lsl (log2size - 1))])

let addr_array_ref arr ofs =
  Cop(Cload Word, [array_indexing log2_size_addr arr ofs])
let unboxed_float_array_ref arr ofs =
  Cop(Cload Double_u, [array_indexing log2_size_float arr ofs])
let float_array_ref arr ofs =
  box_float(unboxed_float_array_ref arr ofs)

let addr_array_set arr ofs newval =
  Cop(Cextcall("caml_modify", typ_void, false, Debuginfo.none),
      [array_indexing log2_size_addr arr ofs; newval])
let int_array_set arr ofs newval =
  Cop(Cstore Word, [array_indexing log2_size_addr arr ofs; newval])
let float_array_set arr ofs newval =
  Cop(Cstore Double_u, [array_indexing log2_size_float arr ofs; newval])

(* String length *)

(* Length of string block *)

let string_length exp =
  bind "str" exp (fun str ->
    let tmp_var = Ident.create "tmp" in
    Clet(tmp_var,
         Cop(Csubi,
             [Cop(Clsl,
                   [get_size str;
                     Cconst_int log2_size_addr]);
              Cconst_int 1]),
         Cop(Csubi,
             [Cvar tmp_var;
               Cop(Cload Byte_unsigned,
                     [Cop(Cadda, [str; Cvar tmp_var])])])))

(* Message sending *)

let lookup_tag obj tag =
  bind "tag" tag (fun tag ->
    Cop(Cextcall("caml_get_public_method", typ_addr, false, Debuginfo.none),
        [obj; tag]))

let lookup_label obj lab =
  bind "lab" lab (fun lab ->
    let table = Cop (Cload Word, [obj]) in
    addr_array_ref table lab)

let call_cached_method obj tag cache pos args dbg =
  let arity = List.length args in
  let cache = array_indexing log2_size_addr cache pos in
  Compilenv.need_send_fun arity;
  Cop(Capply (typ_addr, dbg),
      Cconst_symbol("caml_send" ^ string_of_int arity) ::
      obj :: tag :: cache :: args)

(* Allocation *)

let make_alloc_generic set_fn tag wordsize args =
  if wordsize <= Config.max_young_wosize then
    Cop(Calloc, Cconst_blockheader(block_header tag wordsize) :: args)
  else begin
    let id = Ident.create "alloc" in
    let rec fill_fields idx = function
      [] -> Cvar id
    | e1::el -> Csequence(set_fn (Cvar id) (Cconst_int idx) e1,
                          fill_fields (idx + 2) el) in
    Clet(id,
         Cop(Cextcall("caml_alloc", typ_addr, true, Debuginfo.none),
                 [Cconst_int wordsize; Cconst_int tag]),
         fill_fields 1 args)
  end

let make_alloc tag args =
  make_alloc_generic addr_array_set tag (List.length args) args
let make_float_alloc tag args =
  make_alloc_generic float_array_set tag
                     (List.length args * size_float / size_addr) args

(* Bounds checking *)

let make_checkbound dbg = function
  | [Cop(Clsr, [a1; Cconst_int n]); Cconst_int m] when (m lsl n) > n ->
      Cop(Ccheckbound dbg, [a1; Cconst_int(m lsl n + 1 lsl n - 1)])
  | args ->
      Cop(Ccheckbound dbg, args)

(* To compile "let rec" over values *)

let fundecls_size fundecls =
  let sz = ref (-1) in
  List.iter
    (fun f -> sz := !sz + 1 + (if f.arity = 1 then 2 else 3))
    fundecls;
  !sz

type rhs_kind =
  | RHS_block of int
  | RHS_floatblock of int
  | RHS_nonrec
;;
let rec expr_size env = function
  | Uvar id ->
      begin try Ident.find_same id env with Not_found -> RHS_nonrec end
  | Uclosure(fundecls, clos_vars) ->
      RHS_block (fundecls_size fundecls + List.length clos_vars)
  | Ulet(id, exp, body) ->
      expr_size (Ident.add id (expr_size env exp) env) body
  | Uletrec(bindings, body) ->
      expr_size env body
  | Uprim(Pmakeblock(tag,_,  mut), args, _) ->
      RHS_block (List.length args)
  | Uprim(Pmakearray(Paddrarray | Pintarray), args, _) ->
      RHS_block (List.length args)
  | Uprim(Pmakearray(Pfloatarray), args, _) ->
      RHS_floatblock (List.length args)
  | Uprim (Pduprecord (Record_regular, sz), _, _) ->
      RHS_block sz
  | Uprim (Pduprecord (Record_float, sz), _, _) ->
      RHS_floatblock sz
  | Usequence(exp, exp') ->
      expr_size env exp'
  | _ -> RHS_nonrec

(* Record application and currying functions *)

let apply_function n =
  Compilenv.need_apply_fun n; "caml_apply" ^ string_of_int n
let curry_function n =
  Compilenv.need_curry_fun n;
  if n >= 0
  then "caml_curry" ^ string_of_int n
  else "caml_tuplify" ^ string_of_int (-n)

(* Comparisons *)

let transl_comparison = function
    Lambda.Ceq -> Ceq
  | Lambda.Cneq -> Cne
  | Lambda.Cge -> Cge
  | Lambda.Cgt -> Cgt
  | Lambda.Cle -> Cle
  | Lambda.Clt -> Clt

(* Translate structured constants *)

let transl_constant = function
  | Uconst_int n ->
      int_const n
  | Uconst_ptr n ->
      if n <= max_repr_int && n >= min_repr_int
      then Cconst_pointer((n lsl 1) + 1)
      else Cconst_natpointer
              (Nativeint.add (Nativeint.shift_left (Nativeint.of_int n) 1) 1n)
  | Uconst_ref (label, _) ->
      Cconst_symbol label

let transl_structured_constant cst =
  let label = Compilenv.new_structured_constant cst ~shared:true in
  Cconst_symbol label

(* Translate constant closures *)

let constant_closures =
  ref ([] : (string * ufunction list) list)

(* Boxed integers *)

let box_int_constant bi n =
  match bi with
    Pnativeint -> Uconst_nativeint n
  | Pint32 -> Uconst_int32 (Nativeint.to_int32 n)
  | Pint64 -> Uconst_int64 (Int64.of_nativeint n)

let operations_boxed_int bi =
  match bi with
    Pnativeint -> "caml_nativeint_ops"
  | Pint32 -> "caml_int32_ops"
  | Pint64 -> "caml_int64_ops"

let alloc_header_boxed_int bi =
  match bi with
    Pnativeint -> alloc_boxedintnat_header
  | Pint32 -> alloc_boxedint32_header
  | Pint64 -> alloc_boxedint64_header

let box_int bi arg =
  match arg with
    Cconst_int n ->
      transl_structured_constant (box_int_constant bi (Nativeint.of_int n))
  | Cconst_natint n ->
      transl_structured_constant (box_int_constant bi n)
  | _ ->
      let arg' =
        if bi = Pint32 && size_int = 8 && big_endian
        then Cop(Clsl, [arg; Cconst_int 32])
        else arg in
      Cop(Calloc, [alloc_header_boxed_int bi;
                   Cconst_symbol(operations_boxed_int bi);
                   arg'])

let rec unbox_int bi arg =
  match arg with
    Cop(Calloc, [hdr; ops; Cop(Clsl, [contents; Cconst_int 32])])
    when bi = Pint32 && size_int = 8 && big_endian ->
      (* Force sign-extension of low 32 bits *)
      Cop(Casr, [Cop(Clsl, [contents; Cconst_int 32]); Cconst_int 32])
  | Cop(Calloc, [hdr; ops; contents])
    when bi = Pint32 && size_int = 8 && not big_endian ->
      (* Force sign-extension of low 32 bits *)
      Cop(Casr, [Cop(Clsl, [contents; Cconst_int 32]); Cconst_int 32])
  | Cop(Calloc, [hdr; ops; contents]) ->
      contents
  | Clet(id, exp, body) -> Clet(id, exp, unbox_int bi body)
  | Cifthenelse(cond, e1, e2) ->
      Cifthenelse(cond, unbox_int bi e1, unbox_int bi e2)
  | Csequence(e1, e2) -> Csequence(e1, unbox_int bi e2)
  | Cswitch(e, tbl, el) -> Cswitch(e, tbl, Array.map (unbox_int bi) el)
  | Ccatch(n, ids, e1, e2) -> Ccatch(n, ids, unbox_int bi e1, unbox_int bi e2)
  | Ctrywith(e1, id, e2) -> Ctrywith(unbox_int bi e1, id, unbox_int bi e2)
  | _ ->
      Cop(Cload(if bi = Pint32 then Thirtytwo_signed else Word),
          [Cop(Cadda, [arg; Cconst_int size_addr])])

let make_unsigned_int bi arg =
  if bi = Pint32 && size_int = 8
  then Cop(Cand, [arg; Cconst_natint 0xFFFFFFFFn])
  else arg

(* Big arrays *)

let bigarray_elt_size = function
    Pbigarray_unknown -> assert false
  | Pbigarray_float32 -> 4
  | Pbigarray_float64 -> 8
  | Pbigarray_sint8 -> 1
  | Pbigarray_uint8 -> 1
  | Pbigarray_sint16 -> 2
  | Pbigarray_uint16 -> 2
  | Pbigarray_int32 -> 4
  | Pbigarray_int64 -> 8
  | Pbigarray_caml_int -> size_int
  | Pbigarray_native_int -> size_int
  | Pbigarray_complex32 -> 8
  | Pbigarray_complex64 -> 16

let bigarray_indexing unsafe elt_kind layout b args dbg =
  let check_bound a1 a2 k =
    if unsafe then k else Csequence(make_checkbound dbg [a1;a2], k) in
  let rec ba_indexing dim_ofs delta_ofs = function
    [] -> assert false
  | [arg] ->
      bind "idx" (untag_int arg)
        (fun idx ->
           check_bound (Cop(Cload Word,[field_address b dim_ofs])) idx idx)
  | arg1 :: argl ->
      let rem = ba_indexing (dim_ofs + delta_ofs) delta_ofs argl in
      bind "idx" (untag_int arg1)
        (fun idx ->
          bind "bound" (Cop(Cload Word, [field_address b dim_ofs]))
          (fun bound ->
            check_bound bound idx (add_int (mul_int rem bound) idx))) in
  let offset =
    match layout with
      Pbigarray_unknown_layout ->
        assert false
    | Pbigarray_c_layout ->
        ba_indexing (4 + List.length args) (-1) (List.rev args)
    | Pbigarray_fortran_layout ->
        ba_indexing 5 1 (List.map (fun idx -> sub_int idx (Cconst_int 2)) args)
  and elt_size =
    bigarray_elt_size elt_kind in
  let byte_offset =
    if elt_size = 1
    then offset
    else Cop(Clsl, [offset; Cconst_int(log2 elt_size)]) in
  Cop(Cadda, [Cop(Cload Word, [field_address b 1]); byte_offset])

let bigarray_word_kind = function
    Pbigarray_unknown -> assert false
  | Pbigarray_float32 -> Single
  | Pbigarray_float64 -> Double
  | Pbigarray_sint8 -> Byte_signed
  | Pbigarray_uint8 -> Byte_unsigned
  | Pbigarray_sint16 -> Sixteen_signed
  | Pbigarray_uint16 -> Sixteen_unsigned
  | Pbigarray_int32 -> Thirtytwo_signed
  | Pbigarray_int64 -> Word
  | Pbigarray_caml_int -> Word
  | Pbigarray_native_int -> Word
  | Pbigarray_complex32 -> Single
  | Pbigarray_complex64 -> Double

let bigarray_get unsafe elt_kind layout b args dbg =
  bind "ba" b (fun b ->
    match elt_kind with
      Pbigarray_complex32 | Pbigarray_complex64 ->
        let kind = bigarray_word_kind elt_kind in
        let sz = bigarray_elt_size elt_kind / 2 in
        bind "addr" (bigarray_indexing unsafe elt_kind layout b args dbg)
          (fun addr ->
          box_complex
            (Cop(Cload kind, [addr]))
            (Cop(Cload kind, [Cop(Cadda, [addr; Cconst_int sz])])))
    | _ ->
        Cop(Cload (bigarray_word_kind elt_kind),
            [bigarray_indexing unsafe elt_kind layout b args dbg]))

let bigarray_set unsafe elt_kind layout b args newval dbg =
  bind "ba" b (fun b ->
    match elt_kind with
      Pbigarray_complex32 | Pbigarray_complex64 ->
        let kind = bigarray_word_kind elt_kind in
        let sz = bigarray_elt_size elt_kind / 2 in
        bind "newval" newval (fun newv ->
        bind "addr" (bigarray_indexing unsafe elt_kind layout b args dbg)
          (fun addr ->
          Csequence(
            Cop(Cstore kind, [addr; complex_re newv]),
            Cop(Cstore kind,
                [Cop(Cadda, [addr; Cconst_int sz]); complex_im newv]))))
    | _ ->
        Cop(Cstore (bigarray_word_kind elt_kind),
            [bigarray_indexing unsafe elt_kind layout b args dbg; newval]))

let unaligned_load_16 ptr idx =
  if Arch.allow_unaligned_access
  then Cop(Cload Sixteen_unsigned, [add_int ptr idx])
  else
    let v1 = Cop(Cload Byte_unsigned, [add_int ptr idx]) in
    let v2 = Cop(Cload Byte_unsigned,
                 [add_int (add_int ptr idx) (Cconst_int 1)]) in
    let b1, b2 = if Arch.big_endian then v1, v2 else v2, v1 in
    Cop(Cor, [lsl_int b1 (Cconst_int 8); b2])

let unaligned_set_16 ptr idx newval =
  if Arch.allow_unaligned_access
  then Cop(Cstore Sixteen_unsigned, [add_int ptr idx; newval])
  else
    let v1 = Cop(Cand, [Cop(Clsr, [newval; Cconst_int 8]); Cconst_int 0xFF]) in
    let v2 = Cop(Cand, [newval; Cconst_int 0xFF]) in
    let b1, b2 = if Arch.big_endian then v1, v2 else v2, v1 in
    Csequence(
        Cop(Cstore Byte_unsigned, [add_int ptr idx; b1]),
        Cop(Cstore Byte_unsigned,
            [add_int (add_int ptr idx) (Cconst_int 1); b2]))

let unaligned_load_32 ptr idx =
  if Arch.allow_unaligned_access
  then Cop(Cload Thirtytwo_unsigned, [add_int ptr idx])
  else
    let v1 = Cop(Cload Byte_unsigned, [add_int ptr idx]) in
    let v2 = Cop(Cload Byte_unsigned,
                 [add_int (add_int ptr idx) (Cconst_int 1)]) in
    let v3 = Cop(Cload Byte_unsigned,
                 [add_int (add_int ptr idx) (Cconst_int 2)]) in
    let v4 = Cop(Cload Byte_unsigned,
                 [add_int (add_int ptr idx) (Cconst_int 3)]) in
    let b1, b2, b3, b4 =
      if Arch.big_endian
      then v1, v2, v3, v4
      else v4, v3, v2, v1 in
    Cop(Cor,
        [Cop(Cor, [lsl_int b1 (Cconst_int 24); lsl_int b2 (Cconst_int 16)]);
         Cop(Cor, [lsl_int b3 (Cconst_int 8); b4])])

let unaligned_set_32 ptr idx newval =
  if Arch.allow_unaligned_access
  then Cop(Cstore Thirtytwo_unsigned, [add_int ptr idx; newval])
  else
    let v1 =
      Cop(Cand, [Cop(Clsr, [newval; Cconst_int 24]); Cconst_int 0xFF]) in
    let v2 =
      Cop(Cand, [Cop(Clsr, [newval; Cconst_int 16]); Cconst_int 0xFF]) in
    let v3 =
      Cop(Cand, [Cop(Clsr, [newval; Cconst_int 8]); Cconst_int 0xFF]) in
    let v4 = Cop(Cand, [newval; Cconst_int 0xFF]) in
    let b1, b2, b3, b4 =
      if Arch.big_endian
      then v1, v2, v3, v4
      else v4, v3, v2, v1 in
    Csequence(
        Csequence(
            Cop(Cstore Byte_unsigned, [add_int ptr idx; b1]),
            Cop(Cstore Byte_unsigned,
                [add_int (add_int ptr idx) (Cconst_int 1); b2])),
        Csequence(
            Cop(Cstore Byte_unsigned,
                [add_int (add_int ptr idx) (Cconst_int 2); b3]),
            Cop(Cstore Byte_unsigned,
                [add_int (add_int ptr idx) (Cconst_int 3); b4])))

let unaligned_load_64 ptr idx =
  assert(size_int = 8);
  if Arch.allow_unaligned_access
  then Cop(Cload Word, [add_int ptr idx])
  else
    let v1 = Cop(Cload Byte_unsigned, [add_int ptr idx]) in
    let v2 = Cop(Cload Byte_unsigned,
                 [add_int (add_int ptr idx) (Cconst_int 1)]) in
    let v3 = Cop(Cload Byte_unsigned,
                 [add_int (add_int ptr idx) (Cconst_int 2)]) in
    let v4 = Cop(Cload Byte_unsigned,
                 [add_int (add_int ptr idx) (Cconst_int 3)]) in
    let v5 = Cop(Cload Byte_unsigned,
                 [add_int (add_int ptr idx) (Cconst_int 4)]) in
    let v6 = Cop(Cload Byte_unsigned,
                 [add_int (add_int ptr idx) (Cconst_int 5)]) in
    let v7 = Cop(Cload Byte_unsigned,
                 [add_int (add_int ptr idx) (Cconst_int 6)]) in
    let v8 = Cop(Cload Byte_unsigned,
                 [add_int (add_int ptr idx) (Cconst_int 7)]) in
    let b1, b2, b3, b4, b5, b6, b7, b8 =
      if Arch.big_endian
      then v1, v2, v3, v4, v5, v6, v7, v8
      else v8, v7, v6, v5, v4, v3, v2, v1 in
    Cop(Cor,
        [Cop(Cor,
             [Cop(Cor, [lsl_int b1 (Cconst_int (8*7));
                        lsl_int b2 (Cconst_int (8*6))]);
              Cop(Cor, [lsl_int b3 (Cconst_int (8*5));
                        lsl_int b4 (Cconst_int (8*4))])]);
         Cop(Cor,
             [Cop(Cor, [lsl_int b5 (Cconst_int (8*3));
                        lsl_int b6 (Cconst_int (8*2))]);
              Cop(Cor, [lsl_int b7 (Cconst_int 8);
                        b8])])])

let unaligned_set_64 ptr idx newval =
  assert(size_int = 8);
  if Arch.allow_unaligned_access
  then Cop(Cstore Word, [add_int ptr idx; newval])
  else
    let v1 =
      Cop(Cand, [Cop(Clsr, [newval; Cconst_int (8*7)]); Cconst_int 0xFF]) in
    let v2 =
      Cop(Cand, [Cop(Clsr, [newval; Cconst_int (8*6)]); Cconst_int 0xFF]) in
    let v3 =
      Cop(Cand, [Cop(Clsr, [newval; Cconst_int (8*5)]); Cconst_int 0xFF]) in
    let v4 =
      Cop(Cand, [Cop(Clsr, [newval; Cconst_int (8*4)]); Cconst_int 0xFF]) in
    let v5 =
      Cop(Cand, [Cop(Clsr, [newval; Cconst_int (8*3)]); Cconst_int 0xFF]) in
    let v6 =
      Cop(Cand, [Cop(Clsr, [newval; Cconst_int (8*2)]); Cconst_int 0xFF]) in
    let v7 = Cop(Cand, [Cop(Clsr, [newval; Cconst_int 8]); Cconst_int 0xFF]) in
    let v8 = Cop(Cand, [newval; Cconst_int 0xFF]) in
    let b1, b2, b3, b4, b5, b6, b7, b8 =
      if Arch.big_endian
      then v1, v2, v3, v4, v5, v6, v7, v8
      else v8, v7, v6, v5, v4, v3, v2, v1 in
    Csequence(
        Csequence(
            Csequence(
                Cop(Cstore Byte_unsigned, [add_int ptr idx; b1]),
                Cop(Cstore Byte_unsigned,
                    [add_int (add_int ptr idx) (Cconst_int 1); b2])),
            Csequence(
                Cop(Cstore Byte_unsigned,
                    [add_int (add_int ptr idx) (Cconst_int 2); b3]),
                Cop(Cstore Byte_unsigned,
                    [add_int (add_int ptr idx) (Cconst_int 3); b4]))),
        Csequence(
            Csequence(
                Cop(Cstore Byte_unsigned,
                    [add_int (add_int ptr idx) (Cconst_int 4); b5]),
                Cop(Cstore Byte_unsigned,
                    [add_int (add_int ptr idx) (Cconst_int 5); b6])),
            Csequence(
                Cop(Cstore Byte_unsigned,
                    [add_int (add_int ptr idx) (Cconst_int 6); b7]),
                Cop(Cstore Byte_unsigned,
                    [add_int (add_int ptr idx) (Cconst_int 7); b8]))))

let max_or_zero a =
  bind "size" a (fun a ->
    (* equivalent to
       Cifthenelse(Cop(Ccmpi Cle, [a; Cconst_int 0]), Cconst_int 0, a)

       if a is positive, sign is 0 hence sign_negation is full of 1
                         so sign_negation&a = a
       if a is negative, sign is full of 1 hence sign_negation is 0
                         so sign_negation&a = 0 *)
    let sign = Cop(Casr, [a; Cconst_int (size_int * 8 - 1)]) in
    let sign_negation = Cop(Cxor, [sign; Cconst_int (-1)]) in
    Cop(Cand, [sign_negation; a]))

let check_bound unsafe dbg a1 a2 k =
  if unsafe then k
  else Csequence(make_checkbound dbg [max_or_zero a1;a2], k)

(* Simplification of some primitives into C calls *)

let default_prim name =
  { prim_name = name; prim_arity = 0 (*ignored*);
    prim_alloc = true; prim_native_name = ""; prim_native_float = false ; }

let simplif_primitive_32bits = function
    Pbintofint Pint64 -> Pccall (default_prim "caml_int64_of_int")
  | Pintofbint Pint64 -> Pccall (default_prim "caml_int64_to_int")
  | Pcvtbint(Pint32, Pint64) -> Pccall (default_prim "caml_int64_of_int32")
  | Pcvtbint(Pint64, Pint32) -> Pccall (default_prim "caml_int64_to_int32")
  | Pcvtbint(Pnativeint, Pint64) ->
      Pccall (default_prim "caml_int64_of_nativeint")
  | Pcvtbint(Pint64, Pnativeint) ->
      Pccall (default_prim "caml_int64_to_nativeint")
  | Pnegbint Pint64 -> Pccall (default_prim "caml_int64_neg")
  | Paddbint Pint64 -> Pccall (default_prim "caml_int64_add")
  | Psubbint Pint64 -> Pccall (default_prim "caml_int64_sub")
  | Pmulbint Pint64 -> Pccall (default_prim "caml_int64_mul")
  | Pdivbint Pint64 -> Pccall (default_prim "caml_int64_div")
  | Pmodbint Pint64 -> Pccall (default_prim "caml_int64_mod")
  | Pandbint Pint64 -> Pccall (default_prim "caml_int64_and")
  | Porbint Pint64 ->  Pccall (default_prim "caml_int64_or")
  | Pxorbint Pint64 -> Pccall (default_prim "caml_int64_xor")
  | Plslbint Pint64 -> Pccall (default_prim "caml_int64_shift_left")
  | Plsrbint Pint64 -> Pccall (default_prim "caml_int64_shift_right_unsigned")
  | Pasrbint Pint64 -> Pccall (default_prim "caml_int64_shift_right")
  | Pbintcomp(Pint64, Lambda.Ceq) -> Pccall (default_prim "caml_equal")
  | Pbintcomp(Pint64, Lambda.Cneq) -> Pccall (default_prim "caml_notequal")
  | Pbintcomp(Pint64, Lambda.Clt) -> Pccall (default_prim "caml_lessthan")
  | Pbintcomp(Pint64, Lambda.Cgt) -> Pccall (default_prim "caml_greaterthan")
  | Pbintcomp(Pint64, Lambda.Cle) -> Pccall (default_prim "caml_lessequal")
  | Pbintcomp(Pint64, Lambda.Cge) -> Pccall (default_prim "caml_greaterequal")
  | Pbigarrayref(unsafe, n, Pbigarray_int64, layout) ->
      Pccall (default_prim ("caml_ba_get_" ^ string_of_int n))
  | Pbigarrayset(unsafe, n, Pbigarray_int64, layout) ->
      Pccall (default_prim ("caml_ba_set_" ^ string_of_int n))
  | Pstring_load_64(_) -> Pccall (default_prim "caml_string_get64")
  | Pstring_set_64(_) -> Pccall (default_prim "caml_string_set64")
  | Pbigstring_load_64(_) -> Pccall (default_prim "caml_ba_uint8_get64")
  | Pbigstring_set_64(_) -> Pccall (default_prim "caml_ba_uint8_set64")
  | Pbbswap Pint64 -> Pccall (default_prim "caml_int64_bswap")
  | p -> p

let simplif_primitive p =
  match p with
  | Pduprecord _ ->
      Pccall (default_prim "caml_obj_dup")
  | Pbigarrayref(unsafe, n, Pbigarray_unknown, layout) ->
      Pccall (default_prim ("caml_ba_get_" ^ string_of_int n))
  | Pbigarrayset(unsafe, n, Pbigarray_unknown, layout) ->
      Pccall (default_prim ("caml_ba_set_" ^ string_of_int n))
  | Pbigarrayref(unsafe, n, kind, Pbigarray_unknown_layout) ->
      Pccall (default_prim ("caml_ba_get_" ^ string_of_int n))
  | Pbigarrayset(unsafe, n, kind, Pbigarray_unknown_layout) ->
      Pccall (default_prim ("caml_ba_set_" ^ string_of_int n))
  | p ->
      if size_int = 8 then p else simplif_primitive_32bits p

(* Build switchers both for constants and blocks *)

let transl_isout h arg = tag_int (Cop(Ccmpa Clt, [h ; arg]))

(* Build an actual switch (ie jump table) *)

module SArgBlocks =
struct
  type primitive = operation

  let eqint = Ccmpi Ceq
  let neint = Ccmpi Cne
  let leint = Ccmpi Cle
  let ltint = Ccmpi Clt
  let geint = Ccmpi Cge
  let gtint = Ccmpi Cgt

  type act = expression

  let default = Cexit (0,[])
  let make_const i =  Cconst_int i
  let make_prim p args = Cop (p,args)
  let make_offset arg n = add_const arg n
  let make_isout h arg =  Cop (Ccmpa Clt, [h ; arg])
  let make_isin h arg =  Cop (Ccmpa Cge, [h ; arg])
  let make_if cond ifso ifnot = Cifthenelse (cond, ifso, ifnot)
  let make_switch arg cases actions = Cswitch (arg,cases,actions)
  let bind arg body = bind "switcher" arg body

  let make_catch handler = match handler with
  | Cexit (i,[]) -> i,fun e -> e
  | _ ->
      let i = next_raise_count () in
(*
      Printf.eprintf  "SHARE CMM: %i\n" i ;
      Printcmm.expression Format.str_formatter handler ;
      Printf.eprintf "%s\n" (Format.flush_str_formatter ()) ;
*)
      i,
      (fun body -> match body with
      | Cexit (j,_) ->
          if i=j then handler
          else body
      | _ ->  Ccatch (i,[],body,handler))

  let make_exit i = Cexit (i,[])

end

(* cmm store, as sharing as normally been detected in previous
   phases, we only share exits *)
module StoreExp =
  Switch.Store
    (struct
      type t = expression
      type key = int
      let make_key = function
        | Cexit (i,[]) -> Some i
        | _ -> None
    end)

module SwitcherBlocks = Switch.Make(SArgBlocks)

(* Int switcher, arg in [low..high],
   cases is list of individual cases, and is sorted by first component *)

let transl_int_switch arg low high cases default = match cases with
| [] -> assert false
| _::_ ->
    let store = StoreExp.mk_store () in
    assert (store.Switch.act_store default = 0) ;
    let cases =
      List.map
        (fun (i,act) -> i,store.Switch.act_store act)
        cases in
    let rec inters plow phigh pact = function
      | [] ->
          if phigh = high then [plow,phigh,pact]
          else [(plow,phigh,pact); (phigh+1,high,0) ]
      | (i,act)::rem ->
          if i = phigh+1 then
            if pact = act then
              inters plow i pact rem
            else
              (plow,phigh,pact)::inters i i act rem
          else (* insert default *)
            if pact = 0 then
              if act = 0 then
                inters plow i 0 rem
              else
                (plow,i-1,pact)::
                inters i i act rem
            else (* pact <> 0 *)
              (plow,phigh,pact)::
              begin
                if act = 0 then inters (phigh+1) i 0 rem
                else (phigh+1,i-1,0)::inters i i act rem
              end in
    let inters = match cases with
    | [] -> assert false
    | (k0,act0)::rem ->
        if k0 = low then inters k0 k0 act0 rem
        else inters low (k0-1) 0 cases in
    bind "switcher" arg
      (fun a ->
        SwitcherBlocks.zyva
          (low,high)
          a
          (Array.of_list inters) store)


(* Auxiliary functions for optimizing "let" of boxed numbers (floats and
   boxed integers *)

type unboxed_number_kind =
    No_unboxing
  | Boxed_float
  | Boxed_integer of boxed_integer

let rec is_unboxed_number = function
    Uconst(Uconst_ref(_, Uconst_float _)) ->
      Boxed_float
  | Uprim(p, _, _) ->
      begin match simplif_primitive p with
          Pccall p -> if p.prim_native_float then Boxed_float else No_unboxing
        | Pfloatfield _ -> Boxed_float
        | Pfloatofint -> Boxed_float
        | Pnegfloat -> Boxed_float
        | Pabsfloat -> Boxed_float
        | Paddfloat -> Boxed_float
        | Psubfloat -> Boxed_float
        | Pmulfloat -> Boxed_float
        | Pdivfloat -> Boxed_float
        | Parrayrefu Pfloatarray -> Boxed_float
        | Parrayrefs Pfloatarray -> Boxed_float
        | Pbintofint bi -> Boxed_integer bi
        | Pcvtbint(src, dst) -> Boxed_integer dst
        | Pnegbint bi -> Boxed_integer bi
        | Paddbint bi -> Boxed_integer bi
        | Psubbint bi -> Boxed_integer bi
        | Pmulbint bi -> Boxed_integer bi
        | Pdivbint bi -> Boxed_integer bi
        | Pmodbint bi -> Boxed_integer bi
        | Pandbint bi -> Boxed_integer bi
        | Porbint bi -> Boxed_integer bi
        | Pxorbint bi -> Boxed_integer bi
        | Plslbint bi -> Boxed_integer bi
        | Plsrbint bi -> Boxed_integer bi
        | Pasrbint bi -> Boxed_integer bi
        | Pbigarrayref(_, _, (Pbigarray_float32 | Pbigarray_float64), _) ->
            Boxed_float
        | Pbigarrayref(_, _, Pbigarray_int32, _) -> Boxed_integer Pint32
        | Pbigarrayref(_, _, Pbigarray_int64, _) -> Boxed_integer Pint64
        | Pbigarrayref(_, _, Pbigarray_native_int,_) -> Boxed_integer Pnativeint
        | Pstring_load_32(_) -> Boxed_integer Pint32
        | Pstring_load_64(_) -> Boxed_integer Pint64
        | Pbigstring_load_32(_) -> Boxed_integer Pint32
        | Pbigstring_load_64(_) -> Boxed_integer Pint64
        | Pbbswap bi -> Boxed_integer bi
        | _ -> No_unboxing
      end
  | Ulet (_, _, e) | Usequence (_, e) -> is_unboxed_number e
  | _ -> No_unboxing

let subst_boxed_number unbox_fn boxed_id unboxed_id box_chunk box_offset exp =
  let need_boxed = ref false in
  let assigned = ref false in
  let rec subst = function
      Cvar id as e ->
        if Ident.same id boxed_id then need_boxed := true; e
    | Clet(id, arg, body) -> Clet(id, subst arg, subst body)
    | Cassign(id, arg) ->
        if Ident.same id boxed_id then begin
          assigned := true;
          Cassign(unboxed_id, subst(unbox_fn arg))
        end else
          Cassign(id, subst arg)
    | Ctuple argv -> Ctuple(List.map subst argv)
    | Cop(Cload chunk, [Cvar id]) as e ->
      if not (Ident.same id boxed_id) then e
      else if chunk = box_chunk && box_offset = 0 then
        Cvar unboxed_id
      else begin
        need_boxed := true;
        e
      end
    | Cop(Cload chunk, [Cop(Cadda, [Cvar id; Cconst_int ofs])]) as e ->
      if not (Ident.same id boxed_id) then e
      else if chunk = box_chunk && ofs = box_offset then
        Cvar unboxed_id
      else begin
        need_boxed := true;
        e
      end
    | Cop(op, argv) -> Cop(op, List.map subst argv)
    | Csequence(e1, e2) -> Csequence(subst e1, subst e2)
    | Cifthenelse(e1, e2, e3) -> Cifthenelse(subst e1, subst e2, subst e3)
    | Cswitch(arg, index, cases) ->
        Cswitch(subst arg, index, Array.map subst cases)
    | Cloop e -> Cloop(subst e)
    | Ccatch(nfail, ids, e1, e2) -> Ccatch(nfail, ids, subst e1, subst e2)
    | Cexit (nfail, el) -> Cexit (nfail, List.map subst el)
    | Ctrywith(e1, id, e2) -> Ctrywith(subst e1, id, subst e2)
    | Cconst_int _ | Cconst_natint _ | Cconst_float _ | Cconst_symbol _
    | Cconst_pointer _ | Cconst_natpointer _
    | Cconst_blockheader _ as e -> e
  in
  let res = subst exp in
  (res, !need_boxed, !assigned)

(* Translate an expression *)

let functions = (Queue.create() : ufunction Queue.t)

let strmatch_compile =
  let module S =
    Strmatch.Make
      (struct
        let string_block_length = get_size
        let transl_switch = transl_int_switch
      end) in
  S.compile

let rec transl = function
    Uvar id ->
      Cvar id
  | Uconst sc ->
      transl_constant sc
  | Uclosure(fundecls, []) ->
      let lbl = Compilenv.new_const_symbol() in
      constant_closures := (lbl, fundecls) :: !constant_closures;
      List.iter (fun f -> Queue.add f functions) fundecls;
      Cconst_symbol lbl
  | Uclosure(fundecls, clos_vars) ->
      let block_size =
        fundecls_size fundecls + List.length clos_vars in
      let rec transl_fundecls pos = function
          [] ->
            List.map transl clos_vars
        | f :: rem ->
            Queue.add f functions;
            let header =
              if pos = 0
              then alloc_closure_header block_size
              else alloc_infix_header pos in
            if f.arity = 1 then
              header ::
              Cconst_symbol f.label ::
              int_const 1 ::
              transl_fundecls (pos + 3) rem
            else
              header ::
              Cconst_symbol(curry_function f.arity) ::
              int_const f.arity ::
              Cconst_symbol f.label ::
              transl_fundecls (pos + 4) rem in
      Cop(Calloc, transl_fundecls 0 fundecls)
  | Uoffset(arg, offset) ->
      field_address (transl arg) offset
  | Udirect_apply(lbl, args, dbg) ->
      Cop(Capply(typ_addr, dbg), Cconst_symbol lbl :: List.map transl args)
  | Ugeneric_apply(clos, [arg], dbg) ->
      bind "fun" (transl clos) (fun clos ->
        Cop(Capply(typ_addr, dbg), [get_field clos 0; transl arg; clos]))
  | Ugeneric_apply(clos, args, dbg) ->
      let arity = List.length args in
      let cargs = Cconst_symbol(apply_function arity) ::
        List.map transl (args @ [clos]) in
      Cop(Capply(typ_addr, dbg), cargs)
  | Usend(kind, met, obj, args, dbg) ->
      let call_met obj args clos =
        if args = [] then
          Cop(Capply(typ_addr, dbg), [get_field clos 0;obj;clos])
        else
          let arity = List.length args + 1 in
          let cargs = Cconst_symbol(apply_function arity) :: obj ::
            (List.map transl args) @ [clos] in
          Cop(Capply(typ_addr, dbg), cargs)
      in
      bind "obj" (transl obj) (fun obj ->
        match kind, args with
          Self, _ ->
            bind "met" (lookup_label obj (transl met)) (call_met obj args)
        | Cached, cache :: pos :: args ->
            call_cached_method obj (transl met) (transl cache) (transl pos)
              (List.map transl args) dbg
        | _ ->
            bind "met" (lookup_tag obj (transl met)) (call_met obj args))
  | Ulet(id, exp, body) ->
      begin match is_unboxed_number exp with
        No_unboxing ->
          Clet(id, transl exp, transl body)
      | Boxed_float ->
          transl_unbox_let box_float unbox_float transl_unbox_float
                           Double_u 0
                           id exp body
      | Boxed_integer bi ->
          transl_unbox_let (box_int bi) (unbox_int bi) (transl_unbox_int bi)
                           (if bi = Pint32 then Thirtytwo_signed else Word)
                           size_addr
                           id exp body
      end
  | Uletrec(bindings, body) ->
      transl_letrec bindings (transl body)

  (* Primitives *)
  | Uprim(prim, args, dbg) ->
      begin match (simplif_primitive prim, args) with
        (Pgetglobal id, []) ->
          Cconst_symbol (Ident.name id)
      | (Pmakeblock(tag, _, mut), []) ->
          assert false
      | (Pmakeblock(tag,_, mut), args) ->
          make_alloc tag (List.map transl args)
      | (Pccall prim, args) ->
          if prim.prim_native_float then
            box_float
              (Cop(Cextcall(prim.prim_native_name, typ_float, false, dbg),
                   List.map transl_unbox_float args))
          else
            Cop(Cextcall(Primitive.native_name prim, typ_addr, prim.prim_alloc,
                         dbg),
                List.map transl args)
      | (Pmakearray kind, []) ->
          transl_structured_constant (Uconst_block(0, []))
      | (Pmakearray kind, args) ->
          begin match kind with
            Pgenarray ->
              Cop(Cextcall("caml_make_array", typ_addr, true, Debuginfo.none),
                  [make_alloc 0 (List.map transl args)])
          | Paddrarray | Pintarray ->
              make_alloc 0 (List.map transl args)
          | Pfloatarray ->
              make_float_alloc Obj.double_array_tag
                              (List.map transl_unbox_float args)
          end
      | (Pbigarrayref(unsafe, num_dims, elt_kind, layout), arg1 :: argl) ->
          let elt =
            bigarray_get unsafe elt_kind layout
              (transl arg1) (List.map transl argl) dbg in
          begin match elt_kind with
            Pbigarray_float32 | Pbigarray_float64 -> box_float elt
          | Pbigarray_complex32 | Pbigarray_complex64 -> elt
          | Pbigarray_int32 -> box_int Pint32 elt
          | Pbigarray_int64 -> box_int Pint64 elt
          | Pbigarray_native_int -> box_int Pnativeint elt
          | Pbigarray_caml_int -> force_tag_int elt
          | _ -> tag_int elt
          end
      | (Pbigarrayset(unsafe, num_dims, elt_kind, layout), arg1 :: argl) ->
          let (argidx, argnewval) = split_last argl in
          return_unit(bigarray_set unsafe elt_kind layout
            (transl arg1)
            (List.map transl argidx)
            (match elt_kind with
              Pbigarray_float32 | Pbigarray_float64 ->
                transl_unbox_float argnewval
            | Pbigarray_complex32 | Pbigarray_complex64 -> transl argnewval
            | Pbigarray_int32 -> transl_unbox_int Pint32 argnewval
            | Pbigarray_int64 -> transl_unbox_int Pint64 argnewval
            | Pbigarray_native_int -> transl_unbox_int Pnativeint argnewval
            | _ -> untag_int (transl argnewval))
            dbg)
      | (Pbigarraydim(n), [b]) ->
          let dim_ofs = 4 + n in
          tag_int (Cop(Cload Word, [field_address (transl b) dim_ofs]))
      | (p, [arg]) ->
          transl_prim_1 p arg dbg
      | (p, [arg1; arg2]) ->
          transl_prim_2 p arg1 arg2 dbg
      | (p, [arg1; arg2; arg3]) ->
          transl_prim_3 p arg1 arg2 arg3 dbg
      | (_, _) ->
          fatal_error "Cmmgen.transl:prim"
      end

  (* Control structures *)
  | Uswitch(arg, s) ->
      (* As in the bytecode interpreter, only matching against constants
         can be checked *)
      if Array.length s.us_index_blocks = 0 then
        Cswitch
          (untag_int (transl arg),
           s.us_index_consts,
           Array.map transl s.us_actions_consts)
      else if Array.length s.us_index_consts = 0 then
        transl_switch (get_tag (transl arg))
          s.us_index_blocks s.us_actions_blocks
      else
        bind "switch" (transl arg) (fun arg ->
          Cifthenelse(
          Cop(Cand, [arg; Cconst_int 1]),
          transl_switch
            (untag_int arg) s.us_index_consts s.us_actions_consts,
          transl_switch
            (get_tag arg) s.us_index_blocks s.us_actions_blocks))
  | Ustringswitch(arg,sw,d) ->
      bind "switch" (transl arg)
        (fun arg ->
          strmatch_compile arg (Misc.may_map transl d)
            (List.map (fun (s,act) -> s,transl act) sw))
  | Ustaticfail (nfail, args) ->
      Cexit (nfail, List.map transl args)
  | Ucatch(nfail, [], body, handler) ->
      make_catch nfail (transl body) (transl handler)
  | Ucatch(nfail, ids, body, handler) ->
      Ccatch(nfail, ids, transl body, transl handler)
  | Utrywith(body, exn, handler) ->
      Ctrywith(transl body, exn, transl handler)
  | Uifthenelse(Uprim(Pnot, [arg], _), ifso, ifnot) ->
      transl (Uifthenelse(arg, ifnot, ifso))
  | Uifthenelse(cond, ifso, Ustaticfail (nfail, [])) ->
      exit_if_false cond (transl ifso) nfail
  | Uifthenelse(cond, Ustaticfail (nfail, []), ifnot) ->
      exit_if_true cond nfail (transl ifnot)
  | Uifthenelse(Uprim(Psequand, _, _) as cond, ifso, ifnot) ->
      let raise_num = next_raise_count () in
      make_catch
        raise_num
        (exit_if_false cond (transl ifso) raise_num)
        (transl ifnot)
  | Uifthenelse(Uprim(Psequor, _, _) as cond, ifso, ifnot) ->
      let raise_num = next_raise_count () in
      make_catch
        raise_num
        (exit_if_true cond raise_num (transl ifnot))
        (transl ifso)
  | Uifthenelse (Uifthenelse (cond, condso, condnot), ifso, ifnot) ->
      let num_true = next_raise_count () in
      make_catch
        num_true
        (make_catch2
           (fun shared_false ->
             Cifthenelse
               (test_bool (transl cond),
                exit_if_true condso num_true shared_false,
                exit_if_true condnot num_true shared_false))
           (transl ifnot))
        (transl ifso)
  | Uifthenelse(cond, ifso, ifnot) ->
      Cifthenelse(test_bool(transl cond), transl ifso, transl ifnot)
  | Usequence(exp1, exp2) ->
      Csequence(remove_unit(transl exp1), transl exp2)
  | Uwhile(cond, body) ->
      let raise_num = next_raise_count () in
      return_unit
        (Ccatch
           (raise_num, [],
            Cloop(exit_if_false cond (remove_unit(transl body)) raise_num),
            Ctuple []))
  | Ufor(id, low, high, dir, body) ->
      let tst = match dir with Upto -> Cgt   | Downto -> Clt in
      let inc = match dir with Upto -> Caddi | Downto -> Csubi in
      let raise_num = next_raise_count () in
      let id_prev = Ident.rename id in
      return_unit
        (Clet
           (id, transl low,
            bind_nonvar "bound" (transl high) (fun high ->
              Ccatch
                (raise_num, [],
                 Cifthenelse
                   (Cop(Ccmpi tst, [Cvar id; high]), Cexit (raise_num, []),
                    Cloop
                      (Csequence
                         (remove_unit(transl body),
                         Clet(id_prev, Cvar id,
                          Csequence
                            (Cassign(id,
                               Cop(inc, [Cvar id; Cconst_int 2])),
                             Cifthenelse
                               (Cop(Ccmpi Ceq, [Cvar id_prev; high]),
                                Cexit (raise_num,[]), Ctuple [])))))),
                 Ctuple []))))
  | Uassign(id, exp) ->
      return_unit(Cassign(id, transl exp))

and transl_prim_1 p arg dbg =
  match p with
  (* Generic operations *)
  | (Pidentity | Pbytes_to_string | Pbytes_of_string ) ->
      transl arg
  | Pignore ->
      return_unit(remove_unit (transl arg))
  (* Heap operations *)
  | Pfield (n,_) ->
      get_field (transl arg) n
  | Pfloatfield (n,_) ->
      let ptr = transl arg in
      box_float(
        Cop(Cload Double_u,
            [if n = 0 then ptr
                       else Cop(Cadda, [ptr; Cconst_int(n * size_float)])]))
  | Pint_as_pointer ->
     Cop(Cadda, [transl arg; Cconst_int (-1)])
  (* Exceptions *)
  | Praise k ->
      Cop(Craise (k, dbg), [transl arg])
  (* Integer operations *)
  | Pnegint ->
      Cop(Csubi, [Cconst_int 2; transl arg])
  | Pctconst c ->
      let const_of_bool b = tag_int (Cconst_int (if b then 1 else 0)) in
      begin
        match c with
        | Big_endian -> const_of_bool Arch.big_endian
        | Word_size -> tag_int (Cconst_int (8*Arch.size_int))
        | Ostype_unix -> const_of_bool (Sys.os_type = "Unix")
        | Ostype_win32 -> const_of_bool (Sys.os_type = "Win32")
        | Ostype_cygwin -> const_of_bool (Sys.os_type = "Cygwin")
      end
  | Poffsetint n ->
      if no_overflow_lsl n then
        add_const (transl arg) (n lsl 1)
      else
        transl_prim_2 Paddint arg (Uconst (Uconst_int n))
                      Debuginfo.none
  | Poffsetref n ->
      return_unit
        (bind "ref" (transl arg) (fun arg ->
          Cop(Cstore Word,
              [arg; add_const (Cop(Cload Word, [arg])) (n lsl 1)])))
  (* Floating-point operations *)
  | Pfloatofint ->
      box_float(Cop(Cfloatofint, [untag_int(transl arg)]))
  | Pintoffloat ->
     tag_int(Cop(Cintoffloat, [transl_unbox_float arg]))
  | Pnegfloat ->
      box_float(Cop(Cnegf, [transl_unbox_float arg]))
  | Pabsfloat ->
      box_float(Cop(Cabsf, [transl_unbox_float arg]))
  (* String operations *)
  | (Pstringlength | Pbyteslength) ->
      tag_int(string_length (transl arg))
  (* Array operations *)
  | Parraylength kind ->
      begin match kind with
        Pgenarray ->
          let len =
            if wordsize_shift = numfloat_shift then
              Cop(Clsr, [header(transl arg); Cconst_int wordsize_shift])
            else
              bind "header" (header(transl arg)) (fun hdr ->
                Cifthenelse(is_addr_array_hdr hdr,
                            Cop(Clsr, [hdr; Cconst_int wordsize_shift]),
                            Cop(Clsr, [hdr; Cconst_int numfloat_shift]))) in
          Cop(Cor, [len; Cconst_int 1])
      | Paddrarray | Pintarray ->
          Cop(Cor, [addr_array_length(header(transl arg)); Cconst_int 1])
      | Pfloatarray ->
          Cop(Cor, [float_array_length(header(transl arg)); Cconst_int 1])
      end
  (* Boolean operations *)
  | Pnot ->
      Cop(Csubi, [Cconst_int 4; transl arg]) (* 1 -> 3, 3 -> 1 *)
  (* Test integer/block *)
  | Pisint ->
      tag_int(Cop(Cand, [transl arg; Cconst_int 1]))
  (* Boxed integers *)
  | Pbintofint bi ->
      box_int bi (untag_int (transl arg))
  | Pintofbint bi ->
      force_tag_int (transl_unbox_int bi arg)
  | Pcvtbint(bi1, bi2) ->
      box_int bi2 (transl_unbox_int bi1 arg)
  | Pnegbint bi ->
      box_int bi (Cop(Csubi, [Cconst_int 0; transl_unbox_int bi arg]))
  | Pbbswap bi ->
      let prim = match bi with
        | Pnativeint -> "nativeint"
        | Pint32 -> "int32"
        | Pint64 -> "int64" in
      box_int bi (Cop(Cextcall(Printf.sprintf "caml_%s_direct_bswap" prim,
                               typ_int, false, Debuginfo.none),
                      [transl_unbox_int bi arg]))
  | Pbswap16 ->
      tag_int (Cop(Cextcall("caml_bswap16_direct", typ_int, false,
                            Debuginfo.none),
                   [untag_int (transl arg)]))
  | _ ->
      fatal_error "Cmmgen.transl_prim_1"

and transl_prim_2 p arg1 arg2 dbg =
  match p with
  (* Heap operations *)
    Psetfield(n, ptr, _) ->
      if ptr then
        return_unit(Cop(Cextcall("caml_modify", typ_void, false,Debuginfo.none),
                        [field_address (transl arg1) n; transl arg2]))
      else
        return_unit(set_field (transl arg1) n (transl arg2))
  | Psetfloatfield (n,_) ->
      let ptr = transl arg1 in
      return_unit(
        Cop(Cstore Double_u,
            [if n = 0 then ptr
                       else Cop(Cadda, [ptr; Cconst_int(n * size_float)]);
                   transl_unbox_float arg2]))

  (* Boolean operations *)
  | Psequand ->
      Cifthenelse(test_bool(transl arg1), transl arg2, Cconst_int 1)
      (* let id = Ident.create "res1" in
      Clet(id, transl arg1,
           Cifthenelse(test_bool(Cvar id), transl arg2, Cvar id)) *)
  | Psequor ->
      Cifthenelse(test_bool(transl arg1), Cconst_int 3, transl arg2)

  (* Integer operations *)
  | Paddint ->
      decr_int(add_int (transl arg1) (transl arg2))
  | Psubint ->
      incr_int(sub_int (transl arg1) (transl arg2))
  | Pmulint ->
      incr_int(mul_int (decr_int(transl arg1)) (untag_int(transl arg2)))
  | Pdivint ->
      tag_int(div_int (untag_int(transl arg1)) (untag_int(transl arg2)) dbg)
  | Pmodint ->
      tag_int(mod_int (untag_int(transl arg1)) (untag_int(transl arg2)) dbg)
  | Pandint ->
      Cop(Cand, [transl arg1; transl arg2])
  | Porint ->
      Cop(Cor, [transl arg1; transl arg2])
  | Pxorint ->
      Cop(Cor, [Cop(Cxor, [ignore_low_bit_int(transl arg1);
                           ignore_low_bit_int(transl arg2)]);
                Cconst_int 1])
  | Plslint ->
      incr_int(lsl_int (decr_int(transl arg1)) (untag_int(transl arg2)))
  | Plsrint ->
      Cop(Cor, [lsr_int (transl arg1) (untag_int(transl arg2));
                Cconst_int 1])
  | Pasrint ->
      Cop(Cor, [asr_int (transl arg1) (untag_int(transl arg2));
                Cconst_int 1])
  | Pintcomp cmp ->
      tag_int(Cop(Ccmpi(transl_comparison cmp), [transl arg1; transl arg2]))
  | Pisout ->
      transl_isout (transl arg1) (transl arg2)
  (* Float operations *)
  | Paddfloat ->
      box_float(Cop(Caddf,
                    [transl_unbox_float arg1; transl_unbox_float arg2]))
  | Psubfloat ->
      box_float(Cop(Csubf,
                    [transl_unbox_float arg1; transl_unbox_float arg2]))
  | Pmulfloat ->
      box_float(Cop(Cmulf,
                    [transl_unbox_float arg1; transl_unbox_float arg2]))
  | Pdivfloat ->
      box_float(Cop(Cdivf,
                    [transl_unbox_float arg1; transl_unbox_float arg2]))
  | Pfloatcomp cmp ->
      tag_int(Cop(Ccmpf(transl_comparison cmp),
                  [transl_unbox_float arg1; transl_unbox_float arg2]))

  (* String operations *)
  | (Pstringrefu | Pbytesrefu) ->
      tag_int(Cop(Cload Byte_unsigned,
                  [add_int (transl arg1) (untag_int(transl arg2))]))
  | (Pstringrefs | Pbytesrefs) ->
      tag_int
        (bind "str" (transl arg1) (fun str ->
          bind "index" (untag_int (transl arg2)) (fun idx ->
            Csequence(
              make_checkbound dbg [string_length str; idx],
              Cop(Cload Byte_unsigned, [add_int str idx])))))

  | Pstring_load_16(unsafe) ->
     tag_int
       (bind "str" (transl arg1) (fun str ->
        bind "index" (untag_int (transl arg2)) (fun idx ->
          check_bound unsafe dbg (sub_int (string_length str) (Cconst_int 1))
                      idx (unaligned_load_16 str idx))))

  | Pbigstring_load_16(unsafe) ->
     tag_int
       (bind "ba" (transl arg1) (fun ba ->
        bind "index" (untag_int (transl arg2)) (fun idx ->
        bind "ba_data" (Cop(Cload Word, [field_address ba 1])) (fun ba_data ->
          check_bound unsafe dbg (sub_int (Cop(Cload Word,[field_address ba 5]))
                                          (Cconst_int 1)) idx
                      (unaligned_load_16 ba_data idx)))))

  | Pstring_load_32(unsafe) ->
     box_int Pint32
       (bind "str" (transl arg1) (fun str ->
        bind "index" (untag_int (transl arg2)) (fun idx ->
          check_bound unsafe dbg (sub_int (string_length str) (Cconst_int 3))
                      idx (unaligned_load_32 str idx))))

  | Pbigstring_load_32(unsafe) ->
     box_int Pint32
       (bind "ba" (transl arg1) (fun ba ->
        bind "index" (untag_int (transl arg2)) (fun idx ->
        bind "ba_data" (Cop(Cload Word, [field_address ba 1])) (fun ba_data ->
          check_bound unsafe dbg (sub_int (Cop(Cload Word,[field_address ba 5]))
                                          (Cconst_int 3)) idx
                      (unaligned_load_32 ba_data idx)))))

  | Pstring_load_64(unsafe) ->
     box_int Pint64
       (bind "str" (transl arg1) (fun str ->
        bind "index" (untag_int (transl arg2)) (fun idx ->
          check_bound unsafe dbg (sub_int (string_length str) (Cconst_int 7))
                      idx (unaligned_load_64 str idx))))

  | Pbigstring_load_64(unsafe) ->
     box_int Pint64
       (bind "ba" (transl arg1) (fun ba ->
        bind "index" (untag_int (transl arg2)) (fun idx ->
        bind "ba_data" (Cop(Cload Word, [field_address ba 1])) (fun ba_data ->
          check_bound unsafe dbg (sub_int (Cop(Cload Word,[field_address ba 5]))
                                          (Cconst_int 7)) idx
                      (unaligned_load_64 ba_data idx)))))

  (* Array operations *)
  | Parrayrefu kind ->
      begin match kind with
        Pgenarray ->
          bind "arr" (transl arg1) (fun arr ->
            bind "index" (transl arg2) (fun idx ->
              Cifthenelse(is_addr_array_ptr arr,
                          addr_array_ref arr idx,
                          float_array_ref arr idx)))
      | Paddrarray | Pintarray ->
          addr_array_ref (transl arg1) (transl arg2)
      | Pfloatarray ->
          float_array_ref (transl arg1) (transl arg2)
      end
  | Parrayrefs kind ->
      begin match kind with
      | Pgenarray ->
          bind "index" (transl arg2) (fun idx ->
          bind "arr" (transl arg1) (fun arr ->
          bind "header" (header arr) (fun hdr ->
            if wordsize_shift = numfloat_shift then
              Csequence(make_checkbound dbg [addr_array_length hdr; idx],
                        Cifthenelse(is_addr_array_hdr hdr,
                                    addr_array_ref arr idx,
                                    float_array_ref arr idx))
            else
              Cifthenelse(is_addr_array_hdr hdr,
                Csequence(make_checkbound dbg [addr_array_length hdr; idx],
                          addr_array_ref arr idx),
                Csequence(make_checkbound dbg [float_array_length hdr; idx],
                          float_array_ref arr idx)))))
      | Paddrarray | Pintarray ->
          bind "index" (transl arg2) (fun idx ->
          bind "arr" (transl arg1) (fun arr ->
            Csequence(make_checkbound dbg [addr_array_length(header arr); idx],
                      addr_array_ref arr idx)))
      | Pfloatarray ->
          box_float(
            bind "index" (transl arg2) (fun idx ->
            bind "arr" (transl arg1) (fun arr ->
              Csequence(make_checkbound dbg
                                        [float_array_length(header arr); idx],
                        unboxed_float_array_ref arr idx))))
      end

  (* Operations on bitvects *)
  | Pbittest ->
      bind "index" (untag_int(transl arg2)) (fun idx ->
        tag_int(
          Cop(Cand, [Cop(Clsr, [Cop(Cload Byte_unsigned,
                                    [add_int (transl arg1)
                                      (Cop(Clsr, [idx; Cconst_int 3]))]);
                                Cop(Cand, [idx; Cconst_int 7])]);
                     Cconst_int 1])))

  (* Boxed integers *)
  | Paddbint bi ->
      box_int bi (Cop(Caddi,
                      [transl_unbox_int bi arg1; transl_unbox_int bi arg2]))
  | Psubbint bi ->
      box_int bi (Cop(Csubi,
                      [transl_unbox_int bi arg1; transl_unbox_int bi arg2]))
  | Pmulbint bi ->
      box_int bi (Cop(Cmuli,
                      [transl_unbox_int bi arg1; transl_unbox_int bi arg2]))
  | Pdivbint bi ->
      box_int bi (safe_div_bi
                      (transl_unbox_int bi arg1) (transl_unbox_int bi arg2)
                      bi dbg)
  | Pmodbint bi ->
      box_int bi (safe_mod_bi
                      (transl_unbox_int bi arg1) (transl_unbox_int bi arg2)
                      bi dbg)
  | Pandbint bi ->
      box_int bi (Cop(Cand,
                     [transl_unbox_int bi arg1; transl_unbox_int bi arg2]))
  | Porbint bi ->
      box_int bi (Cop(Cor,
                     [transl_unbox_int bi arg1; transl_unbox_int bi arg2]))
  | Pxorbint bi ->
      box_int bi (Cop(Cxor,
                     [transl_unbox_int bi arg1; transl_unbox_int bi arg2]))
  | Plslbint bi ->
      box_int bi (Cop(Clsl,
                     [transl_unbox_int bi arg1; untag_int(transl arg2)]))
  | Plsrbint bi ->
      box_int bi (Cop(Clsr,
                     [make_unsigned_int bi (transl_unbox_int bi arg1);
                      untag_int(transl arg2)]))
  | Pasrbint bi ->
      box_int bi (Cop(Casr,
                     [transl_unbox_int bi arg1; untag_int(transl arg2)]))
  | Pbintcomp(bi, cmp) ->
      tag_int (Cop(Ccmpi(transl_comparison cmp),
                     [transl_unbox_int bi arg1; transl_unbox_int bi arg2]))
  | _ ->
      fatal_error "Cmmgen.transl_prim_2"

and transl_prim_3 p arg1 arg2 arg3 dbg =
  match p with
  (* String operations *)
  | (Pstringsetu | Pbytessetu) ->
      return_unit(Cop(Cstore Byte_unsigned,
                      [add_int (transl arg1) (untag_int(transl arg2));
                        untag_int(transl arg3)]))
  | (Pstringsets | Pbytessets) ->
      return_unit
        (bind "str" (transl arg1) (fun str ->
          bind "index" (untag_int (transl arg2)) (fun idx ->
            Csequence(
              make_checkbound dbg [string_length str; idx],
              Cop(Cstore Byte_unsigned,
                  [add_int str idx; untag_int(transl arg3)])))))

  (* Array operations *)
  | Parraysetu kind ->
      return_unit(begin match kind with
        Pgenarray ->
          bind "newval" (transl arg3) (fun newval ->
            bind "index" (transl arg2) (fun index ->
              bind "arr" (transl arg1) (fun arr ->
                Cifthenelse(is_addr_array_ptr arr,
                            addr_array_set arr index newval,
                            float_array_set arr index (unbox_float newval)))))
      | Paddrarray ->
          addr_array_set (transl arg1) (transl arg2) (transl arg3)
      | Pintarray ->
          int_array_set (transl arg1) (transl arg2) (transl arg3)
      | Pfloatarray ->
          float_array_set (transl arg1) (transl arg2) (transl_unbox_float arg3)
      end)
  | Parraysets kind ->
      return_unit(begin match kind with
      | Pgenarray ->
          bind "newval" (transl arg3) (fun newval ->
          bind "index" (transl arg2) (fun idx ->
          bind "arr" (transl arg1) (fun arr ->
          bind "header" (header arr) (fun hdr ->
            if wordsize_shift = numfloat_shift then
              Csequence(make_checkbound dbg [addr_array_length hdr; idx],
                        Cifthenelse(is_addr_array_hdr hdr,
                                    addr_array_set arr idx newval,
                                    float_array_set arr idx
                                                    (unbox_float newval)))
            else
              Cifthenelse(is_addr_array_hdr hdr,
                Csequence(make_checkbound dbg [addr_array_length hdr; idx],
                          addr_array_set arr idx newval),
                Csequence(make_checkbound dbg [float_array_length hdr; idx],
                          float_array_set arr idx
                                          (unbox_float newval)))))))
      | Paddrarray ->
          bind "newval" (transl arg3) (fun newval ->
          bind "index" (transl arg2) (fun idx ->
          bind "arr" (transl arg1) (fun arr ->
            Csequence(make_checkbound dbg [addr_array_length(header arr); idx],
                      addr_array_set arr idx newval))))
      | Pintarray ->
          bind "newval" (transl arg3) (fun newval ->
          bind "index" (transl arg2) (fun idx ->
          bind "arr" (transl arg1) (fun arr ->
            Csequence(make_checkbound dbg [addr_array_length(header arr); idx],
                      int_array_set arr idx newval))))
      | Pfloatarray ->
          bind "newval" (transl_unbox_float arg3) (fun newval ->
          bind "index" (transl arg2) (fun idx ->
          bind "arr" (transl arg1) (fun arr ->
            Csequence(make_checkbound dbg [float_array_length(header arr);idx],
                      float_array_set arr idx newval))))
      end)

  | Pstring_set_16(unsafe) ->
     return_unit
       (bind "str" (transl arg1) (fun str ->
        bind "index" (untag_int (transl arg2)) (fun idx ->
        bind "newval" (untag_int (transl arg3)) (fun newval ->
          check_bound unsafe dbg (sub_int (string_length str) (Cconst_int 1))
                      idx (unaligned_set_16 str idx newval)))))

  | Pbigstring_set_16(unsafe) ->
     return_unit
       (bind "ba" (transl arg1) (fun ba ->
        bind "index" (untag_int (transl arg2)) (fun idx ->
        bind "newval" (untag_int (transl arg3)) (fun newval ->
        bind "ba_data" (Cop(Cload Word, [field_address ba 1])) (fun ba_data ->
          check_bound unsafe dbg (sub_int (Cop(Cload Word,[field_address ba 5]))
                                          (Cconst_int 1))
                      idx (unaligned_set_16 ba_data idx newval))))))

  | Pstring_set_32(unsafe) ->
     return_unit
       (bind "str" (transl arg1) (fun str ->
        bind "index" (untag_int (transl arg2)) (fun idx ->
        bind "newval" (transl_unbox_int Pint32 arg3) (fun newval ->
          check_bound unsafe dbg (sub_int (string_length str) (Cconst_int 3))
                      idx (unaligned_set_32 str idx newval)))))

  | Pbigstring_set_32(unsafe) ->
     return_unit
       (bind "ba" (transl arg1) (fun ba ->
        bind "index" (untag_int (transl arg2)) (fun idx ->
        bind "newval" (transl_unbox_int Pint32 arg3) (fun newval ->
        bind "ba_data" (Cop(Cload Word, [field_address ba 1])) (fun ba_data ->
          check_bound unsafe dbg (sub_int (Cop(Cload Word,[field_address ba 5]))
                                          (Cconst_int 3))
                      idx (unaligned_set_32 ba_data idx newval))))))

  | Pstring_set_64(unsafe) ->
     return_unit
       (bind "str" (transl arg1) (fun str ->
        bind "index" (untag_int (transl arg2)) (fun idx ->
        bind "newval" (transl_unbox_int Pint64 arg3) (fun newval ->
          check_bound unsafe dbg (sub_int (string_length str) (Cconst_int 7))
                      idx (unaligned_set_64 str idx newval)))))

  | Pbigstring_set_64(unsafe) ->
     return_unit
       (bind "ba" (transl arg1) (fun ba ->
        bind "index" (untag_int (transl arg2)) (fun idx ->
        bind "newval" (transl_unbox_int Pint64 arg3) (fun newval ->
        bind "ba_data" (Cop(Cload Word, [field_address ba 1])) (fun ba_data ->
          check_bound unsafe dbg (sub_int (Cop(Cload Word,[field_address ba 5]))
                                          (Cconst_int 7)) idx
                      (unaligned_set_64 ba_data idx newval))))))

  | _ ->
    fatal_error "Cmmgen.transl_prim_3"

and transl_unbox_float = function
    Uconst(Uconst_ref(_, Uconst_float f)) -> Cconst_float f
  | exp -> unbox_float(transl exp)

and transl_unbox_int bi = function
    Uconst(Uconst_ref(_, Uconst_int32 n)) ->
      Cconst_natint (Nativeint.of_int32 n)
  | Uconst(Uconst_ref(_, Uconst_nativeint n)) ->
      Cconst_natint n
  | Uconst(Uconst_ref(_, Uconst_int64 n)) ->
      assert (size_int = 8); Cconst_natint (Int64.to_nativeint n)
  | Uprim(Pbintofint bi',[Uconst(Uconst_int i)],_) when bi = bi' ->
      Cconst_int i
  | exp -> unbox_int bi (transl exp)

and transl_unbox_let box_fn unbox_fn transl_unbox_fn box_chunk box_offset
                     id exp body =
  let unboxed_id = Ident.create (Ident.name id) in
  let trbody1 = transl body in
  let (trbody2, need_boxed, is_assigned) =
    subst_boxed_number unbox_fn id unboxed_id box_chunk box_offset trbody1 in
  if need_boxed && is_assigned then
    Clet(id, transl exp, trbody1)
  else
    Clet(unboxed_id, transl_unbox_fn exp,
         if need_boxed
         then Clet(id, box_fn(Cvar unboxed_id), trbody2)
         else trbody2)

and make_catch ncatch body handler = match body with
| Cexit (nexit,[]) when nexit=ncatch -> handler
| _ ->  Ccatch (ncatch, [], body, handler)

and make_catch2 mk_body handler = match handler with
| Cexit (_,[])|Ctuple []|Cconst_int _|Cconst_pointer _ ->
    mk_body handler
| _ ->
    let nfail = next_raise_count () in
    make_catch
      nfail
      (mk_body (Cexit (nfail,[])))
      handler

and exit_if_true cond nfail otherwise =
  match cond with
  | Uconst (Uconst_ptr 0) -> otherwise
  | Uconst (Uconst_ptr 1) -> Cexit (nfail,[])
  | Uprim(Psequor, [arg1; arg2], _) ->
      exit_if_true arg1 nfail (exit_if_true arg2 nfail otherwise)
  | Uprim(Psequand, _, _) ->
      begin match otherwise with
      | Cexit (raise_num,[]) ->
          exit_if_false cond (Cexit (nfail,[])) raise_num
      | _ ->
          let raise_num = next_raise_count () in
          make_catch
            raise_num
            (exit_if_false cond (Cexit (nfail,[])) raise_num)
            otherwise
      end
  | Uprim(Pnot, [arg], _) ->
      exit_if_false arg otherwise nfail
  | Uifthenelse (cond, ifso, ifnot) ->
      make_catch2
        (fun shared ->
          Cifthenelse
            (test_bool (transl cond),
             exit_if_true ifso nfail shared,
             exit_if_true ifnot nfail shared))
        otherwise
  | _ ->
      Cifthenelse(test_bool(transl cond), Cexit (nfail, []), otherwise)

and exit_if_false cond otherwise nfail =
  match cond with
  | Uconst (Uconst_ptr 0) -> Cexit (nfail,[])
  | Uconst (Uconst_ptr 1) -> otherwise
  | Uprim(Psequand, [arg1; arg2], _) ->
      exit_if_false arg1 (exit_if_false arg2 otherwise nfail) nfail
  | Uprim(Psequor, _, _) ->
      begin match otherwise with
      | Cexit (raise_num,[]) ->
          exit_if_true cond raise_num (Cexit (nfail,[]))
      | _ ->
          let raise_num = next_raise_count () in
          make_catch
            raise_num
            (exit_if_true cond raise_num (Cexit (nfail,[])))
            otherwise
      end
  | Uprim(Pnot, [arg], _) ->
      exit_if_true arg nfail otherwise
  | Uifthenelse (cond, ifso, ifnot) ->
      make_catch2
        (fun shared ->
          Cifthenelse
            (test_bool (transl cond),
             exit_if_false ifso shared nfail,
             exit_if_false ifnot shared nfail))
        otherwise
  | _ ->
      Cifthenelse(test_bool(transl cond), otherwise, Cexit (nfail, []))

and transl_switch arg index cases = match Array.length cases with
| 0 -> fatal_error "Cmmgen.transl_switch"
| 1 -> transl cases.(0)
| _ ->
    let cases = Array.map transl cases in
    let store = StoreExp.mk_store () in
    let index =
      Array.map
        (fun j -> store.Switch.act_store cases.(j))
        index in
    let n_index = Array.length index in
    let inters = ref []
    and this_high = ref (n_index-1)
    and this_low = ref (n_index-1)
    and this_act = ref index.(n_index-1) in
    for i = n_index-2 downto 0 do
      let act = index.(i) in
      if act = !this_act then
        decr this_low
      else begin
        inters := (!this_low, !this_high, !this_act) :: !inters ;
        this_high := i ;
        this_low := i ;
        this_act := act
      end
    done ;
    inters := (0, !this_high, !this_act) :: !inters ;
    match !inters with
    | [_] -> cases.(0)
    | inters ->
        bind "switcher" arg
          (fun a ->
            SwitcherBlocks.zyva
              (0,n_index-1)
              a
              (Array.of_list inters) store)

and transl_letrec bindings cont =
  let bsz =
    List.map (fun (id, exp) -> (id, exp, expr_size Ident.empty exp)) bindings in
  let op_alloc prim sz =
    Cop(Cextcall(prim, typ_addr, true, Debuginfo.none), [int_const sz]) in
  let rec init_blocks = function
    | [] -> fill_nonrec bsz
    | (id, exp, RHS_block sz) :: rem ->
        Clet(id, op_alloc "caml_alloc_dummy" sz, init_blocks rem)
    | (id, exp, RHS_floatblock sz) :: rem ->
        Clet(id, op_alloc "caml_alloc_dummy_float" sz, init_blocks rem)
    | (id, exp, RHS_nonrec) :: rem ->
        Clet (id, Cconst_int 0, init_blocks rem)
  and fill_nonrec = function
    | [] -> fill_blocks bsz
    | (id, exp, (RHS_block _ | RHS_floatblock _)) :: rem ->
        fill_nonrec rem
    | (id, exp, RHS_nonrec) :: rem ->
        Clet (id, transl exp, fill_nonrec rem)
  and fill_blocks = function
    | [] -> cont
    | (id, exp, (RHS_block _ | RHS_floatblock _)) :: rem ->
        let op =
          Cop(Cextcall("caml_update_dummy", typ_void, false, Debuginfo.none),
              [Cvar id; transl exp]) in
        Csequence(op, fill_blocks rem)
    | (id, exp, RHS_nonrec) :: rem ->
        fill_blocks rem
  in init_blocks bsz

(* Translate a function definition *)

let transl_function f =
  Cfunction {fun_name = f.label;
             fun_args = List.map (fun id -> (id, typ_addr)) f.params;
             fun_body = transl f.body;
             fun_fast = !Clflags.optimize_for_speed;
             fun_dbg  = f.dbg; }

(* Translate all function definitions *)

module StringSet =
  Set.Make(struct
    type t = string
    let compare (x:t) y = compare x y
  end)

let rec transl_all_functions already_translated cont =
  try
    let f = Queue.take functions in
    if StringSet.mem f.label already_translated then
      transl_all_functions already_translated cont
    else begin
      transl_all_functions
        (StringSet.add f.label already_translated)
        (transl_function f :: cont)
    end
  with Queue.Empty ->
    cont

(* Emit structured constants *)

let rec emit_structured_constant symb cst cont =
  let emit_block white_header symb cont =
    (* Headers for structured constants must be marked black in case we
       are in no-naked-pointers mode.  See [caml_darken]. *)
    let black_header = Nativeint.logor white_header caml_black in
    Cint black_header :: Cdefine_symbol symb :: cont
  in
  match cst with
  | Uconst_float s->
      emit_block float_header symb (Cdouble s :: cont)
  | Uconst_string s ->
      emit_block (string_header (String.length s)) symb
        (emit_string_constant s cont)
  | Uconst_int32 n ->
      emit_block boxedint32_header symb
        (emit_boxed_int32_constant n cont)
  | Uconst_int64 n ->
      emit_block boxedint64_header symb
        (emit_boxed_int64_constant n cont)
  | Uconst_nativeint n ->
      emit_block boxedintnat_header symb
        (emit_boxed_nativeint_constant n cont)
  | Uconst_block (tag, csts) ->
      let cont = List.fold_right emit_constant csts cont in
      emit_block (block_header tag (List.length csts)) symb cont
  | Uconst_float_array fields ->
      emit_block (floatarray_header (List.length fields)) symb
        (Misc.map_end (fun f -> Cdouble f) fields cont)

and emit_constant cst cont =
  match cst with
  | Uconst_int n | Uconst_ptr n ->
      Cint(Nativeint.add (Nativeint.shift_left (Nativeint.of_int n) 1) 1n)
      :: cont
  | Uconst_ref (label, _) ->
      Csymbol_address label :: cont

and emit_string_constant s cont =
  let n = size_int - 1 - (String.length s) mod size_int in
  Cstring s :: Cskip n :: Cint8 n :: cont

and emit_boxed_int32_constant n cont =
  let n = Nativeint.of_int32 n in
  if size_int = 8 then
    Csymbol_address("caml_int32_ops") :: Cint32 n :: Cint32 0n :: cont
  else
    Csymbol_address("caml_int32_ops") :: Cint n :: cont

and emit_boxed_nativeint_constant n cont =
  Csymbol_address("caml_nativeint_ops") :: Cint n :: cont

and emit_boxed_int64_constant n cont =
  let lo = Int64.to_nativeint n in
  if size_int = 8 then
    Csymbol_address("caml_int64_ops") :: Cint lo :: cont
  else begin
    let hi = Int64.to_nativeint (Int64.shift_right n 32) in
    if big_endian then
      Csymbol_address("caml_int64_ops") :: Cint hi :: Cint lo :: cont
    else
      Csymbol_address("caml_int64_ops") :: Cint lo :: Cint hi :: cont
  end

(* Emit constant closures *)

let emit_constant_closure symb fundecls cont =
  match fundecls with
    [] -> assert false
  | f1 :: remainder ->
      let rec emit_others pos = function
        [] -> cont
      | f2 :: rem ->
          if f2.arity = 1 then
            Cint(infix_header pos) ::
            Csymbol_address f2.label ::
            Cint 3n ::
            emit_others (pos + 3) rem
          else
            Cint(infix_header pos) ::
            Csymbol_address(curry_function f2.arity) ::
            Cint(Nativeint.of_int (f2.arity lsl 1 + 1)) ::
            Csymbol_address f2.label ::
            emit_others (pos + 4) rem in
      Cint(black_closure_header (fundecls_size fundecls)) ::
      Cdefine_symbol symb ::
      if f1.arity = 1 then
        Csymbol_address f1.label ::
        Cint 3n ::
        emit_others 3 remainder
      else
        Csymbol_address(curry_function f1.arity) ::
        Cint(Nativeint.of_int (f1.arity lsl 1 + 1)) ::
        Csymbol_address f1.label ::
        emit_others 4 remainder

(* Emit all structured constants *)

let emit_all_constants cont =
  let c = ref cont in
  List.iter
    (fun (lbl, global, cst) ->
       let cst = emit_structured_constant lbl cst [] in
       let cst = if global then
         Cglobal_symbol lbl :: cst
       else cst in
         c:= Cdata(cst):: !c)
    (Compilenv.structured_constants());
  List.iter
    (fun (symb, fundecls) ->
        c := Cdata(emit_constant_closure symb fundecls []) :: !c)
    !constant_closures;
  constant_closures := [];
  !c

(* Translate a compilation unit *)

let compunit size ulam =
  let glob = Compilenv.make_symbol None in
  let init_code = transl ulam in
  let c1 = [Cfunction {fun_name = Compilenv.make_symbol (Some "entry");
                       fun_args = [];
                       fun_body = init_code; fun_fast = false;
                       fun_dbg  = Debuginfo.none }] in
  let c2 = transl_all_functions StringSet.empty c1 in
  let c3 = emit_all_constants c2 in
  let space =
    (* These words will be registered as roots and as such must contain
       valid values, in case we are in no-naked-pointers mode.  Likewise
       the block header must be black, below (see [caml_darken]), since
       the overall record may be referenced. *)
    Array.to_list
      (Array.init size (fun _index ->
        Cint (Nativeint.of_int 1 (* Val_unit *))))
  in
  Cdata ([Cint(black_block_header 0 size);
         Cglobal_symbol glob;
         Cdefine_symbol glob] @ space) :: c3

(*
CAMLprim value caml_cache_public_method (value meths, value tag, value *cache)
{
  int li = 3, hi = Field(meths,0), mi;
  while (li < hi) { // no need to check the 1st time
    mi = ((li+hi) >> 1) | 1;
    if (tag < Field(meths,mi)) hi = mi-2;
    else li = mi;
  }
  *cache = (li-3)*sizeof(value)+1;
  return Field (meths, li-1);
}
*)

let cache_public_method meths tag cache =
  let raise_num = next_raise_count () in
  let li = Ident.create "li" and hi = Ident.create "hi"
  and mi = Ident.create "mi" and tagged = Ident.create "tagged" in
  Clet (
  li, Cconst_int 3,
  Clet (
  hi, Cop(Cload Word, [meths]),
  Csequence(
  Ccatch
    (raise_num, [],
     Cloop
       (Clet(
        mi,
        Cop(Cor,
            [Cop(Clsr, [Cop(Caddi, [Cvar li; Cvar hi]); Cconst_int 1]);
             Cconst_int 1]),
        Csequence(
        Cifthenelse
          (Cop (Ccmpi Clt,
                [tag;
                 Cop(Cload Word,
                     [Cop(Cadda,
                          [meths; lsl_const (Cvar mi) log2_size_addr])])]),
           Cassign(hi, Cop(Csubi, [Cvar mi; Cconst_int 2])),
           Cassign(li, Cvar mi)),
        Cifthenelse
          (Cop(Ccmpi Cge, [Cvar li; Cvar hi]), Cexit (raise_num, []),
           Ctuple [])))),
     Ctuple []),
  Clet (
  tagged, Cop(Cadda, [lsl_const (Cvar li) log2_size_addr;
                      Cconst_int(1 - 3 * size_addr)]),
  Csequence(Cop (Cstore Word, [cache; Cvar tagged]),
            Cvar tagged)))))

(* Generate an application function:
     (defun caml_applyN (a1 ... aN clos)
       (if (= clos.arity N)
         (app clos.direct a1 ... aN clos)
         (let (clos1 (app clos.code a1 clos)
               clos2 (app clos1.code a2 clos)
               ...
               closN-1 (app closN-2.code aN-1 closN-2))
           (app closN-1.code aN closN-1))))
*)

let apply_function_body arity =
  let arg = Array.make arity (Ident.create "arg") in
  for i = 1 to arity - 1 do arg.(i) <- Ident.create "arg" done;
  let clos = Ident.create "clos" in
  let rec app_fun clos n =
    if n = arity-1 then
      Cop(Capply(typ_addr, Debuginfo.none),
          [get_field (Cvar clos) 0; Cvar arg.(n); Cvar clos])
    else begin
      let newclos = Ident.create "clos" in
      Clet(newclos,
           Cop(Capply(typ_addr, Debuginfo.none),
               [get_field (Cvar clos) 0; Cvar arg.(n); Cvar clos]),
           app_fun newclos (n+1))
    end in
  let args = Array.to_list arg in
  let all_args = args @ [clos] in
  (args, clos,
   if arity = 1 then app_fun clos 0 else
   Cifthenelse(
   Cop(Ccmpi Ceq, [get_field (Cvar clos) 1; int_const arity]),
   Cop(Capply(typ_addr, Debuginfo.none),
       get_field (Cvar clos) 2 :: List.map (fun s -> Cvar s) all_args),
   app_fun clos 0))

let send_function arity =
  let (args, clos', body) = apply_function_body (1+arity) in
  let cache = Ident.create "cache"
  and obj = List.hd args
  and tag = Ident.create "tag" in
  let clos =
    let cache = Cvar cache and obj = Cvar obj and tag = Cvar tag in
    let meths = Ident.create "meths" and cached = Ident.create "cached" in
    let real = Ident.create "real" in
    let mask = get_field (Cvar meths) 1 in
    let cached_pos = Cvar cached in
    let tag_pos = Cop(Cadda, [Cop (Cadda, [cached_pos; Cvar meths]);
                              Cconst_int(3*size_addr-1)]) in
    let tag' = Cop(Cload Word, [tag_pos]) in
    Clet (
    meths, Cop(Cload Word, [obj]),
    Clet (
    cached, Cop(Cand, [Cop(Cload Word, [cache]); mask]),
    Clet (
    real,
    Cifthenelse(Cop(Ccmpa Cne, [tag'; tag]),
                cache_public_method (Cvar meths) tag cache,
                cached_pos),
    Cop(Cload Word, [Cop(Cadda, [Cop (Cadda, [Cvar real; Cvar meths]);
                                 Cconst_int(2*size_addr-1)])]))))

  in
  let body = Clet(clos', clos, body) in
  let fun_args =
    [obj, typ_addr; tag, typ_int; cache, typ_addr]
    @ List.map (fun id -> (id, typ_addr)) (List.tl args) in
  Cfunction
   {fun_name = "caml_send" ^ string_of_int arity;
    fun_args = fun_args;
    fun_body = body;
    fun_fast = true;
    fun_dbg  = Debuginfo.none }

let apply_function arity =
  let (args, clos, body) = apply_function_body arity in
  let all_args = args @ [clos] in
  Cfunction
   {fun_name = "caml_apply" ^ string_of_int arity;
    fun_args = List.map (fun id -> (id, typ_addr)) all_args;
    fun_body = body;
    fun_fast = true;
    fun_dbg  = Debuginfo.none }

(* Generate tuplifying functions:
      (defun caml_tuplifyN (arg clos)
        (app clos.direct #0(arg) ... #N-1(arg) clos)) *)

let tuplify_function arity =
  let arg = Ident.create "arg" in
  let clos = Ident.create "clos" in
  let rec access_components i =
    if i >= arity
    then []
    else get_field (Cvar arg) i :: access_components(i+1) in
  Cfunction
   {fun_name = "caml_tuplify" ^ string_of_int arity;
    fun_args = [arg, typ_addr; clos, typ_addr];
    fun_body =
      Cop(Capply(typ_addr, Debuginfo.none),
          get_field (Cvar clos) 2 :: access_components 0 @ [Cvar clos]);
    fun_fast = true;
    fun_dbg  = Debuginfo.none }

(* Generate currying functions:
      (defun caml_curryN (arg clos)
         (alloc HDR caml_curryN_1 <arity (N-1)> caml_curry_N_1_app arg clos))
      (defun caml_curryN_1 (arg clos)
         (alloc HDR caml_curryN_2 <arity (N-2)> caml_curry_N_2_app arg clos))
      ...
      (defun caml_curryN_N-1 (arg clos)
         (let (closN-2 clos.vars[1]
               closN-3 closN-2.vars[1]
               ...
               clos1 clos2.vars[1]
               clos clos1.vars[1])
           (app clos.direct
                clos1.vars[0] ... closN-2.vars[0] clos.vars[0] arg clos)))

    Special "shortcut" functions are also generated to handle the
    case where a partially applied function is applied to all remaining
    arguments in one go.  For instance:
      (defun caml_curry_N_1_app (arg2 ... argN clos)
        (let clos' clos.vars[1]
           (app clos'.direct clos.vars[0] arg2 ... argN clos')))

    Those shortcuts may lead to a quadratic number of application
    primitives being generated in the worst case, which resulted in
    linking time blowup in practice (PR#5933), so we only generate and
    use them when below a fixed arity 'max_arity_optimized'.
*)

let max_arity_optimized = 15
let final_curry_function arity =
  let last_arg = Ident.create "arg" in
  let last_clos = Ident.create "clos" in
  let rec curry_fun args clos n =
    if n = 0 then
      Cop(Capply(typ_addr, Debuginfo.none),
          get_field (Cvar clos) 2 ::
          args @ [Cvar last_arg; Cvar clos])
    else
      if n = arity - 1 || arity > max_arity_optimized then
        begin
      let newclos = Ident.create "clos" in
      Clet(newclos,
           get_field (Cvar clos) 3,
           curry_fun (get_field (Cvar clos) 2 :: args) newclos (n-1))
        end else
        begin
          let newclos = Ident.create "clos" in
          Clet(newclos,
               get_field (Cvar clos) 4,
               curry_fun (get_field (Cvar clos) 3 :: args) newclos (n-1))
    end in
  Cfunction
   {fun_name = "caml_curry" ^ string_of_int arity ^
               "_" ^ string_of_int (arity-1);
    fun_args = [last_arg, typ_addr; last_clos, typ_addr];
    fun_body = curry_fun [] last_clos (arity-1);
    fun_fast = true;
    fun_dbg  = Debuginfo.none }

let rec intermediate_curry_functions arity num =
  if num = arity - 1 then
    [final_curry_function arity]
  else begin
    let name1 = "caml_curry" ^ string_of_int arity in
    let name2 = if num = 0 then name1 else name1 ^ "_" ^ string_of_int num in
    let arg = Ident.create "arg" and clos = Ident.create "clos" in
    Cfunction
     {fun_name = name2;
      fun_args = [arg, typ_addr; clos, typ_addr];
      fun_body =
         if arity - num > 2 && arity <= max_arity_optimized then
           Cop(Calloc,
               [alloc_closure_header 5;
                Cconst_symbol(name1 ^ "_" ^ string_of_int (num+1));
                int_const (arity - num - 1);
                Cconst_symbol(name1 ^ "_" ^ string_of_int (num+1) ^ "_app");
                Cvar arg; Cvar clos])
         else
           Cop(Calloc,
                     [alloc_closure_header 4;
                      Cconst_symbol(name1 ^ "_" ^ string_of_int (num+1));
                      int_const 1; Cvar arg; Cvar clos]);
      fun_fast = true;
      fun_dbg  = Debuginfo.none }
    ::
      (if arity <= max_arity_optimized && arity - num > 2 then
          let rec iter i =
            if i <= arity then
              let arg = Ident.create (Printf.sprintf "arg%d" i) in
              (arg, typ_addr) :: iter (i+1)
            else []
          in
          let direct_args = iter (num+2) in
          let rec iter i args clos =
            if i = 0 then
              Cop(Capply(typ_addr, Debuginfo.none),
                  (get_field (Cvar clos) 2) :: args @ [Cvar clos])
            else
              let newclos = Ident.create "clos" in
              Clet(newclos,
                   get_field (Cvar clos) 4,
                   iter (i-1) (get_field (Cvar clos) 3 :: args) newclos)
          in
          let cf =
            Cfunction
              {fun_name = name1 ^ "_" ^ string_of_int (num+1) ^ "_app";
               fun_args = direct_args @ [clos, typ_addr];
               fun_body = iter (num+1)
                  (List.map (fun (arg,_) -> Cvar arg) direct_args) clos;
               fun_fast = true;
               fun_dbg = Debuginfo.none }
          in
          cf :: intermediate_curry_functions arity (num+1)
       else
          intermediate_curry_functions arity (num+1))
  end

let curry_function arity =
  if arity >= 0
  then intermediate_curry_functions arity 0
  else [tuplify_function (-arity)]


module IntSet = Set.Make(
  struct
    type t = int
    let compare (x:t) y = compare x y
  end)

let default_apply = IntSet.add 2 (IntSet.add 3 IntSet.empty)
  (* These apply funs are always present in the main program because
     the run-time system needs them (cf. asmrun/<arch>.S) . *)

let generic_functions shared units =
  let (apply,send,curry) =
    List.fold_left
      (fun (apply,send,curry) ui ->
         List.fold_right IntSet.add ui.ui_apply_fun apply,
         List.fold_right IntSet.add ui.ui_send_fun send,
         List.fold_right IntSet.add ui.ui_curry_fun curry)
      (IntSet.empty,IntSet.empty,IntSet.empty)
      units in
  let apply = if shared then apply else IntSet.union apply default_apply in
  let accu = IntSet.fold (fun n accu -> apply_function n :: accu) apply [] in
  let accu = IntSet.fold (fun n accu -> send_function n :: accu) send accu in
  IntSet.fold (fun n accu -> curry_function n @ accu) curry accu

(* Generate the entry point *)

let entry_point namelist =
  let incr_global_inited =
    Cop(Cstore Word,
        [Cconst_symbol "caml_globals_inited";
         Cop(Caddi, [Cop(Cload Word, [Cconst_symbol "caml_globals_inited"]);
                     Cconst_int 1])]) in
  let body =
    List.fold_right
      (fun name next ->
        let entry_sym = Compilenv.make_symbol ~unitname:name (Some "entry") in
        Csequence(Cop(Capply(typ_void, Debuginfo.none),
                         [Cconst_symbol entry_sym]),
                  Csequence(incr_global_inited, next)))
      namelist (Cconst_int 1) in
  Cfunction {fun_name = "caml_program";
             fun_args = [];
             fun_body = body;
             fun_fast = false;
             fun_dbg  = Debuginfo.none }

(* Generate the table of globals *)

let cint_zero = Cint 0n

let global_table namelist =
  let mksym name =
    Csymbol_address (Compilenv.make_symbol ~unitname:name None)
  in
  Cdata(Cglobal_symbol "caml_globals" ::
        Cdefine_symbol "caml_globals" ::
        List.map mksym namelist @
        [cint_zero])

let reference_symbols namelist =
  let mksym name = Csymbol_address name in
  Cdata(List.map mksym namelist)

let global_data name v =
  Cdata(Cglobal_symbol name ::
          emit_structured_constant name
          (Uconst_string (Marshal.to_string v [])) [])

let globals_map v = global_data "caml_globals_map" v

(* Generate the master table of frame descriptors *)

let frame_table namelist =
  let mksym name =
    Csymbol_address (Compilenv.make_symbol ~unitname:name (Some "frametable"))
  in
  Cdata(Cglobal_symbol "caml_frametable" ::
        Cdefine_symbol "caml_frametable" ::
        List.map mksym namelist
        @ [cint_zero])

(* Generate the table of module data and code segments *)

let segment_table namelist symbol begname endname =
  let addsyms name lst =
    Csymbol_address (Compilenv.make_symbol ~unitname:name (Some begname)) ::
    Csymbol_address (Compilenv.make_symbol ~unitname:name (Some endname)) ::
    lst
  in
  Cdata(Cglobal_symbol symbol ::
        Cdefine_symbol symbol ::
        List.fold_right addsyms namelist [cint_zero])

let data_segment_table namelist =
  segment_table namelist "caml_data_segments" "data_begin" "data_end"

let code_segment_table namelist =
  segment_table namelist "caml_code_segments" "code_begin" "code_end"

(* Initialize a predefined exception *)

let predef_exception i name =
  let symname = "caml_exn_" ^ name in
  let cst = Uconst_string name in
  let label = Compilenv.new_const_symbol () in
  let cont = emit_structured_constant label cst [] in
  Cdata(Cglobal_symbol symname ::
        emit_structured_constant symname
          (Uconst_block(Obj.object_tag,
                       [
                         Uconst_ref(label, cst);
                         Uconst_int (-i-1);
                       ])) cont)

(* Header for a plugin *)

let mapflat f l = List.flatten (List.map f l)

let plugin_header units =
  let mk (ui,crc) =
    { dynu_name = ui.ui_name;
      dynu_crc = crc;
      dynu_imports_cmi = ui.ui_imports_cmi;
      dynu_imports_cmx = ui.ui_imports_cmx;
      dynu_defines = ui.ui_defines
    } in
  global_data "caml_plugin_header"
    { dynu_magic = Config.cmxs_magic_number; dynu_units = List.map mk units }
