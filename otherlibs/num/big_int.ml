(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*    Valerie Menissier-Morain, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

open Int_misc
open Nat

type big_int =
   { sign : int;
     abs_value : nat }

let create_big_int sign nat =
 if sign = 1 || sign = -1 ||
    (sign = 0 &&
     is_zero_nat nat 0 (num_digits_nat nat 0 (length_nat nat)))
 then { sign = sign;
         abs_value = nat }
 else invalid_arg "create_big_int"

(* Sign of a big_int *)
let sign_big_int bi = bi.sign

let zero_big_int =
 { sign = 0;
   abs_value = make_nat 1 }

let unit_big_int =
  { sign = 1;
    abs_value = nat_of_int 1 }

(* Number of digits in a big_int *)
let num_digits_big_int bi =
 num_digits_nat (bi.abs_value) 0 (length_nat bi.abs_value)

(* Opposite of a big_int *)
let minus_big_int bi =
 { sign = - bi.sign;
   abs_value = copy_nat (bi.abs_value) 0 (num_digits_big_int bi)}

(* Absolute value of a big_int *)
let abs_big_int bi =
    { sign = if bi.sign = 0 then 0 else 1;
      abs_value = copy_nat (bi.abs_value) 0 (num_digits_big_int bi)}

(* Comparison operators on big_int *)

(*
   compare_big_int (bi, bi2) = sign of (bi-bi2)
   i.e. 1 if bi > bi2
        0 if bi = bi2
        -1 if bi < bi2
*)
let compare_big_int bi1 bi2 =
  if bi1.sign = 0 && bi2.sign = 0 then 0
  else if bi1.sign < bi2.sign then -1
  else if bi1.sign > bi2.sign then 1
  else if bi1.sign = 1 then
            compare_nat (bi1.abs_value) 0 (num_digits_big_int bi1)
                        (bi2.abs_value) 0 (num_digits_big_int bi2)
  else
            compare_nat (bi2.abs_value) 0 (num_digits_big_int bi2)
                        (bi1.abs_value) 0 (num_digits_big_int bi1)

let eq_big_int bi1 bi2 = compare_big_int bi1 bi2 = 0
and le_big_int bi1 bi2 = compare_big_int bi1 bi2 <= 0
and ge_big_int bi1 bi2 = compare_big_int bi1 bi2 >= 0
and lt_big_int bi1 bi2 = compare_big_int bi1 bi2 < 0
and gt_big_int bi1 bi2 = compare_big_int bi1 bi2 > 0

let max_big_int bi1 bi2 = if lt_big_int bi1 bi2 then bi2 else bi1
and min_big_int bi1 bi2 = if gt_big_int bi1 bi2 then bi2 else bi1

(* Operations on big_int *)

let pred_big_int bi =
 match bi.sign with
    0 -> { sign = -1; abs_value = nat_of_int 1}
  | 1 -> let size_bi = num_digits_big_int bi in
          let copy_bi = copy_nat (bi.abs_value) 0 size_bi in
            ignore (decr_nat copy_bi 0 size_bi 0);
            { sign = if is_zero_nat copy_bi 0 size_bi then 0 else 1;
              abs_value = copy_bi }
  | _ -> let size_bi = num_digits_big_int bi in
         let size_res = succ (size_bi) in
         let copy_bi = create_nat (size_res) in
          blit_nat copy_bi 0 (bi.abs_value) 0 size_bi;
          set_digit_nat copy_bi size_bi 0;
          ignore (incr_nat copy_bi 0 size_res 1);
          { sign = -1;
            abs_value = copy_bi }

let succ_big_int bi =
 match bi.sign with
    0 -> {sign = 1; abs_value = nat_of_int 1}
  | -1 -> let size_bi = num_digits_big_int bi in
           let copy_bi = copy_nat (bi.abs_value) 0 size_bi in
            ignore (decr_nat copy_bi 0 size_bi 0);
            { sign = if is_zero_nat copy_bi 0 size_bi then 0 else -1;
              abs_value = copy_bi }
  | _ -> let size_bi = num_digits_big_int bi in
         let size_res = succ (size_bi) in
         let copy_bi = create_nat (size_res) in
          blit_nat copy_bi 0 (bi.abs_value) 0 size_bi;
          set_digit_nat copy_bi size_bi 0;
          ignore (incr_nat copy_bi 0 size_res 1);
          { sign = 1;
            abs_value = copy_bi }

let add_big_int bi1 bi2 =
 let size_bi1 = num_digits_big_int bi1
 and size_bi2 = num_digits_big_int bi2 in
  if bi1.sign = bi2.sign
   then    (* Add absolute values if signs are the same *)
    { sign = bi1.sign;
      abs_value =
       match compare_nat (bi1.abs_value) 0 size_bi1
                         (bi2.abs_value) 0 size_bi2 with
        -1 -> let res = create_nat (succ size_bi2) in
                (blit_nat res 0 (bi2.abs_value) 0 size_bi2;
                 set_digit_nat res size_bi2 0;
                 ignore
                   (add_nat res 0 (succ size_bi2)
                      (bi1.abs_value) 0 size_bi1 0);
                 res)
       |_  -> let res = create_nat (succ size_bi1) in
               (blit_nat res 0 (bi1.abs_value) 0 size_bi1;
                set_digit_nat res size_bi1 0;
                ignore (add_nat res 0 (succ size_bi1)
                         (bi2.abs_value) 0 size_bi2 0);
                res)}

  else      (* Subtract absolute values if signs are different *)
    match compare_nat (bi1.abs_value) 0 size_bi1
                      (bi2.abs_value) 0 size_bi2 with
       0 -> zero_big_int
     | 1 -> { sign = bi1.sign;
               abs_value =
                let res = copy_nat (bi1.abs_value) 0 size_bi1 in
                 (ignore (sub_nat res 0 size_bi1
                            (bi2.abs_value) 0 size_bi2 1);
                  res) }
     | _ -> { sign = bi2.sign;
              abs_value =
               let res = copy_nat (bi2.abs_value) 0 size_bi2 in
                 (ignore (sub_nat res 0 size_bi2
                            (bi1.abs_value) 0 size_bi1 1);
                  res) }

(* Coercion with int type *)
let big_int_of_int i =
  { sign = sign_int i;
    abs_value =
      let res = (create_nat 1)
      in (if i = monster_int
             then (set_digit_nat res 0 biggest_int;
                   ignore (incr_nat res 0 1 1))
             else set_digit_nat res 0 (abs i));
      res }

let add_int_big_int i bi = add_big_int (big_int_of_int i) bi

let sub_big_int bi1 bi2 = add_big_int bi1 (minus_big_int bi2)

(* Returns i * bi *)
let mult_int_big_int i bi =
 let size_bi = num_digits_big_int bi in
 let size_res = succ size_bi in
  if i = monster_int
     then let res = create_nat size_res in
            blit_nat res 0 (bi.abs_value) 0 size_bi;
            set_digit_nat res size_bi 0;
            ignore (mult_digit_nat res 0 size_res (bi.abs_value) 0 size_bi
                      (nat_of_int biggest_int) 0);
            { sign = - (sign_big_int bi);
              abs_value = res }
     else let res = make_nat (size_res) in
          ignore (mult_digit_nat res 0 size_res (bi.abs_value) 0 size_bi
                    (nat_of_int (abs i)) 0);
          { sign = (sign_int i) * (sign_big_int bi);
            abs_value = res }

let mult_big_int bi1 bi2 =
 let size_bi1 = num_digits_big_int bi1
 and size_bi2 = num_digits_big_int bi2 in
 let size_res = size_bi1 + size_bi2 in
 let res = make_nat (size_res) in
  { sign = bi1.sign * bi2.sign;
    abs_value =
         if size_bi2 > size_bi1
           then (ignore (mult_nat res 0 size_res (bi2.abs_value) 0 size_bi2
                           (bi1.abs_value) 0 size_bi1);res)
           else (ignore (mult_nat res 0 size_res (bi1.abs_value) 0 size_bi1
                           (bi2.abs_value) 0 size_bi2);res) }

(* (quotient, rest) of the euclidian division of 2 big_int *)
let quomod_big_int bi1 bi2 =
 if bi2.sign = 0 then raise Division_by_zero
 else
  let size_bi1 = num_digits_big_int bi1
  and size_bi2 = num_digits_big_int bi2 in
   match compare_nat (bi1.abs_value) 0 size_bi1
                     (bi2.abs_value) 0 size_bi2 with
      -1 -> (* 1/2  -> 0, reste 1, -1/2  -> -1, reste 1 *)
            (* 1/-2 -> 0, reste 1, -1/-2 -> 1, reste 1 *)
             if bi1.sign >= 0 then
               (big_int_of_int 0, bi1)
             else if bi2.sign >= 0 then
               (big_int_of_int(-1), add_big_int bi2 bi1)
             else
               (big_int_of_int 1, sub_big_int bi1 bi2)
    | 0 -> (big_int_of_int (bi1.sign * bi2.sign), zero_big_int)
    | _ -> let bi1_negatif = bi1.sign = -1 in
           let size_q =
            if bi1_negatif
             then succ (max (succ (size_bi1 - size_bi2)) 1)
             else max (succ (size_bi1 - size_bi2)) 1
           and size_r = succ (max size_bi1 size_bi2)
            (* r is long enough to contain both quotient and remainder *)
            (* of the euclidian division *)
           in
            (* set up quotient, remainder *)
            let q = create_nat size_q
            and r = create_nat size_r in
            blit_nat r 0 (bi1.abs_value) 0 size_bi1;
            set_to_zero_nat r size_bi1 (size_r - size_bi1);

            (* do the division of |bi1| by |bi2|
               - at the beginning, r contains |bi1|
               - at the end, r contains
                 * in the size_bi2 least significant digits, the remainder
                 * in the size_r-size_bi2 most significant digits, the quotient
              note the conditions for application of div_nat are verified here
             *)
            div_nat r 0 size_r (bi2.abs_value) 0 size_bi2;

            (* separate quotient and remainder *)
            blit_nat q 0 r size_bi2 (size_r - size_bi2);
            let not_null_mod = not (is_zero_nat r 0 size_bi2) in

            (* correct the signs, adjusting the quotient and remainder *)
            if bi1_negatif && not_null_mod
             then
              (* bi1<0, r>0, noting r for (r, size_bi2) the remainder,      *)
              (* we have |bi1|=q * |bi2| + r with 0 < r < |bi2|,            *)
              (* thus -bi1 = q * |bi2| + r                                  *)
              (* and bi1 = (-q) * |bi2| + (-r) with -|bi2| < (-r) < 0       *)
              (* thus bi1 = -(q+1) * |bi2| + (|bi2|-r)                      *)
              (* with 0 < (|bi2|-r) < |bi2|                                 *)
              (* so the quotient has for sign the opposite of the bi2'one   *)
              (*                 and for value q+1                          *)
              (* and the remainder is strictly positive                     *)
              (*                  has for value |bi2|-r                     *)
              (let new_r = copy_nat (bi2.abs_value) 0 size_bi2 in
                      (* new_r contains (r, size_bi2) the remainder *)
                { sign = - bi2.sign;
                  abs_value = (set_digit_nat q (pred size_q) 0;
                               ignore (incr_nat q 0 size_q 1); q) },
                { sign = 1;
                 abs_value =
                      (ignore (sub_nat new_r 0 size_bi2 r 0 size_bi2 1);
                      new_r) })
             else
              (if bi1_negatif then set_digit_nat q (pred size_q) 0;
                { sign = if is_zero_nat q 0 size_q
                          then 0
                          else bi1.sign * bi2.sign;
                  abs_value = q },
                { sign = if not_null_mod then 1 else 0;
                  abs_value = copy_nat r 0 size_bi2 })

let div_big_int bi1 bi2 = fst (quomod_big_int bi1 bi2)
and mod_big_int bi1 bi2 = snd (quomod_big_int bi1 bi2)

let gcd_big_int bi1 bi2 =
 let size_bi1 = num_digits_big_int bi1
 and size_bi2 = num_digits_big_int bi2 in
  if is_zero_nat (bi1.abs_value) 0 size_bi1 then abs_big_int bi2
  else if is_zero_nat (bi2.abs_value) 0 size_bi2 then
        { sign = 1;
          abs_value = bi1.abs_value }
  else
        { sign = 1;
          abs_value =
           match compare_nat (bi1.abs_value) 0 size_bi1
                             (bi2.abs_value) 0 size_bi2 with
           0 -> bi1.abs_value
         | 1 ->
            let res = copy_nat (bi1.abs_value) 0 size_bi1 in
            let len =
              gcd_nat res 0 size_bi1 (bi2.abs_value) 0 size_bi2 in
            copy_nat res 0 len
         | _ ->
            let res = copy_nat (bi2.abs_value) 0 size_bi2 in
            let len =
              gcd_nat res 0 size_bi2 (bi1.abs_value) 0 size_bi1 in
            copy_nat res 0 len
         }

(* Coercion operators *)

let monster_big_int = big_int_of_int monster_int;;

let monster_nat = monster_big_int.abs_value;;

let is_int_big_int bi =
  num_digits_big_int bi == 1 &&
  match compare_nat bi.abs_value 0 1 monster_nat 0 1 with
  | 0 -> bi.sign == -1
  | -1 -> true
  | _ -> false;;

let int_of_big_int bi =
  try let n = int_of_nat bi.abs_value in
      if bi.sign = -1 then - n else n
  with Failure _ ->
    if eq_big_int bi monster_big_int then monster_int
    else failwith "int_of_big_int";;

let big_int_of_nativeint i =
  if i = 0n then
    zero_big_int
  else if i > 0n then begin
    let res = create_nat 1 in
    set_digit_nat_native res 0 i;
    { sign = 1; abs_value = res }
  end else begin
    let res = create_nat 1 in
    set_digit_nat_native res 0 (Nativeint.neg i);
    { sign = -1; abs_value = res }
  end

let nativeint_of_big_int bi =
  if num_digits_big_int bi > 1 then failwith "nativeint_of_big_int";
  let i = nth_digit_nat_native bi.abs_value 0 in
  if bi.sign >= 0 then
    if i >= 0n then i else failwith "nativeint_of_big_int"
  else
    if i >= 0n || i = Nativeint.min_int
    then Nativeint.neg i
    else failwith "nativeint_of_big_int"

let big_int_of_int32 i = big_int_of_nativeint (Nativeint.of_int32 i)

let int32_of_big_int bi =
  let i = nativeint_of_big_int bi in
  if i <= 0x7FFF_FFFFn && i >= -0x8000_0000n
  then Nativeint.to_int32 i
  else failwith "int32_of_big_int"

let big_int_of_int64 i =
  if Sys.word_size = 64 then
    big_int_of_nativeint (Int64.to_nativeint i)
  else begin
    let (sg, absi) =
      if i = 0L then (0, 0L)
      else if i > 0L then (1, i)
      else (-1, Int64.neg i) in
    let res = create_nat 2 in
    set_digit_nat_native res 0 (Int64.to_nativeint absi);
    set_digit_nat_native res 1 (Int64.to_nativeint (Int64.shift_right absi 32));
    { sign = sg; abs_value = res }
  end

let int64_of_big_int bi =
  if Sys.word_size = 64 then
    Int64.of_nativeint (nativeint_of_big_int bi)
  else begin
    let i =
      match num_digits_big_int bi with
      | 1 -> Int64.logand
               (Int64.of_nativeint (nth_digit_nat_native bi.abs_value 0))
               0xFFFFFFFFL
      | 2 -> Int64.logor
               (Int64.logand
                 (Int64.of_nativeint (nth_digit_nat_native bi.abs_value 0))
                 0xFFFFFFFFL)
               (Int64.shift_left
                 (Int64.of_nativeint (nth_digit_nat_native bi.abs_value 1))
                 32)
      | _ -> failwith "int64_of_big_int" in
    if bi.sign >= 0 then
      if i >= 0L then i else failwith "int64_of_big_int"
    else
      if i >= 0L || i = Int64.min_int
      then Int64.neg i
      else failwith "int64_of_big_int"
  end

(* Coercion with nat type *)
let nat_of_big_int bi =
 if bi.sign = -1
 then failwith "nat_of_big_int"
 else copy_nat (bi.abs_value) 0 (num_digits_big_int bi)

let sys_big_int_of_nat nat off len =
 let length = num_digits_nat nat off len in
    { sign = if is_zero_nat nat off  length then 0 else 1;
      abs_value = copy_nat nat off length }

let big_int_of_nat nat =
 sys_big_int_of_nat nat 0 (length_nat nat)

(* Coercion with string type *)

let string_of_big_int bi =
  if bi.sign = -1
  then "-" ^ string_of_nat bi.abs_value
  else string_of_nat bi.abs_value


let sys_big_int_of_string_aux s ofs len sgn base =
  if len < 1 then failwith "sys_big_int_of_string";
  let n = sys_nat_of_string base s ofs len in
  if is_zero_nat n 0 (length_nat n) then zero_big_int
  else {sign = sgn; abs_value = n}
;;

let sys_big_int_of_string_base s ofs len sgn =
  if len < 1 then failwith "sys_big_int_of_string";
  if len < 2 then sys_big_int_of_string_aux s ofs len sgn 10
  else
    match (s.[ofs], s.[ofs+1]) with
    | ('0', 'x') | ('0', 'X') -> sys_big_int_of_string_aux s (ofs+2) (len-2) sgn 16
    | ('0', 'o') | ('0', 'O') -> sys_big_int_of_string_aux s (ofs+2) (len-2) sgn 8
    | ('0', 'b') | ('0', 'B') -> sys_big_int_of_string_aux s (ofs+2) (len-2) sgn 2
    | _ -> sys_big_int_of_string_aux s ofs len sgn 10
;;

let sys_big_int_of_string s ofs len =
  if len < 1 then failwith "sys_big_int_of_string";
  match s.[ofs] with
  | '-' -> sys_big_int_of_string_base s (ofs+1) (len-1) (-1)
  | '+' -> sys_big_int_of_string_base s (ofs+1) (len-1) 1
  | _ -> sys_big_int_of_string_base s ofs len 1
;;

let big_int_of_string s =
  sys_big_int_of_string s 0 (String.length s)

let power_base_nat base nat off len =
  if base = 0 then nat_of_int 0 else
  if is_zero_nat nat off len || base = 1 then nat_of_int 1 else
  let power_base = make_nat (succ length_of_digit) in
  let (pmax, pint) = make_power_base base power_base in
  let (n, rem) =
      let (x, y) = quomod_big_int (sys_big_int_of_nat nat off len)
                                  (big_int_of_int (succ pmax)) in
        (int_of_big_int x, int_of_big_int y) in
  if n = 0 then copy_nat power_base (pred rem) 1 else
   begin
    let res = make_nat n
    and res2 = make_nat (succ n)
    and l = num_bits_int n - 2 in
      blit_nat res 0 power_base pmax 1;
      for i = l downto 0 do
        let len = num_digits_nat res 0 n in
        let len2 = min n (2 * len) in
        let succ_len2 = succ len2 in
          ignore (square_nat res2 0 len2 res 0 len);
          begin
           if n land (1 lsl i) > 0
              then (set_to_zero_nat res 0 len;
                    ignore (mult_digit_nat res 0 succ_len2
                              res2 0 len2 power_base pmax))
              else blit_nat res 0 res2 0 len2
          end;
          set_to_zero_nat res2 0 len2
      done;
    if rem > 0
     then (ignore (mult_digit_nat res2 0 (succ n)
                     res 0 n power_base (pred rem));
           res2)
     else res
  end

let power_int_positive_int i n =
  match sign_int n with
    0 -> unit_big_int
  | -1 -> invalid_arg "power_int_positive_int"
  | _ -> let nat = power_base_int (abs i) n in
           { sign = if i >= 0
                       then sign_int i
                       else if n land 1 = 0
                               then 1
                               else -1;
             abs_value = nat}

let power_big_int_positive_int bi n =
  match sign_int n with
    0 -> unit_big_int
  | -1 -> invalid_arg "power_big_int_positive_int"
  | _ -> let bi_len = num_digits_big_int bi in
         let res_len = bi_len * n in
         let res = make_nat res_len
         and res2 = make_nat res_len
         and l = num_bits_int n - 2 in
         blit_nat res 0 bi.abs_value 0 bi_len;
         for i = l downto 0 do
           let len = num_digits_nat res 0 res_len in
           let len2 = min res_len (2 * len) in
           set_to_zero_nat res2 0 len2;
           ignore (square_nat res2 0 len2 res 0 len);
           if n land (1 lsl i) > 0 then begin
             let lenp = min res_len (len2 + bi_len) in
             set_to_zero_nat res 0 lenp;
             ignore(mult_nat res 0 lenp res2 0 len2 (bi.abs_value) 0 bi_len)
           end else begin
             blit_nat res 0 res2 0 len2
           end
         done;
         {sign = if bi.sign >=  0 then bi.sign
                 else if n land 1 = 0 then 1 else -1;
            abs_value = res}

let power_int_positive_big_int i bi =
  match sign_big_int bi with
    0 -> unit_big_int
  | -1 -> invalid_arg "power_int_positive_big_int"
  | _ -> let nat = power_base_nat
                     (abs i) (bi.abs_value) 0 (num_digits_big_int bi) in
           { sign = if i >= 0
                       then sign_int i
                       else if is_digit_odd (bi.abs_value) 0
                               then -1
                               else 1;
             abs_value = nat }

let power_big_int_positive_big_int bi1 bi2 =
  match sign_big_int bi2 with
    0 -> unit_big_int
  | -1 -> invalid_arg "power_big_int_positive_big_int"
  | _ -> try
           power_big_int_positive_int bi1 (int_of_big_int bi2)
         with Failure _ ->
         try
           power_int_positive_big_int (int_of_big_int bi1) bi2
         with Failure _ ->
           raise Out_of_memory
           (* If neither bi1 nor bi2 is a small integer, bi1^bi2 is not
              representable.  Indeed, on a 32-bit platform,
              |bi1| >= 2 and |bi2| >= 2^30, hence bi1^bi2 has at least
              2^30 bits = 2^27 bytes, greater than the max size of
              allocated blocks.  On a 64-bit platform,
              |bi1| >= 2 and |bi2| >= 2^62, hence bi1^bi2 has at least
              2^62 bits = 2^59 bytes, greater than the max size of
              allocated blocks. *)

(* base_power_big_int compute bi*base^n *)
let base_power_big_int base n bi =
  match sign_int n with
    0 -> bi
  | -1 -> let nat = power_base_int base (-n) in
           let len_nat = num_digits_nat nat 0 (length_nat nat)
           and len_bi = num_digits_big_int bi in
             if len_bi < len_nat then
               invalid_arg "base_power_big_int"
             else if len_bi = len_nat &&
                     compare_digits_nat (bi.abs_value) len_bi nat len_nat = -1
               then invalid_arg "base_power_big_int"
             else
               let copy = create_nat (succ len_bi) in
                      blit_nat copy 0 (bi.abs_value) 0 len_bi;
                      set_digit_nat copy len_bi 0;
                      div_nat copy 0 (succ len_bi)
                              nat 0 len_nat;
                      if not (is_zero_nat copy 0 len_nat)
                         then invalid_arg "base_power_big_int"
                         else { sign = bi.sign;
                                abs_value = copy_nat copy len_nat 1 }
  | _ -> let nat = power_base_int base n in
         let len_nat = num_digits_nat nat 0 (length_nat nat)
         and len_bi = num_digits_big_int bi in
         let new_len = len_bi + len_nat in
         let res = make_nat new_len in
         ignore
           (if len_bi > len_nat
               then mult_nat res 0 new_len
                              (bi.abs_value) 0 len_bi
                              nat 0 len_nat
               else mult_nat res 0 new_len
                              nat 0 len_nat
                              (bi.abs_value) 0 len_bi)
          ; if is_zero_nat res 0 new_len
               then zero_big_int
               else create_big_int (bi.sign) res

(* Coercion with float type *)

let float_of_big_int bi =
  float_of_string (string_of_big_int bi)

(* XL: suppression de big_int_of_float et nat_of_float. *)

(* Other functions needed *)

(* Integer part of the square root of a big_int *)
let sqrt_big_int bi =
 match bi.sign with
 | 0 -> zero_big_int
 | -1 -> invalid_arg "sqrt_big_int"
 | _ -> {sign = 1;
         abs_value = sqrt_nat (bi.abs_value) 0 (num_digits_big_int bi)}

let square_big_int bi =
  if bi.sign == 0 then zero_big_int else
  let len_bi = num_digits_big_int bi in
  let len_res = 2 * len_bi in
  let res = make_nat len_res in
  ignore (square_nat res 0 len_res (bi.abs_value) 0 len_bi);
  {sign = 1; abs_value = res}

(* round off of the futur last digit (of the integer represented by the string
   argument of the function) that is now the previous one.
   if s contains an integer of the form (10^n)-1
    then s <- only 0 digits and the result_int is true
   else s <- the round number and the result_int is false *)
let round_futur_last_digit s off_set length =
 let l = pred (length + off_set) in
  if Char.code(Bytes.get s l) >= Char.code '5'
    then
     let rec round_rec l =
       if l < off_set then true else begin
         let current_char = Bytes.get s l in
         if current_char = '9' then
           (Bytes.set s l '0'; round_rec (pred l))
         else
           (Bytes.set s l (Char.chr (succ (Char.code current_char)));
            false)
       end
     in round_rec (pred l)
   else false


(* Approximation with floating decimal point a` la approx_ratio_exp *)
let approx_big_int prec bi =
  let len_bi = num_digits_big_int bi in
  let n =
    max 0
        (int_of_big_int (
          add_int_big_int
            (-prec)
            (div_big_int (mult_big_int (big_int_of_int (pred len_bi))
                                      (big_int_of_string "963295986"))
                        (big_int_of_string "100000000")))) in
  let s =
    Bytes.unsafe_of_string
      (string_of_big_int (div_big_int bi (power_int_positive_int 10 n)))
  in
  let (sign, off, len) =
    if Bytes.get s 0 = '-'
       then ("-", 1, succ prec)
       else ("", 0, prec) in
  if (round_futur_last_digit s off (succ prec))
       then (sign^"1."^(String.make prec '0')^"e"^
             (string_of_int (n + 1 - off + Bytes.length s)))
       else (sign^(Bytes.sub_string s off 1)^"."^
             (Bytes.sub_string s (succ off) (pred prec))
             ^"e"^(string_of_int (n - succ off + Bytes.length s)))

(* Logical operations *)

(* Shift left by N bits *)

let shift_left_big_int bi n =
  if n < 0 then invalid_arg "shift_left_big_int"
  else if n = 0 then bi
  else if bi.sign = 0 then bi
  else begin
    let size_bi = num_digits_big_int bi in
    let size_res = size_bi + ((n + length_of_digit - 1) / length_of_digit) in
    let res = create_nat size_res in
    let ndigits = n / length_of_digit in
    set_to_zero_nat res 0 ndigits;
    blit_nat res ndigits bi.abs_value 0 size_bi;
    let nbits = n mod length_of_digit in
    if nbits > 0 then
      shift_left_nat res ndigits size_bi res (ndigits + size_bi) nbits;
    { sign = bi.sign; abs_value = res }
  end

(* Shift right by N bits (rounds toward zero) *)

let shift_right_towards_zero_big_int bi n =
  if n < 0 then invalid_arg "shift_right_towards_zero_big_int"
  else if n = 0 then bi
  else if bi.sign = 0 then bi
  else begin
    let size_bi = num_digits_big_int bi in
    let ndigits = n / length_of_digit in
    let nbits = n mod length_of_digit in
    if ndigits >= size_bi then zero_big_int else begin
      let size_res = size_bi - ndigits in
      let res = create_nat size_res in
      blit_nat res 0 bi.abs_value ndigits size_res;
      if nbits > 0 then begin
        let tmp = create_nat 1 in
        shift_right_nat res 0 size_res tmp 0 nbits
      end;
      if is_zero_nat res 0 size_res
      then zero_big_int
      else { sign = bi.sign; abs_value = res }
    end
  end

(* Compute 2^n - 1 *)

let two_power_m1_big_int n =
  if n < 0 then invalid_arg "two_power_m1_big_int"
  else if n = 0 then zero_big_int
  else begin
    let size_res = (n + length_of_digit - 1) / length_of_digit in
    let res = make_nat size_res in
    set_digit_nat_native res (n / length_of_digit)
                             (Nativeint.shift_left 1n (n mod length_of_digit));
    ignore (decr_nat res 0 size_res 0);
    { sign = 1; abs_value = res }
  end

(* Shift right by N bits (rounds toward minus infinity) *)

let shift_right_big_int bi n =
  if n < 0 then invalid_arg "shift_right_big_int"
  else if bi.sign >= 0 then shift_right_towards_zero_big_int bi n
  else shift_right_towards_zero_big_int (sub_big_int bi (two_power_m1_big_int n)) n

(* Extract N bits starting at ofs.
   Treats bi in two's complement.
   Result is always positive. *)

let extract_big_int bi ofs n =
  if ofs < 0 || n < 0 then invalid_arg "extract_big_int"
  else if bi.sign = 0 then bi
  else begin
    let size_bi = num_digits_big_int bi in
    let size_res = (n + length_of_digit - 1) / length_of_digit in
    let ndigits = ofs / length_of_digit in
    let nbits = ofs mod length_of_digit in
    let res = make_nat size_res in
    if ndigits < size_bi then
      blit_nat res 0 bi.abs_value ndigits (min size_res (size_bi - ndigits));
    if bi.sign < 0 then begin
      (* Two's complement *)
      complement_nat res 0 size_res;
      (* PR#6010: need to increment res iff digits 0...ndigits-1 of bi are 0.
         In this case, digits 0...ndigits-1 of not(bi) are all 0xFF...FF,
         and adding 1 to them produces a carry out at ndigits. *)
      let rec carry_incr i =
        i >= ndigits || i >= size_bi ||
          (is_digit_zero bi.abs_value i && carry_incr (i + 1)) in
      if carry_incr 0 then ignore (incr_nat res 0 size_res 1)
    end;
    if nbits > 0 then begin
      let tmp = create_nat 1 in
      shift_right_nat res 0 size_res tmp 0 nbits
    end;
    let n' = n mod length_of_digit in
    if n' > 0 then begin
      let tmp = create_nat 1 in
      set_digit_nat_native tmp 0
          (Nativeint.shift_right_logical (-1n) (length_of_digit - n'));
      land_digit_nat res (size_res - 1) tmp 0
    end;
    if is_zero_nat res 0 size_res
    then zero_big_int
    else { sign = 1; abs_value = res }
  end

(* Bitwise logical operations.  Arguments must be >= 0. *)

let and_big_int a b =
  if a.sign < 0 || b.sign < 0 then invalid_arg "and_big_int"
  else if a.sign = 0 || b.sign = 0 then zero_big_int
  else begin
    let size_a = num_digits_big_int a
    and size_b = num_digits_big_int b in
    let size_res = min size_a size_b in
    let res = create_nat size_res in
    blit_nat res 0 a.abs_value 0 size_res;
    for i = 0 to size_res - 1 do
      land_digit_nat res i b.abs_value i
    done;
    if is_zero_nat res 0 size_res
    then zero_big_int
    else { sign = 1; abs_value = res }
  end

let or_big_int a b =
  if a.sign < 0 || b.sign < 0 then invalid_arg "or_big_int"
  else if a.sign = 0 then b
  else if b.sign = 0 then a
  else begin
    let size_a = num_digits_big_int a
    and size_b = num_digits_big_int b in
    let size_res = max size_a size_b in
    let res = create_nat size_res in
    let or_aux a' b' size_b' =
      blit_nat res 0 a'.abs_value 0 size_res;
      for i = 0 to size_b' - 1 do
        lor_digit_nat res i b'.abs_value i
      done in
    if size_a >= size_b
    then or_aux a b size_b
    else or_aux b a size_a;
    if is_zero_nat res 0 size_res
    then zero_big_int
    else { sign = 1; abs_value = res }
  end

let xor_big_int a b =
  if a.sign < 0 || b.sign < 0 then invalid_arg "xor_big_int"
  else if a.sign = 0 then b
  else if b.sign = 0 then a
  else begin
    let size_a = num_digits_big_int a
    and size_b = num_digits_big_int b in
    let size_res = max size_a size_b in
    let res = create_nat size_res in
    let xor_aux a' b' size_b' =
      blit_nat res 0 a'.abs_value 0 size_res;
      for i = 0 to size_b' - 1 do
        lxor_digit_nat res i b'.abs_value i
      done in
    if size_a >= size_b
    then xor_aux a b size_b
    else xor_aux b a size_a;
    if is_zero_nat res 0 size_res
    then zero_big_int
    else { sign = 1; abs_value = res }
  end
