(* Copyright (C) 2018 Authors of BuckleScript
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)



let (<< ) = Caml_nativeint_extern.shift_left
let (>>>) = Caml_nativeint_extern.shift_right_logical
let (|~) = Caml_nativeint_extern.logor
let (^) = Caml_nativeint_extern.logxor

external ( *~ ) : nativeint -> nativeint -> nativeint = "caml_int32_mul" 
external ( +~ ) : nativeint -> nativeint -> nativeint = "caml_int32_add"



let rotl32 (x : nativeint) n  = 
  (x << n) |~ (x >>> (32 - n))


let caml_hash_mix_int h  d = 
  let d = ref d in 
  d := !d *~ 0xcc9e2d51n ;
  d := rotl32 !d 15 ;
  d := !d *~ 0x1b873593n ;
  let h = ref (h ^ !d) in
  h := rotl32 !h 13 ;
  !h +~ (!h << 2)  +~ 0xe6546b64n  

let caml_hash_final_mix h = 
  let h = ref (h ^ (h >>> 16)) in
  h := !h *~ 0x85ebca6bn ;
  h := !h ^ (!h >>> 13);
  h := !h *~ 0xc2b2ae35n ;
  !h ^ (!h >>> 16)
  (* Caml_nativeint_extern.logand  (!h ^ (!h >>> 16)) 0x3FFFFFFFn *)

let caml_hash_mix_string h  s = 
  let module String = Caml_string_extern in 
  let len =Caml_string_extern.length s in
  let block = len / 4 - 1  in
  let hash = ref h in  
  for i = 0 to block  do 
    let j = 4 * i in 
    let w = 
      Caml_char.code s.[j] lor 
      (Caml_char.code s.[j+1] lsl 8) lor 
      (Caml_char.code s.[j+2] lsl 16) lor 
      (Caml_char.code s.[j+3] lsl 24)
    in
    hash := caml_hash_mix_int !hash (Caml_nativeint_extern.of_int w)
  done ;
  let modulo =  len land 0b11 in 
  if modulo <> 0 then 
    begin 
      let w =
        if modulo = 3 then 
          (Caml_char.code s.[len - 1] lsl 16) lor 
          (Caml_char.code s.[len - 2] lsl 8) lor
          (Caml_char.code s.[len - 3])
        else if modulo = 2 then 
          (Caml_char.code s.[len -1] lsl 8) lor 
          Caml_char.code s.[len -2]
        else Caml_char.code s.[len - 1] 
      in 
      hash := caml_hash_mix_int !hash (Caml_nativeint_extern.of_int w)
    end;
  hash := !hash ^ (Caml_nativeint_extern.of_int len) ;
  !hash 

 
