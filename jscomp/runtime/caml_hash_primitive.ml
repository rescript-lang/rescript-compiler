(* Copyright (C) 2018 Hongbo Zhang, Authors of ReScript
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






let rotl32 (x : int) n  = 
  (x lsl n) lor (x lsr (32 - n))

external (.![]) : string -> int -> int = "charCodeAt" [@@bs.send]
let hash_mix_int h  d = 
  let d = ref d in 
  d.contents <- d.contents * 0xcc9e2d51 ;
  d.contents <- rotl32 d.contents 15 ;
  d.contents <- d.contents * 0x1b873593 ;
  let h = ref (h lxor d.contents) in
  h.contents <- rotl32 h.contents 13 ;
  h.contents + (h.contents lsl 2)  + 0xe6546b64  

let hash_final_mix h = 
  let h = ref (h lxor (h lsr 16)) in
  h.contents <- h.contents * 0x85ebca6b ;
  h.contents <- h.contents lxor (h.contents lsr 13);
  h.contents <- h.contents * 0xc2b2ae35 ;
  h.contents lxor (h.contents lsr 16)
(* Caml_nativeint_extern.logand  (h.contents ^ (h.contents >>> 16)) 0x3FFFFFFFn *)

let hash_mix_string h  s = 

  let len =Caml_string_extern.length s in
  let block = len / 4 - 1  in
  let hash = ref h in  
  for i = 0 to block  do 
    let j = 4 * i in 
    let w = 
      s.![j] lor 
      (s.![j+1] lsl 8) lor 
      (s.![j+2] lsl 16) lor 
      (s.![j+3] lsl 24)
    in
    hash.contents <- hash_mix_int hash.contents  w
  done ;
  let modulo =  len land 0b11 in 
  if modulo <> 0 then 
    begin 
      let w =
        if modulo = 3 then 
          (s.![len - 1] lsl 16) lor 
          (s.![len - 2] lsl 8) lor
          s.![len - 3]
        else if modulo = 2 then 
          (s.![len -1] lsl 8) lor 
          s.![len -2]
        else  s.![len - 1] 
      in 
      hash.contents <- hash_mix_int hash.contents  w
    end;
  hash.contents <- hash.contents lxor len ;
  hash.contents 


