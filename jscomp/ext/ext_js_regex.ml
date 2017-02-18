(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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

 type byte = 
  | Single of int
  | Cont of int
  | Leading of int * int
  | Invalid
 
(** [classify chr] returns the {!byte} corresponding to [chr] *)
let classify chr =
    let c = int_of_char chr in
    (* Classify byte according to leftmost 0 bit *)
    if c land 0b1000_0000 = 0 then Single c else 
      (* c 0b0____*)
    if c land 0b0100_0000 = 0 then Cont (c land 0b0011_1111) else
      (* c 0b10___*)
    if c land 0b0010_0000 = 0 then Leading (1, c land 0b0001_1111) else
      (* c 0b110__*)
    if c land 0b0001_0000 = 0 then Leading (2, c land 0b0000_1111) else
      (* c 0b1110_ *)
    if c land 0b0000_1000 = 0 then Leading (3, c land 0b0000_0111) else
      (* c 0b1111_0___*)
    if c land 0b0000_0100 = 0 then Leading (4, c land 0b0000_0011) else
      (* c 0b1111_10__*)
    if c land 0b0000_0010 = 0 then Leading (5, c land 0b0000_0001)
       (* c 0b1111_110__ *)
    else Invalid
 
 
(** [utf8_decode strm] returns a code point stream that will lazily decode
    the byte stream [strm] *)
let rec utf8_decode strm =
    Stream.slazy (fun () ->
        match Stream.peek strm with
        | Some chr ->
            Stream.junk strm;
            (match classify chr with
            | Single c -> Stream.icons c (utf8_decode strm)
            | Cont _ -> raise (Stream.Error "Unexpected continuation byte")
            | Leading (n, c) ->
              (** [follow strm n c] returns the code point based on [c] plus [n] continuation
                  bytes taken from [strm] *)
              let rec follow strm n c =
                if n = 0 then c
                else
                  (match classify (Stream.next strm) with
                   | Cont cc -> follow strm (n-1) ((c lsl 6) lor (cc land 0x3f))
                   | _ -> raise (Stream.Error "Continuation byte expected"))
              in
              Stream.icons (follow strm n c)  (utf8_decode strm)
            | Invalid -> raise (Stream.Error "Invalid byte"))
        | None -> Stream.sempty)

let  decode bytes offset  =
  let rec  init offset = 
    match classify (Bytes.get bytes offset) with 
    | Single c ->  c, offset + 1 
    | Invalid 
    | Cont _ -> invalid_arg "decode" 
    | Leading(n,c) -> leading n c (offset  + 1)
  and leading n c offset  = 
    if n = 0 then c, offset 
    else
      begin match classify (Bytes.get bytes offset) with 
        | Cont cc -> leading (n - 1) ((c lsl 6) lor (cc land 0x3f)) (offset + 1 )
        | _ -> invalid_arg "decode"
      end 
  in init offset

let utf8_list_of_string_reversed s = let  v = ref [] in 
    let add u = v := u :: !v in 
    begin 
        utf8_decode (Stream.of_string s)
        |> Stream.iter add;
    end;
    let codes = !v in codes

let check_from_end s =
    if String.length s = 0 then false
    else 
    let ul = utf8_list_of_string_reversed s in
    let rec aux l  = 
        match l with
        | [] -> false
        | (e::r) ->
            if e < 0 || e > 255 then false
             else (let c = Char.chr e in
             if c = '/' then true
               else (if c = 'i' || c = 'g' || c = 'm' || c = 'y' then aux r
               else false))
    in aux ul

let js_regex_checker s =
  if String.length s = 0 then false else
  let check_first = String.get s 0 = '/' in
  let check_last = check_from_end s in 
  check_first && check_last