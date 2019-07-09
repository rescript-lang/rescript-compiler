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






external reraise: exn -> 'a = "%reraise"

let finally v action f   = 
  match f v with
  | exception e -> 
      action v ;
      reraise e 
  | e ->  action v ; e 

let try_it f  =   
  try ignore (f ()) with _ -> ()

let with_file_as_chan filename f = 
  finally (open_out_bin filename) close_out f 

let with_file_as_pp filename f = 
  finally (open_out_bin filename) close_out
    (fun chan -> 
      let fmt = Format.formatter_of_out_channel chan in
      let v = f  fmt in
      Format.pp_print_flush fmt ();
      v
    ) 


let  is_pos_pow n = 
  let module M = struct exception E end in 
  let rec aux c (n : Int32.t) = 
    if n <= 0l then -2 
    else if n = 1l then c 
    else if Int32.logand n 1l =  0l then   
      aux (c + 1) (Int32.shift_right n 1 )
    else raise M.E in 
  try aux 0 n  with M.E -> -1

let failwithf ~loc fmt = Format.ksprintf (fun s -> failwith (loc ^ s))
    fmt
    
let invalid_argf fmt = Format.ksprintf invalid_arg fmt

let bad_argf fmt = Format.ksprintf (fun x -> raise (Arg.Bad x ) ) fmt

external id : 'a -> 'a = "%identity"


let hash_variant s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := 223 * !accu + Char.code s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land (1 lsl 31 - 1);
  (* make it signed for 64 bits architectures *)
  if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu

let todo loc = 
  failwith (loc ^ " Not supported yet")

let digest_length = 16  