(* Copyright (C) 2019 - Present Authors of BuckleScript
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

 
let bsbuild_cache = Literals.bsbuild_cache


let nl buf = 
  Ext_buffer.add_char buf '\n'



(* IDEAS: 
  Pros: 
    - could be even shortened to a single byte
  Cons: 
    - decode would allocate
    - code too verbose
    - not readable 
 *)  

let make_encoding length buf =
  let max_range = length lsl 1 + 1 in 
  if max_range <= 0xff then begin 
    Ext_buffer.add_char buf '1';
    Ext_buffer.add_int_1
  end
  else if max_range <= 0xff_ff then begin 
    Ext_buffer.add_char buf '2';
    Ext_buffer.add_int_2
  end
  else if length <= 0x7f_ff_ff then begin 
    Ext_buffer.add_char buf '3';
    Ext_buffer.add_int_3
  end
  else if length <= 0x7f_ff_ff_ff then begin
    Ext_buffer.add_char buf '4';
    Ext_buffer.add_int_4
  end else assert false 
(* Make sure [tmp_buf1] and [tmp_buf2] is cleared ,
  they are only used to control the order.
  Strictly speaking, [tmp_buf1] is not needed
*)
let encode_single (db : Bsb_db.t) (buf : Ext_buffer.t) =    
  nl buf ; (* module name section *)
  let len = String_map.cardinal db in 
  Ext_buffer.add_string_char buf (string_of_int len) '\n';
  let mapping = String_hashtbl.create 50 in 
  String_map.iter db (fun name {dir} ->  
      Ext_buffer.add_string_char buf name '\n'; 
      if not (String_hashtbl.mem mapping dir) then
        String_hashtbl.add mapping dir (String_hashtbl.length mapping)
    ); 
  let length = String_hashtbl.length mapping in   
  let rev_mapping = Array.make length "" in 
  String_hashtbl.iter mapping (fun k i -> Array.unsafe_set rev_mapping i k);
  (* directory name section *)
  Ext_array.iter rev_mapping (fun s -> Ext_buffer.add_string_char buf s '\t');
  nl buf; (* module name info section *)
  let len_encoding = make_encoding length buf in 
  String_map.iter db (fun _ module_info ->       
      len_encoding buf 
        (String_hashtbl.find_exn  mapping module_info.dir lsl 1 + Obj.magic module_info.case ))      
    
let encode (dbs : Bsb_db.ts) buf =     
  
  Ext_buffer.add_char_string buf '\n' (string_of_int (Array.length dbs)); 
  Ext_array.iter dbs (fun x ->  encode_single x  buf)
  


let write_build_cache ~dir (bs_files : Bsb_db.ts)  : string = 
  let oc = open_out_bin (Filename.concat dir bsbuild_cache) in 
  let buf = Ext_buffer.create 100_000 in 
  encode bs_files buf ; 
  let digest = Digest.to_hex (Ext_buffer.digest buf) in
  output_string oc digest;
  Ext_buffer.output_buffer oc buf;
  close_out oc; 
  digest
