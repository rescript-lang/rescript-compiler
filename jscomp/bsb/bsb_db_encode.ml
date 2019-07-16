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

let comma buf = 
  Ext_buffer.add_char buf ','

(* IDEAS: 
  Pros: 
    - could be even shortened to a single byte
  Cons: 
    - decode would allocate
    - code too verbose
    - not readable 
 *)  


let encode_module_info  (x : Bsb_db.module_info) (buf : Ext_buffer.t) =   
  Ext_buffer.add_char buf (if x.case then '1' else '0');
  Ext_buffer.add_string buf x.dir
  
  
  
  

(* Make sure [tmp_buf1] and [tmp_buf2] is cleared ,
  they are only used to control the order.
  Strictly speaking, [tmp_buf1] is not needed
*)
let encode_single (db : Bsb_db.t) (buf : Ext_buffer.t) =    
  let len = String_map.cardinal db in 
  nl buf ; 
  Ext_buffer.add_string buf (string_of_int len);
  String_map.iter db (fun name module_info ->
      nl buf; 
      Ext_buffer.add_string buf name; 

    ); 
  String_map.iter db (fun name module_info -> 
      nl buf; 
      encode_module_info module_info buf 
    )
let encode (dbs : Bsb_db.ts) (oc : out_channel)=     
  let buf = Ext_buffer.create 100_000 in 
  Ext_buffer.add_char buf '\n';
  let len = Array.length dbs in 
  Ext_buffer.add_string buf (string_of_int len); 
  Ext_array.iter dbs (fun x ->  encode_single x  buf);
  Ext_buffer.output_buffer oc buf


let write_build_cache ~dir (bs_files : Bsb_db.ts)  : unit = 
  let oc = open_out_bin (Filename.concat dir bsbuild_cache) in 
  output_string oc Bs_version.version ;
  encode bs_files oc; 
  close_out oc 
