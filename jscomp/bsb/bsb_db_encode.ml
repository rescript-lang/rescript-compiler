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
  Buffer.add_char buf '\n'

let comma buf = 
  Buffer.add_char buf ','

(* IDEAS: 
  Pros: 
    - could be even shortened to a single byte
  Cons: 
    - decode would allocate
    - code too verbose
    - not readable 
 *)  
let encode_ml_info (x : Bsb_db.ml_info ) : char =   
  match x with 
  | Ml_empty -> '0'
  | Ml_source(false,false) -> '1'
  | Ml_source(false,true) -> '2'
  | Ml_source(true, false) -> '3'
  | Ml_source(true, true) -> '4'

let encode_mli_info (x : Bsb_db.mli_info ) : char =   
  match x with 
  | Mli_empty -> '0'
  | Mli_source(false,false) -> '1'
  | Mli_source(false,true) -> '2'
  | Mli_source(true, false) -> '3'
  | Mli_source(true, true) -> '4'

let rec encode_module_info  (x : Bsb_db.module_info) (buf : Buffer.t) =   
  Buffer.add_string buf x.name_sans_extension;
  comma buf; 
  Buffer.add_char buf (encode_mli_info x.mli_info);  
  Buffer.add_char buf (encode_ml_info x.ml_info)
  

(* Make sure [tmp_buf1] and [tmp_buf2] is cleared ,
  they are only used to control the order.
  Strictly speaking, [tmp_buf1] is not needed
*)
let encode_single (x : Bsb_db.t) (buf : Buffer.t)  (buf2 : Buffer.t) =    
  let len = String_map.cardinal x in 
  nl buf ; 
  Buffer.add_string buf (string_of_int len);
  String_map.iter x (fun name module_info ->
      nl buf; 
      Buffer.add_string buf name; 
      nl buf2; 
      encode_module_info module_info buf2 
    ) 

let encode (x : Bsb_db.ts) (oc : out_channel)=     
  output_char oc '\n';
  let len = Array.length x in 
  output_string oc (string_of_int len); 
  let tmp_buf1 = Buffer.create 10_000 in 
  let tmp_buf2 = Buffer.create 60_000 in 
  Ext_array.iter x (fun x -> begin 
        encode_single x  tmp_buf1 tmp_buf2;
        Buffer.output_buffer oc tmp_buf1;
        Buffer.output_buffer oc tmp_buf2;
        Buffer.clear tmp_buf1; 
        Buffer.clear tmp_buf2
      end
    )


let write_build_cache ~dir (bs_files : Bsb_db.ts)  : unit = 
  let oc = open_out_bin (Filename.concat dir bsbuild_cache) in 
  output_string oc Bs_version.version ;
  encode bs_files oc; 
  close_out oc 
