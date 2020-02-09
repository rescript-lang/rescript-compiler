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


 type group = {
   modules : string array ; 
   dir_length : int;
   dir_info_offset : int ; 
   module_info_offset : int;
 }

type t = group array * string (* string is whole content*)


type cursor = int ref 


(*TODO: special case when module_count is zero *)
let rec decode_internal (x : string) (offset : cursor) =   
  let len = Ext_pervasives.parse_nat_of_string x offset in  
  incr offset;
  let first = decode_single x offset in 
  if len = 1 then [|first|]
  else 
    let result = Array.make len first in 
    for i = 1 to len - 1 do 
      Array.unsafe_set result i (decode_single x offset)
    done ;
    result
  
and decode_single (x : string) (offset : cursor) : group = 
  let module_number = Ext_pervasives.parse_nat_of_string x offset in 
  incr offset;
  let modules = decode_modules x offset module_number in 
  let dir_info_offset = !offset in 
  let module_info_offset = 
    String.index_from x dir_info_offset '\n'  + 1 in
  let dir_length = Char.code x.[module_info_offset] - 48 (* Char.code '0'*) in
  offset := 
    module_info_offset +
    1 +
    dir_length * module_number +
    1 
    ;
  { modules ; dir_info_offset; module_info_offset ; dir_length}
and decode_modules (x : string) (offset : cursor) module_number : string array =   
  let result = Array.make module_number "" in 
  let last = ref !offset in 
  let cur = ref !offset in 
  let tasks = ref 0 in 
  while !tasks <> module_number do 
    if String.unsafe_get x !cur = '\n' then 
      begin 
        let offs = !last in 
        let len = (!cur - !last) in         
        Array.unsafe_set result !tasks
        (Ext_string.unsafe_sub x offs len);
        incr tasks;
        last := !cur + 1;
      end;
    incr cur
  done ;
  offset := !cur;
  result
  

(* TODO: shall we check the consistency of digest *)
let read_build_cache ~dir  : t =   
  let all_content = 
    Ext_io.load_file (Filename.concat dir bsbuild_cache) in   
  decode_internal all_content (ref (Ext_digest.length + 1)), all_content



type module_info =  {
  case : bool ; (* which is Bsb_db.case*)
  dir_name : string
} 


let find_opt 
  ((sorteds,whole) : t )  i (key : string) 
    : module_info option = 
  let group = sorteds.(i) in 
  let i = Ext_string_array.find_sorted  group.modules key in 
  match i with 
  | None -> None 
  | Some count ->     
    let encode_len = group.dir_length in 
    let index = 
      Ext_string.get_1_2_3_4 whole 
      ~off:(group.module_info_offset + 1 + count * encode_len)
      encode_len
    in 
    let case = not (index mod 2 = 0) in 
    let ith = index lsr 1 in 
    let dir_name_start = 
      if ith = 0 then group.dir_info_offset 
      else 
        Ext_string.index_count 
          whole group.dir_info_offset '\t'
          ith + 1
    in 
    let dir_name_finish = 
      String.index_from
        whole dir_name_start '\t' 
    in    
    Some {case ; dir_name = String.sub whole dir_name_start (dir_name_finish - dir_name_start)}
  
        
      