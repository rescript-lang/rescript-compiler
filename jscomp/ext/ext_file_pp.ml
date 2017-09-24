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

type action = 
  (out_channel -> int -> unit)

  


type interval = {
  loc_start : Lexing.position ; 
  loc_end : Lexing.position ; 
  action : action 
}

let interval_compare x y = 
  Pervasives.compare (x.loc_start.pos_cnum : int) y.loc_start.pos_cnum

(*
  It tries to copy io stream from [ic] into [oc]
  except it will skip those wholes, for each 
  whole a callback can be attached
  When come across a whole, 
  it will print 
  - line directive (based on previous info)
  - all content before
*)
let process_wholes 
    (whole_intervals : interval list ) 
    file_size
    ?line_directive ic oc 
  = 
  let buf = Buffer.create 4096 in 
  let rec aux (cur, line, offset)  wholes = 
    seek_in ic cur ;
    begin match line_directive with 
      | Some fname -> 
        output_string oc "# ";
        output_string oc  (string_of_int line);
        output_string oc " \"";
        output_string oc fname; (* TOOD escape ? *)
        output_string oc "\"\n";
      | None -> ()
    end;
    if offset <> 0 then 
      begin 
        output_string oc (String.make offset ' ')
      end; 
    let print next = 
      Buffer.add_channel buf ic (next - cur) ;
      Buffer.output_buffer oc buf ; 
      Buffer.clear buf 
    in 
    match wholes with 
    | [] -> print file_size
    | {
      loc_start = 
        {Lexing.pos_cnum = start   };
      loc_end  = {Lexing.pos_cnum = stop; pos_bol ; pos_lnum} ;
      action 
    } :: xs  -> 
      let offset = stop - pos_bol in
      print start ;
      action oc offset ;
      aux (stop, pos_lnum, offset) xs 
  in 
    aux (0, 1, 0) whole_intervals


let print_arrays file_array oc offset  =
  let indent = String.make offset ' ' in 
  let p_str s = 
    output_string oc indent ; 
    output_string oc s ;
    output_string oc "\n"
  in
  let len = String_vec.length file_array in 
  match len with 
  | 0
    -> output_string oc "[ ]\n"
  | 1 
    -> output_string oc ("[ \"" ^ String_vec.get file_array 0  ^ "\" ]\n")
  | _ (* first::(_::_ as rest) *)
    -> 
    output_string oc "[ \n";
    String_vec.iter_range ~from:0 ~to_:(len - 2 ) 
      (fun s -> p_str @@ "\"" ^ s ^ "\",") file_array;
    p_str @@ "\"" ^ (String_vec.last file_array) ^ "\"";

    p_str "]"

let patch_action file_array 
  loc_start loc_end  =
  {loc_start ; loc_end ; 
  action = print_arrays file_array 
  }   


(* TODO: in the future, support [bspp.exe]
  with line directive as well
 *)
(*let cpp_process_file fname 
  (whole_intervals : (Lexing.position * Lexing.position) list)
  oc = 
  let ic = open_in_bin fname in
  let file_size = in_channel_length ic in 
  process_wholes ~line_directive:fname 
    (Ext_list.map (fun (x,y) -> {loc_start = x ; loc_end = y; action = Skip}) whole_intervals)
    file_size   ic oc ;
  close_in ic *)
