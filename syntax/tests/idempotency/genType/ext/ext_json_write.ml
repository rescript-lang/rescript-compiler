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

(** poor man's serialization *)

let quot x = 
    "\"" ^ String.escaped x ^ "\""

let rec encode_aux (x : Ext_json_types.t ) 
    (buf : Buffer.t) : unit =  
  let a str = Buffer.add_string buf str in 
  match x with 
  | Null _ -> a "null"
  | Str {str = s; _}  -> a (quot s)
  | Flo {flo = s; _} -> 
    a s (* 
    since our parsing keep the original float representation, we just dump it as is, there is no cases like [nan] *)
  | Arr  {content; _} -> 
    begin match content with 
      | [||] -> a "[]"
      | _ -> 
        a "[ ";
        encode_aux
          (Array.unsafe_get content 0)
          buf ; 
        for i = 1 to Array.length content - 1 do 
          a " , ";
          encode_aux 
            (Array.unsafe_get content i)
            buf
        done;    
        a " ]"
    end
  | True _ -> a "true"
  | False _ -> a "false"
  | Obj {map; _} -> 
    if String_map.is_empty map then 
      a "{}"
    else 
      begin  
        (*prerr_endline "WEIRD";
        prerr_endline (string_of_int @@ String_map.cardinal map );   *)
        a "{ ";
        let _ : int =  String_map.fold (fun  k v i -> 
            if i <> 0 then begin
              a " , " 
            end; 
            a (quot k);
            a " : ";
            encode_aux v buf ;
            i + 1 
          ) map 0 in 
          a " }"
      end


let to_string (x : Ext_json_types.t) = 
    let buf = Buffer.create 1024 in 
    encode_aux x buf ;
    Buffer.contents buf 

let to_channel (oc : out_channel) x  = 
    let buf = Buffer.create 1024 in 
    encode_aux x buf ;
    Buffer.output_buffer oc buf 
