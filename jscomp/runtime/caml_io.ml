(* BuckleScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Author: Hongbo Zhang  *)

type 'a def


let stdin = Js.undef

let stderr = Js.undef 

type out_channel  = {
  mutable buffer :  string;
  output :   out_channel  -> string -> unit 
}

let stdout = {
  buffer = "";
  output = (fun _ s ->
    let v = String.length s - 1 in
    if [%js.raw{| process && process.stdout && process.stdout.write|}] then
      ([%js.raw{| process.stdout.write |} ] : string -> unit) s
    else         
    if s.[v] = '\n' then
      Js.log (Js.String.slice s 0 v)
    else Js.log s)
}

let stderr = {
  buffer = "";
  output = fun _ s ->
    let v = String.length s - 1 in     
    if s.[v] = '\n' then
      Js.log (Js.String.slice s 0 v) (* TODO: change to Js.error*)
    else Js.log s        
}

let caml_ml_open_descriptor_in (i : int) : in_channel = 
  raise (Failure "caml_ml_open_descriptor_in not implemented")  
let caml_ml_open_descriptor_out (i : int)  : out_channel = 
  raise (Failure "caml_ml_open_descriptor_out not implemented")

(*TODO: we need flush all buffers in the end *)
let caml_ml_flush (oc : out_channel)  : unit = 
  if oc.buffer  <> "" then
    begin     
      oc.output oc oc.buffer;
      oc.buffer <- ""      
    end      

let node_std_output  : string -> bool = [%js.raw{|function (s){
   return process && process.stdout && (process.stdout.write(s), true);
   }
|}]

(** note we need provide both [bytes] and [string] version 
*)
let caml_ml_output (oc : out_channel) (str : string) offset len  =
  let str =
    if offset = 0 && len = String.length str then str    
    else Js.String.slice str offset len in
  if [%js.raw{|process && process.stdout && process.stdout.write |}] &&
     oc == stdout then
    begin     

      ([%js.raw{|process.stdout.write|}] : string -> unit ) str
    end        
  else
    begin     

      let id = Js.String.lastIndexOf str "\n" in
      if id < 0 then
        oc.buffer <- oc.buffer ^ str
      else
        begin 
          oc.buffer <- oc.buffer ^ Js.String.slice str 0 (id +1);
          caml_ml_flush oc;
          oc.buffer <- oc.buffer ^ Js.String.slice_rest str (id + 1)
        end
    end      

let caml_ml_output_char (oc : out_channel)  (char : char) =
  caml_ml_output oc (Js.String.of_char char)

let caml_ml_input (ic : in_channel) (bytes : bytes) offset len : int = 
  raise @@ Failure  "caml_ml_input ic not implemented"

let caml_ml_input_char (ic : in_channel) : char = 
  raise @@ Failure "caml_ml_input_char not implemnted"

let caml_ml_out_channels_list () : out_channel list  =
  assert false 


