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
external undef : 'a def = "undefined" [@@js.global]

let stdin = undef
let stdout = undef 
let stderr = undef 

let caml_ml_open_descriptor_in (i : int) : in_channel = 
  raise (Failure "caml_ml_open_descriptor_in not implemented")  
let caml_ml_open_descriptor_out (i : int)  : out_channel = 
  raise (Failure "caml_ml_open_descriptor_out not implemented")
let caml_ml_output_char (oc : out_channel)  (char : char) =
  raise (Failure "caml_ml_output_char not implemented"  )

(** note we need provide both [bytes] and [string] version 
*)
let caml_ml_output (oc : out_channel) (bytes : bytes) offset len  =
  raise @@ Failure  "caml_ml_output not implemented"  
let caml_ml_input (ic : in_channel) (bytes : bytes) offset len : int = 
  raise @@ Failure  "caml_ml_input ic not implemented"

let caml_ml_input_char (ic : in_channel) : char = 
  raise @@ Failure "caml_ml_input_char not implemnted"

let caml_ml_out_channels_list () : out_channel list  =
  assert false 

let caml_ml_flush oc  = 
  assert false 

