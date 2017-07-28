(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

type 'a t = {mutable next : int ; mutable data : 'a array}

let default_size = 32
;;

let create x = {next = 0 ; data = Array.make default_size x}
and reset t = t.next <- 0
;;

let incr_table table new_size =
  let t = Array.make new_size table.data.(0) in
  Array.blit table.data 0 t 0 (Array.length table.data) ;
  table.data <- t

let emit table i =
 let size = Array.length table.data in
 if table.next >= size then
    incr_table table (2*size);
 table.data.(table.next) <- i ;
 table.next <- table.next + 1
;;


exception Error

let get t i =
  if 0 <= i && i < t.next then
    t.data.(i)
  else
    raise Error

let trim t =
  let r = Array.sub t.data 0 t.next in
  reset t ;
  r

let iter t f =
  let size = t.next
  and data = t.data in
  for i = 0 to size-1 do
    f data.(i)
  done

let size t = t.next
