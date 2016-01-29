(* OCamlScript compiler
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

let of_list kvs = 
  let map = Hashtbl.create 51 in 
  List.iter (fun (k, v) -> Hashtbl.add map k v) kvs ; 
  map


let of_list2 ks vs = 
  let map = Hashtbl.create 51 in 
  List.iter2 (fun k v -> Hashtbl.add map k v) ks vs ; 
  map

let add_list map kvs =    
  List.iter (fun (k, v) ->   Hashtbl.add map  k v) kvs 
