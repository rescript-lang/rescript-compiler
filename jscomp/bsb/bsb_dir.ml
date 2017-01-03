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

(*
type ('a,'b) result = 
  | Ok of 'a
  | Error of 'b

let warp f x = 
  try Ok (f x ) with e -> Error e

let (!) = Lazy.force 

type dir =
  {
    dir_mtime : float ; 
    dir_contents : string array ;
  }

type t = String_hashtbl.t 
(* let cache = String_hashtbl.create 103 *)

let dir_cache_magic_number = "BSDIR20161020"

let write_dir_cache (fname : string)  (x : t) = 
  let oc = open_out_bin fname in 
  output_string oc dir_cache_magic_number ;
  output_value oc x ; 
  close_out oc 

let read_dir_cache (fname : string) : t = 
  let ic = open_in fname in 
  let buffer = really_input_string ic (String.length dir_cache_magic_number) in
  assert (buffer = dir_cache_magic_number);
  let res : t = input_value ic  in 
  close_in ic ; 
  res

(** FIXME: we should not share directory caches, since 
    it may result in  concurrent write issues
    Note, if no dir is ever read, we can leave without
    this cache

    TODO: does it make sense to share with other cache,
    seems like not?
*)
let cache_name = ".bs_dir_cache"

let cache = 
  lazy (try read_dir_cache cache_name with _ -> String_hashtbl.create 103)

let cache_dirty = ref false 

let flush_cache () = 
  if cache_dirty.contents then 
    write_dir_cache cache_name !cache

let () = Pervasives.at_exit flush_cache
    
let readdir dir =
  let stat = Unix.stat dir in 
  let st_mtime = stat.st_mtime in 
  match String_hashtbl.find !cache dir with
  | {dir_mtime} as e when st_mtime <= dir_mtime ->  
    e.dir_contents
  | _ -> 
    let res =  Sys.readdir dir in
    cache_dirty := true; 
    String_hashtbl.replace !cache dir {dir_mtime = st_mtime ; dir_contents = res}; 
    res
  | exception Not_found ->
    let res =  Sys.readdir dir in
    cache_dirty := true ;
    String_hashtbl.add !cache dir {dir_mtime = st_mtime ; dir_contents = res}; 
    res

let  reset_readdir_cache () =
  cache_dirty := true ; 
  String_hashtbl.clear !cache

let reset_readdir_cache_for dir =
  cache_dirty := true; 
  String_hashtbl.remove !cache dir 
*)

(* TODO: see if it is worth turn caching on
   if turned on, we need make sure avoid data racing issues
*)
let readdir = Sys.readdir 
