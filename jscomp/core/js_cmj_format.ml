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



[@@@ocaml.warning "+9"]


type arity = 
  | Single of Lam_arity.t
  | Submodule of Lam_arity.t array

(* TODO: add a magic number *)
type cmj_value = {
  arity : arity ;
  persistent_closed_lambda : Lam.t option ; 
  (** Either constant or closed functor *)
}

type effect = string option


let single_na = Single Lam_arity.na
(** we don't force people to use package *)
type cmj_case = Ext_namespace.file_kind
  
type keyed_cmj_values 
  =  (string * cmj_value) array

type t = {
  values : keyed_cmj_values ;
  pure : bool;
  npm_package_path : Js_packages_info.t ;
  cmj_case : cmj_case; 
}
let empty_values = [||]
let mk ~values ~effect ~npm_package_path ~cmj_case : t = 
  {
    values = String_map.to_sorted_array values; 
    pure = effect = None ; 
    npm_package_path;
    cmj_case
  }

let cmj_magic_number =  "BUCKLE20171012"
let cmj_magic_number_length = 
  String.length cmj_magic_number

let pure_dummy = 
  {
    values = empty_values;
    pure = true;
    npm_package_path = Js_packages_info.empty;
    cmj_case = Little_js;
  }

let no_pure_dummy = 
  {
    values = empty_values;
    pure = false;
    npm_package_path = Js_packages_info.empty;  
    cmj_case = Little_js; (** TODO: consistent with Js_config.bs_suffix default *)
  }

let digest_length = 16 (*16 chars *)

let verify_magic_in_beg ic =
  let buffer = really_input_string ic cmj_magic_number_length in 
  if buffer <> cmj_magic_number then
    Ext_pervasives.failwithf ~loc:__LOC__ 
      "cmj files have incompatible versions, please rebuilt using the new compiler : %s" 
        __LOC__


(* Serialization .. *)
let from_file name : t =
  let ic = open_in_bin name in 
  verify_magic_in_beg ic ; 
  let _digest = Digest.input ic in 
  let v  : t = input_value ic in 
  close_in ic ;
  v 

let from_file_with_digest name : t * Digest.t =
  let ic = open_in_bin name in 
  verify_magic_in_beg ic ; 
  let digest = Digest.input ic in 
  let v  : t = input_value ic in 
  close_in ic ;
  v,digest 


let from_string s : t = 
  let magic_number = String.sub s 0 cmj_magic_number_length in 
  if magic_number = cmj_magic_number then 
    Marshal.from_string s  (digest_length + cmj_magic_number_length)
  else 
    Ext_pervasives.failwithf ~loc:__LOC__ 
      "cmj files have incompatible versions, please rebuilt using the new compiler : %s"
        __LOC__

let fixed_length = cmj_magic_number_length + digest_length

let rec for_sure_not_changed (name : string) (header : string) =   
  if Sys.file_exists name then 
    let ic = open_in_bin name in 
    let holder =
      really_input_string ic fixed_length in 
    close_in ic; 
    holder = header
  else false  
    
(* This may cause some build system always rebuild
  maybe should not be turned on by default
*) 
let to_file name ~check_exists (v : t) = 
  let s = Marshal.to_string v [] in 
  let cur_digest = Digest.string s in 
  let header = cmj_magic_number ^ cur_digest in 
  if  not (check_exists && for_sure_not_changed name header) then 
    let oc = open_out_bin name in 
    output_string oc header;    
    output_string oc s;
    close_out oc 

let keyComp (a : string) (b,_) = 
    String_map.compare_key  a b 

let not_found = single_na, None 
let get_result  midVal = 
  let (_,cmj_value) = midVal in 
  cmj_value.arity,
  if Js_config.get_cross_module_inline () then cmj_value.persistent_closed_lambda
  else None 

let rec binarySearchAux arr lo hi (key : string) = 
  let mid = (lo + hi)/2 in 
  let midVal = Array.unsafe_get arr mid in 
  let c = keyComp key midVal in 
  if c = 0 then 
    get_result midVal
  else if c < 0 then  (*  a[lo] =< key < a[mid] <= a[hi] *)
    if hi = mid then  
      let loVal = (Array.unsafe_get arr lo) in 
      if fst loVal = key then get_result loVal
      else not_found
    else binarySearchAux arr lo mid key 
  else  (*  a[lo] =< a[mid] < key <= a[hi] *)
  if lo = mid then 
    let hiVal = (Array.unsafe_get arr hi) in 
    if fst hiVal = key  then get_result hiVal
    else not_found
  else binarySearchAux arr mid hi key

let binarySearch (sorted : keyed_cmj_values) (key : string) =  
  let len = Array.length sorted in 
  if len = 0 then not_found
  else 
    let lo = Array.unsafe_get sorted 0 in 
    let c = keyComp key lo in 
    if c < 0 then not_found
    else
      let hi = Array.unsafe_get sorted (len - 1) in 
      let c2 = keyComp key hi in 
      if c2 > 0 then not_found
      else binarySearchAux sorted 0 (len - 1) key 


(* FIXME: better error message when ocamldep
  get self-cycle
*)    
let query_by_name (cmj_table : t ) name =   
  let values = cmj_table.values in    
  binarySearch values name 

let is_pure (cmj_table : t ) = 
  cmj_table.pure

let get_npm_package_path (cmj_table : t) = 
  cmj_table.npm_package_path

let get_cmj_case (cmj_table : t) =  
  cmj_table.cmj_case


(* start dumping *)

let f fmt = Printf.fprintf stdout fmt 

let pp_cmj_case  (cmj_case : cmj_case) : unit = 
  match cmj_case with 
  | Little_js -> 
    f  "case : little, .js \n"
  | Little_bs -> 
    f  "case : little, .bs.js \n"    
  | Upper_js -> 
    f  "case: upper, .js  \n"
  | Upper_bs -> 
    f  "case: upper, .bs.js  \n"    

let pp_cmj 
    ({ values ; pure; npm_package_path ; cmj_case} : t) = 
  f  "package info: %s\n"  
    (Format.asprintf "%a" Js_packages_info.dump_packages_info npm_package_path)        
  ;
  pp_cmj_case  cmj_case;

  f "effect: %s\n"
      (if pure then "pure" else "not pure");
   Ext_array.iter values 
    (fun (k , {arity; persistent_closed_lambda}) -> 
       match arity with             
       | Single arity ->
         f "%s: %s\n" k (Format.asprintf "%a" Lam_arity.print arity);
         (match persistent_closed_lambda with 
          | None -> 
            f "%s: not saved\n" k 
          | Some lam -> 
            begin 
              f "%s: ======[start]\n" k ;
              f "%s\n" (Lam_print.lambda_to_string lam);
              f "%s: ======[finish]\n" k
            end )         
       | Submodule xs -> 
         (match persistent_closed_lambda with 
          | None -> f "%s: not saved\n" k 
          | Some lam -> 
            begin 
              f "%s: ======[start]\n" k ;
              f "%s" (Lam_print.lambda_to_string lam);
              f "%s: ======[finish]\n" k
            end 
         );
         Array.iteri 
         (fun i arity -> f "%s[%i] : %s \n" 
          k i 
          (Format.asprintf "%a" Lam_arity.print arity ))
         xs
    )    