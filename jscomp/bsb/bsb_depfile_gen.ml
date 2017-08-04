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


let (//) = Filename.concat
let dep_lit = " :"
let space = " "
let length_space = String.length space 

(** Please refer to {!Binary_ast} for encoding format, we move it here 
    mostly for cutting the dependency so that [bsb_helper.exe] is lean
*)
let read_deps fn = 
  let ic = open_in_bin fn in 
  let size = input_binary_int ic in 
  let s = really_input_string ic size in 
  close_in ic;
  let first_tab  = String.index s '\t' in 
  let return_arr = Array.make (int_of_string (String.sub s 0 first_tab)) "" in 
  let rec aux s ith offset = 
    if offset >= size then 
      ()
    else 
      let next_tab = String.index_from s offset '\t'  in 
      return_arr.(ith) <- String.sub s offset (next_tab - offset) ; 
      aux s (ith + 1) (next_tab + 1) in 
  aux s 0 (first_tab + 1) ; 

  return_arr 

type kind = Js | Bytecode | Native

(** for bucklescript artifacts 
    [lhs_suffix] is [.cmj]
    [rhs_suffix] 
    is [.cmj] if it has [ml] (in this case does not care about mli or not)
    is [.cmi] if it has [mli]
*)
let aux_impl set input_file lhs_suffix rhs_suffix 
    (index : Bsb_dir_index.t)
    (data : Bsb_build_cache.t array) : string = 
  let dependent_file = input_file ^ lhs_suffix ^ dep_lit in
  let (files, len) = 
    Array.fold_left
      (fun ((acc, len) as v) k  -> 
         match String_map.find_opt k data.(0) with
         | Some {ml = Ml_source s | Re_source s  }  
           -> 
           let new_file =  Filename.chop_extension s ^ rhs_suffix  
           in (new_file :: acc , len + String.length new_file + length_space)
         | Some {mli = Mli_source s | Rei_source s } -> 
           let new_file =  Filename.chop_extension s ^ Literals.suffix_cmi in
           (new_file :: acc , len + String.length new_file + length_space)
         | Some _ -> assert false
         | None  -> 
           if Bsb_dir_index.is_lib_dir index  then v 
           else 
             begin match String_map.find_opt k data.((index  :> int)) with 
               | Some {ml = Ml_source s | Re_source s  }
                 -> 
                 let new_file =  Filename.chop_extension s ^ rhs_suffix  
                 in (new_file :: acc , len + String.length new_file + length_space)
               | Some {mli = Mli_source s | Rei_source s } -> 
                 let new_file =  Filename.chop_extension s ^ Literals.suffix_cmi in
                 (new_file :: acc , len + String.length new_file + length_space)
               | Some _ -> assert false
               | None -> 
                 v
             end
      )  ([],String.length dependent_file) set in
  Ext_string.unsafe_concat_with_length len
    space
    (dependent_file :: files) 

(** Note since dependent file is [mli], it only depends on 
  [.cmi] file
*)
let aux_intf
    set
    input_file 
    (index : Bsb_dir_index.t)
    (data : Bsb_build_cache.t array) =     
  let dependent_file = input_file ^ Literals.suffix_cmi ^ dep_lit in
  let (files, len) = 
    Array.fold_left
      (fun ((acc, len) as v) k ->
         match String_map.find_opt k data.(0) with 
         | Some ({ ml = Ml_source f | Re_source f  }
                | { mli = Mli_source f | Rei_source f }) -> 
           let new_file = Filename.chop_extension f ^ Literals.suffix_cmi in
           (new_file :: acc , len + String.length new_file + length_space)
         | Some _ -> assert false
         | None -> 
           if Bsb_dir_index.is_lib_dir index  then v 
           else 
             begin  match String_map.find_opt k data.((index :> int)) with 
               | Some ({ ml = Ml_source f | Re_source f  }
                      | { mli = Mli_source f | Rei_source f }) -> 
                 let new_file = Filename.chop_extension f ^ Literals.suffix_cmi in
                 (new_file :: acc , len + String.length new_file + length_space)
               | Some _ -> assert false
               | None -> v
             end

      )   ([], String.length dependent_file) set in 

  Ext_string.unsafe_concat_with_length len
    space 
    (dependent_file :: files) 


(* TODO: Don't touch the .d file if nothing changed *)
let make
    compilation_kind
    (fn : string)
    (index : Bsb_dir_index.t) : unit = 
  let data  =
    Bsb_build_cache.read_build_cache 
      ~dir:Filename.current_dir_name
  in 
  let set = read_deps fn in 
  match Ext_string.ends_with_then_chop fn Literals.suffix_mlast with 
  | Some  input_file -> 
    let lhs_suffix, rhs_suffix =
      match compilation_kind with
      | Js       -> Literals.suffix_cmj, Literals.suffix_cmj
      | Bytecode -> Literals.suffix_cmi, Literals.suffix_cmo
      | Native   -> Literals.suffix_cmx, Literals.suffix_cmx in
    let deps = 
      aux_impl set input_file 
        lhs_suffix rhs_suffix  index data in    
    let output = input_file ^ Literals.suffix_mlastd in                
    Ext_pervasives.with_file_as_chan output  (fun v -> output_string v deps)

  | None -> 
    begin match Ext_string.ends_with_then_chop fn Literals.suffix_mliast with 
      | Some input_file -> 
        let deps = aux_intf set input_file index data in 
        let output = input_file ^ Literals.suffix_mliastd in
        Ext_pervasives.with_file_as_chan output  
          (fun v -> output_string v deps)
      | None -> 
        raise (Arg.Bad ("don't know what to do with  " ^ fn))
    end
