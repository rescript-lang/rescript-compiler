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



let dep_lit = " :"


let deps_of_channel ic : string array = 
  let size = input_binary_int ic in 
  let s = really_input_string ic size in 
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

(** Please refer to {!Binary_ast} for encoding format, we move it here 
    mostly for cutting the dependency so that [bsb_helper.exe] does
    not depend on compler-libs
*)
let read_deps fn : string array = 
  let ic = open_in_bin fn in 
  let v = deps_of_channel ic in 
  close_in ic;
  v


type kind = Js | Bytecode | Native

let output_file oc source namespace = 
  match namespace with 
  | None -> output_string oc source ;
  | Some ns ->
    output_string oc ( Ext_namespace.make ~ns source)

(** for bucklescript artifacts 
    [lhs_suffix] is [.cmj]
    [rhs_suffix] 
    is [.cmj] if it has [ml] (in this case does not care about mli or not)
    is [.cmi] if it has [mli]
*)

let oc_impl set input_file lhs_suffix rhs_suffix 
    (index : Bsb_dir_index.t)
    (data : Bsb_db.t array)
    namespace
    (oc : out_channel)
  = 
  output_file oc input_file namespace ; 
  output_string oc lhs_suffix; 
  output_string oc dep_lit ; 
  for i = 0 to Array.length set - 1 do
    let k = Array.unsafe_get set i in 
    match String_map.find_opt k data.(0) with
    | Some {ml = Ml_source (source,_,_) }  
      -> 
      output_string oc Ext_string.single_space ;  
      output_file oc source namespace;
      output_string oc rhs_suffix 
    | Some {mli = Mli_source (source,_,_)  } -> 
      output_string oc Ext_string.single_space ;  
      output_file oc source namespace;
      output_string oc Literals.suffix_cmi 
    | Some {mli= Mli_empty; ml = Ml_empty} -> assert false
    | None  -> 
      if Bsb_dir_index.is_lib_dir index  then () 
      else 
        begin match String_map.find_opt k data.((index  :> int)) with 
          | Some {ml = Ml_source (source,_,_) }
            -> 
            output_string oc Ext_string.single_space ;  
            output_file oc source namespace;
            output_string oc rhs_suffix
          | Some {mli = Mli_source (source,_,_) } -> 
            output_string oc Ext_string.single_space ;  
            output_file oc source namespace;
            output_string oc Literals.suffix_cmi 
          | Some {mli = Mli_empty; ml = Ml_empty} -> assert false
          | None -> ()
        end

  done    


(** Note since dependent file is [mli], it only depends on 
    [.cmi] file
*)
let oc_intf
    set
    input_file 
    (index : Bsb_dir_index.t)
    (data : Bsb_db.t array)
    (namespace : string option)
    (oc : out_channel) =   
  output_file oc input_file namespace ; 
  output_string oc Literals.suffix_cmi ; 
  output_string oc dep_lit;
  for i = 0 to Array.length set - 1 do               
    let k = Array.unsafe_get set i in 
    match String_map.find_opt k data.(0) with 
    | Some ({ ml = Ml_source (source,_,_)  }
           | { mli = Mli_source (source,_,_) }) -> 
      output_string oc Ext_string.single_space ; 
      output_file oc source namespace ; 
      output_string oc Literals.suffix_cmi 
    | Some {ml =  Ml_empty; mli = Mli_empty } -> assert false
    | None -> 
      if Bsb_dir_index.is_lib_dir index  then () 
      else 
        match String_map.find_opt k data.((index :> int)) with 
        | Some ({ ml = Ml_source (source,_,_)  }
               | { mli = Mli_source (source,_,_)  }) -> 
          output_string oc Ext_string.single_space ; 
          output_file oc source namespace;
          output_string oc Literals.suffix_cmi
        | Some {ml = Ml_empty; mli = Mli_empty} -> assert false
        | None -> () 
  done  


(* OPT: Don't touch the .d file if nothing changed *)
let emit_dep_file
    compilation_kind
    (fn : string)
    (index : Bsb_dir_index.t) 
    (namespace : string option) : unit = 
  let data  =
    Bsb_db.read_build_cache 
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
    Ext_pervasives.with_file_as_chan (input_file ^ Literals.suffix_mlastd )
      (fun oc -> 
         oc_impl 
           set 
           input_file 
           lhs_suffix 
           rhs_suffix  
           index 
           data
           namespace
           oc
      )
  | None -> 
    begin match Ext_string.ends_with_then_chop fn Literals.suffix_mliast with 
      | Some input_file -> 
        Ext_pervasives.with_file_as_chan (input_file ^ Literals.suffix_mliastd)
          (fun oc -> 
             oc_intf set input_file index data namespace oc 
          )
      | None -> 
        raise (Arg.Bad ("don't know what to do with  " ^ fn))
    end
