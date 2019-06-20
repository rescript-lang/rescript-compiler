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



let dep_lit = " : "
let write_buf name buf  =     
  let oc = open_out_bin name in 
  Buffer.output_buffer oc buf ;
  close_out oc 

(* should be good for small file *)
let load_file name (buf : Buffer.t): unit  = 
  let len = Buffer.length buf in 
  let ic = open_in_bin name in 
  let n = in_channel_length ic in   
  if n <> len then begin close_in ic ; write_buf name buf  end 
  else
    let holder = really_input_string ic  n in 
    close_in ic ; 
    if holder <> Buffer.contents buf then 
      write_buf name buf 
;;
let write_file name  (buf : Buffer.t) = 
  if Sys.file_exists name then 
    load_file name buf 
  else 
    write_buf name buf 
    
(* Make sure it is the same as {!Binary_ast.magic_sep_char}*)
let magic_sep_char = '\n'

let deps_of_channel (ic : in_channel) : string array = 
  let size = input_binary_int ic in 
  let s = really_input_string ic size in 
  let first_tab  = String.index s magic_sep_char in 
  let return_arr = Array.make (int_of_string (String.sub s 0 first_tab)) "" in 
  let rec aux s ith (offset : int) : unit = 
    if offset < size then
      let next_tab = String.index_from s offset magic_sep_char  in 
      return_arr.(ith) <- String.sub s offset (next_tab - offset) ; 
      aux s (ith + 1) (next_tab + 1) 
  in 
  aux s 0 (first_tab + 1) ; 

  return_arr 

(** Please refer to {!Binary_ast} for encoding format, we move it here 
    mostly for cutting the dependency so that [bsb_helper.exe] does
    not depend on compler-libs
*)
let read_deps (fn : string) : string array = 
  let ic = open_in_bin fn in 
  let v = deps_of_channel ic in 
  close_in ic;
  v


type kind = Js | Bytecode | Native

let output_file (oc : Buffer.t) source namespace = 
  Buffer.add_string oc (match namespace with 
      | None ->  source 
      | Some ns ->
        Ext_namespace.make ~ns source)

(** for bucklescript artifacts 
    [lhs_suffix] is [.cmj]
    [rhs_suffix] 
    is [.cmj] if it has [ml] (in this case does not care about mli or not)
    is [.cmi] if it has [mli]
*)
let oc_cmi buf namespace source = 
  Buffer.add_char buf ' ';  
  output_file buf source namespace;
  Buffer.add_string buf Literals.suffix_cmi 


let handle_module_info 
    (module_info : Bsb_db.module_info)
    input_file 
    namespace rhs_suffix buf = 
  let source = module_info.name_sans_extension in 
  if source <> input_file then 
    begin 
      if module_info.ml_info <> Ml_empty then 
        begin
          Buffer.add_char buf ' ';  
          output_file buf source namespace;
          Buffer.add_string buf rhs_suffix
        end;
      (* #3260 cmj changes does not imply cmi change anymore *)
      oc_cmi buf namespace source
    end

let find_module db dependent_module is_not_lib_dir (index : Bsb_dir_index.t) = 
  let opt = Bsb_db_io.find_opt db 0 dependent_module in 
  match opt with 
  | Some _ -> opt
  | None -> 
    if is_not_lib_dir then 
      Bsb_db_io.find_opt db (index :> int) dependent_module 
    else None 
let oc_impl 
    (dependent_module_set : string array)
    (input_file : string)
    (index : Bsb_dir_index.t)
    (db : Bsb_db_io.t)
    (namespace : string option)
    (buf : Buffer.t)
    (lhs_suffix : string)
    (rhs_suffix : string)
  = 
  (* TODO: move namespace upper, it is better to resolve ealier *)  
  let has_deps = ref false in 
  let at_most_once : unit lazy_t  = lazy (
    has_deps := true ;
    output_file buf input_file namespace ; 
    Buffer.add_string buf lhs_suffix; 
    Buffer.add_string buf dep_lit ) in  
  Ext_option.iter namespace (fun ns -> 
      Lazy.force at_most_once;
      Buffer.add_string buf ns;
      Buffer.add_string buf Literals.suffix_cmi;
    ) ; (* TODO: moved into static files*)
  let is_not_lib_dir = not (Bsb_dir_index.is_lib_dir index) in 
  Ext_array.iter dependent_module_set (fun dependent_module ->
      match  
        find_module db dependent_module is_not_lib_dir index  
      with      
      | None -> ()
      | Some module_info -> 
        begin 
          Lazy.force at_most_once;
          handle_module_info module_info input_file namespace rhs_suffix buf
        end     
    );
  if !has_deps then  
    Buffer.add_char buf '\n'



(** Note since dependent file is [mli], it only depends on 
    [.cmi] file
*)
let oc_intf
    (dependent_module_set : string array)
    input_file 
    (index : Bsb_dir_index.t)
    (db : Bsb_db_io.t)
    (namespace : string option)
    (buf : Buffer.t) : unit =   
  let has_deps = ref false in  
  let at_most_once : unit lazy_t = lazy (  
    has_deps := true;
    output_file buf input_file namespace ;   
    Buffer.add_string buf Literals.suffix_cmi ; 
    Buffer.add_string buf dep_lit) in 
  Ext_option.iter namespace (fun ns -> 
      Lazy.force at_most_once;  
      Buffer.add_string buf ns;
      Buffer.add_string buf Literals.suffix_cmi;
    ) ; 
  let is_not_lib_dir = not (Bsb_dir_index.is_lib_dir index)  in  
  Ext_array.iter dependent_module_set begin fun dependent_module ->
    match  find_module db dependent_module is_not_lib_dir index 
    with     
    | None -> ()
    | Some module_info -> 
      let source = module_info.name_sans_extension in 
      if source <> input_file then
        begin 
          Lazy.force at_most_once; 
          oc_cmi buf namespace source             
        end
  end;
  if !has_deps then
    Buffer.add_char buf '\n'


let emit_d mlast 
  (index : Bsb_dir_index.t) 
  (namespace : string option) has_intf = 
  let data  =
    Bsb_db_io.read_build_cache 
      ~dir:Filename.current_dir_name
  in 
  let set_a = read_deps mlast in 
  let buf = Buffer.create 128 in 
  let input_file = Filename.chop_extension mlast in 
  let filename = input_file ^ Literals.suffix_d in   
  let lhs_suffix = Literals.suffix_cmj in   
  let rhs_suffix = Literals.suffix_cmj in 
  oc_impl 
    set_a 
    input_file 
    index 
    data
    namespace
    buf 
    lhs_suffix 
    rhs_suffix ;      
  if has_intf <> "" then begin
    oc_intf 
      (read_deps has_intf)
      input_file 
      index 
      data 
      namespace 
      buf        
  end;          
  write_file filename buf 

(* OPT: Don't touch the .d file if nothing changed *)
let emit_dep_file
    compilation_kind
    (fn : string)
    (index : Bsb_dir_index.t) 
    (namespace : string option) : unit = 
  let data  =
    Bsb_db_io.read_build_cache 
      ~dir:Filename.current_dir_name
  in 
  let set = read_deps fn in 
  match Ext_string.ends_with_then_chop fn Literals.suffix_mlast with 
  | Some  input_file -> 
#if BS_NATIVE then   
    let lhs_suffix, rhs_suffix =
      match compilation_kind with
      | Js       -> Literals.suffix_cmj, Literals.suffix_cmj
      | Bytecode -> Literals.suffix_cmo, Literals.suffix_cmi
      | Native   -> Literals.suffix_cmx, Literals.suffix_cmx 
    in    
#else     
   let lhs_suffix = Literals.suffix_cmj in   
   let rhs_suffix = Literals.suffix_cmj in 
#end
   let buf = Buffer.create 64 in 
   oc_impl 
     set 
     input_file 
     index 
     data
     namespace
     buf 
     lhs_suffix 
     rhs_suffix       
     ;
   write_file (input_file ^ Literals.suffix_d ) buf 
    
  | None -> 
    begin match Ext_string.ends_with_then_chop fn Literals.suffix_mliast with 
      | Some input_file -> 
        let filename = (input_file ^ Literals.suffix_d) in 
        let buf = Buffer.create 64 in 
        oc_intf 
          set 
          input_file 
          index 
          data 
          namespace 
          buf; 
        write_file filename buf 
      | None -> 
        raise (Arg.Bad ("don't know what to do with  " ^ fn))
    end
