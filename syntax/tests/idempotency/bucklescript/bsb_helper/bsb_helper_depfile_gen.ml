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
  Ext_buffer.output_buffer oc buf ;
  close_out oc

(* should be good for small file *)
let load_file name (buf : Ext_buffer.t): unit  =
  let len = Ext_buffer.length buf in
  let ic = open_in_bin name in
  let n = in_channel_length ic in
  if n <> len then begin close_in ic ; write_buf name buf  end
  else
    let holder = really_input_string ic  n in
    close_in ic ;
    if Ext_buffer.not_equal buf holder then
      write_buf name buf
;;
let write_file name  (buf : Ext_buffer.t) =
  if Sys.file_exists name then
    load_file name buf
  else
    write_buf name buf

(* return an non-decoded string *)
let extract_dep_raw_string (fn : string) : string =
  let ic = open_in_bin fn in
  let size = input_binary_int ic in
  let s = really_input_string ic size in
  close_in ic;
  s

(* Make sure it is the same as {!Binary_ast.magic_sep_char}*)
let magic_sep_char = '\n'

let deps_of_channel (ic : in_channel) : string list =
  let size = input_binary_int ic in
  let s = really_input_string ic size in
  let rec aux (s : string) acc (offset : int) size : string list =
    if offset < size then
      let next_tab = String.index_from s offset magic_sep_char in
      aux s
        (String.sub s offset (next_tab - offset)::acc) (next_tab + 1)
        size
    else acc
  in
  aux s [] 1 size





(** Please refer to {!Binary_ast} for encoding format, we move it here
    mostly for cutting the dependency so that [bsb_helper.exe] does
    not depend on compler-libs
*)
let read_deps (fn : string) : string list =
  let ic = open_in_bin fn in
  let v = deps_of_channel ic in
  close_in ic;
  v


type kind = Js | Bytecode | Native

let output_file (buf : Ext_buffer.t) source namespace =
  Ext_buffer.add_string buf
    (Ext_namespace.make ?ns:namespace source)

(** for bucklescript artifacts
    [lhs_suffix] is [.cmj]
    [rhs_suffix]
    is [.cmj] if it has [ml] (in this case does not care about mli or not)
    is [.cmi] if it has [mli]
*)
let oc_cmi buf namespace source =
  Ext_buffer.add_char buf ' ';
  output_file buf source namespace;
  Ext_buffer.add_string buf Literals.suffix_cmi


(* For cases with self cycle
    e.g, in b.ml
    {[
      include B
    ]}
    When ns is not turned on, it makes sense that b may come from third party package.
    Hoever, this case is wont supported.
    It complicates when it has interface file or not.
    - if it has interface file, the current interface will have priority, failed to build?
    - if it does not have interface file, the build will not open this module at all(-bs-read-cmi)

    When ns is turned on, `B` is interprted as `Ns-B` which is a cyclic dependency,
    it can be errored out earlier
*)
let find_module db dependent_module is_not_lib_dir (index : Bsb_dir_index.t) =
  let opt = Bsb_db_decode.find_opt db 0 dependent_module in
  match opt with
  | Some _ -> opt
  | None ->
    if is_not_lib_dir then
      Bsb_db_decode.find_opt db (index : int) dependent_module
    else None
let oc_impl
    (mlast : string)
    (index : Bsb_dir_index.t)
    (db : Bsb_db_decode.t)
    (namespace : string option)
    (buf : Ext_buffer.t)
    (lhs_suffix : string)
    (rhs_suffix : string)
  =
  (* TODO: move namespace upper, it is better to resolve ealier *)
  let has_deps = ref false in
  let cur_module_name = Ext_filename.module_name mlast  in
  let at_most_once : unit lazy_t  = lazy (
    has_deps := true ;
    output_file buf (Ext_filename.chop_extension_maybe mlast) namespace ;
    Ext_buffer.add_string buf lhs_suffix;
    Ext_buffer.add_string buf dep_lit ) in
  Ext_option.iter namespace (fun ns ->
      Lazy.force at_most_once;
      Ext_buffer.add_string buf ns;
      Ext_buffer.add_string buf Literals.suffix_cmi;
    ) ; (* TODO: moved into static files*)
  let is_not_lib_dir = not (Bsb_dir_index.is_lib_dir index) in
  let s = extract_dep_raw_string mlast in
  let offset = ref 1 in
  let size = String.length s in
  while !offset < size do
    let next_tab = String.index_from s !offset magic_sep_char in
    let dependent_module = String.sub s !offset (next_tab - !offset) in
    (if dependent_module = cur_module_name then
      begin
        prerr_endline ("FAILED: " ^ cur_module_name ^ " has a self cycle");
        exit 2
      end
    );
    (match
      find_module db dependent_module is_not_lib_dir index
    with
    | None -> ()
    | Some ({dir_name; case }) ->
      begin
        Lazy.force at_most_once;
        let source =
          Filename.concat dir_name
          (if case then
            dependent_module
          else
            Ext_string.uncapitalize_ascii dependent_module) in
        Ext_buffer.add_char buf ' ';
        output_file buf source namespace;
        Ext_buffer.add_string buf rhs_suffix;

        (* #3260 cmj changes does not imply cmi change anymore *)
        oc_cmi buf namespace source

      end);
    offset := next_tab + 1
  done ;
  if !has_deps then
    Ext_buffer.add_char buf '\n'



(** Note since dependent file is [mli], it only depends on
    [.cmi] file
*)
let oc_intf
    mliast
    (index : Bsb_dir_index.t)
    (db : Bsb_db_decode.t)
    (namespace : string option)
    (buf : Ext_buffer.t) : unit =

  let has_deps = ref false in
  let at_most_once : unit lazy_t = lazy (
    has_deps := true;
    output_file buf (Ext_filename.chop_all_extensions_maybe mliast) namespace ;
    Ext_buffer.add_string buf Literals.suffix_cmi ;
    Ext_buffer.add_string buf dep_lit) in
  Ext_option.iter namespace (fun ns ->
      Lazy.force at_most_once;
      Ext_buffer.add_string buf ns;
      Ext_buffer.add_string buf Literals.suffix_cmi;
    ) ;
  let cur_module_name = Ext_filename.module_name mliast in
  let is_not_lib_dir = not (Bsb_dir_index.is_lib_dir index)  in
  let s = extract_dep_raw_string mliast in
  let offset = ref 1 in
  let size = String.length s in
  while !offset < size do
    let next_tab = String.index_from s !offset magic_sep_char in
    let dependent_module = String.sub s !offset (next_tab - !offset) in
    (if dependent_module = cur_module_name then
       begin
         prerr_endline ("FAILED: " ^ cur_module_name ^ " has a self cycle");
         exit 2
       end
    );
    (match  find_module db dependent_module is_not_lib_dir index
     with
     | None -> ()
     | Some {dir_name; case} ->
       Lazy.force at_most_once;
       oc_cmi buf namespace
         (Filename.concat dir_name
            (if case then dependent_module else
               Ext_string.uncapitalize_ascii dependent_module
            ))
    );
    offset := next_tab + 1
  done;
  if !has_deps then
    Ext_buffer.add_char buf '\n'


let emit_d
  compilation_kind
  (index : Bsb_dir_index.t)
  (namespace : string option) (mlast : string) (mliast : string) =
  let data  =
    Bsb_db_decode.read_build_cache
      ~dir:Filename.current_dir_name in
  let buf = Ext_buffer.create 2048 in
  let filename =
      Ext_filename.new_extension mlast Literals.suffix_d in
  let lhs_suffix, rhs_suffix =
    match compilation_kind with
    | Js       -> Literals.suffix_cmj, Literals.suffix_cmj
    | Bytecode -> Literals.suffix_cmo, Literals.suffix_cmo
    | Native   -> Literals.suffix_cmx, Literals.suffix_cmx
  in
  oc_impl
    mlast
    index
    data
    namespace
    buf
    lhs_suffix
    rhs_suffix ;
  if mliast <> "" then begin
    oc_intf
      mliast
      index
      data
      namespace
      buf
  end;
  write_file filename buf
