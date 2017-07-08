(* Copyright (C) 2017 Authors of BuckleScript
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


let (//) = Ext_filename.combine

let clean_bs_garbage cwd =
  Format.fprintf Format.std_formatter "@{<info>Cleaning:@} in %s@." cwd ; 
  let aux x =
    let x = (cwd // x)  in
    if Sys.file_exists x then
      Bsb_unix.remove_dir_recursive x  in
  try
    List.iter aux Bsb_config.all_lib_artifacts
  with
    e ->
    Format.fprintf Format.err_formatter "@{<warning>Failed@} to clean due to %s" (Printexc.to_string e)


let clean_bs_deps cwd =
  Bsb_build_util.walk_all_deps  cwd  (fun { cwd} ->
      (* whether top or not always do the cleaning *)
      clean_bs_garbage cwd
    )

let clean_self cwd = clean_bs_garbage cwd