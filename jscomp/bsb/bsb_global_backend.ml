(* Copyright (C) 2019 - Authors of BuckleScript
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



let backend = ref Bsb_config_types.Js

let lib_artifacts_dir = ref Bsb_config.lib_bs

let lib_ocaml_dir = ref Bsb_config.lib_ocaml

let backend_string = ref Literals.js



#if BS_NATIVE then
let (//) = Ext_path.combine
let backend_is_set = ref false
let set_backend b =
  backend_is_set := true;
  backend := b;
  match b with
  | Bsb_config_types.Js       -> 
    lib_artifacts_dir := Bsb_config.lib_bs;
    lib_ocaml_dir := Bsb_config.lib_ocaml;
    backend_string := Literals.js;
  | Bsb_config_types.Native   -> 
    lib_artifacts_dir := Bsb_config.lib_lit // "bs-native";
    lib_ocaml_dir := Bsb_config.lib_lit // "ocaml-native";
    backend_string := Literals.native;
  | Bsb_config_types.Bytecode -> 
    lib_artifacts_dir := Bsb_config.lib_lit // "bs-bytecode";
    lib_ocaml_dir := Bsb_config.lib_lit // "ocaml-bytecode";
    backend_string := Literals.bytecode;

#end