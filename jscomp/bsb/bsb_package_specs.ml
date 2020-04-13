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

let ( // ) = Ext_path.combine

(* TODO: sync up with {!Js_package_info.module_system} *)
type format = NodeJS | Es6 | Es6_global

type spec = { format : format; in_source : bool; suffix : string }

module Spec_set = Set.Make (struct
  type t = spec
  let compare = Pervasives.compare
end)

type t = Spec_set.t

let bad_module_format_message_exn ~loc format =
  Bsb_exception.errorf ~loc
    "package-specs: `%s` isn't a valid output module format. It has to be one \
     of:  %s, %s or %s"
    format Literals.commonjs Literals.es6 Literals.es6_global


let supported_format (x : string) loc =
  if x = Literals.commonjs then NodeJS
  else if x = Literals.es6 then Es6
  else if x = Literals.es6_global then Es6_global
  else bad_module_format_message_exn ~loc x


let string_of_format (x : format) =
  match x with
  | NodeJS -> Literals.commonjs
  | Es6 -> Literals.es6
  | Es6_global -> Literals.es6_global


let prefix_of_format (x : format) =
  match x with
  | NodeJS -> Bsb_config.lib_js
  | Es6 -> Bsb_config.lib_es6
  | Es6_global -> Bsb_config.lib_es6_global


let deprecated_bs_suffix_message_warn () =
  Bsb_log.warn
    "@{<warning>DEPRECATED@}: @[top-level 'suffix' field is deprecated;@ \
     please lower your extension-configuration into@ 'package-specs'.@]@."


let bad_suffix_message_warn suffix =
  let open Literals in
  Bsb_log.warn
    "@{<warning>UNSUPPORTED@}: @[package-specs: extension `%s` is \
     unsupported;@ consider one of: %s, %s, %s; %s, %s,@ or %s.@]@."
    suffix suffix_js suffix_mjs suffix_cjs suffix_bs_js suffix_bs_mjs
    suffix_bs_cjs


let supported_suffix (x : string) =
  if
    not
      (List.mem x
         Literals.
           [
             suffix_js;
             suffix_mjs;
             suffix_cjs;
             suffix_bs_js;
             suffix_bs_mjs;
             suffix_bs_cjs;
           ])
  then bad_suffix_message_warn x;
  x


let default_suffix ~deprecated_bs_suffix _format _in_source =
  (* match (format, in_source) with *)
  (* | NodeJS, false -> Literals.suffix_js *)
  (* | NodeJS, true -> Literals.suffix_bs_js *)
  (* | _, false -> Literals.suffix_mjs *)
  (* | _, true -> Literals.suffix_bs_mjs *)

  (* TODO: In the absence of direction to the contrary, the suffix should
     eventually depend on [format] and [in_source]. For now, for
     backwards-compatibility, I'm hardcoding. *)
  if deprecated_bs_suffix then Literals.suffix_bs_js else Literals.suffix_js


module SS = Set.Make (String)

let supported_bs_suffixes =
  Literals.[ suffix_bs_js; suffix_bs_mjs; suffix_bs_cjs ]


(** Produces a [list] of supported, bs-prefixed file-suffixes used in
    [in-source] package-specs. *)
let extract_in_source_bs_suffixes (package_specs : Spec_set.t) =
  let f spec suffixes =
    if spec.in_source && List.mem spec.suffix supported_bs_suffixes then
      SS.add spec.suffix suffixes
    else suffixes
  in
  let suffixes = Spec_set.fold f package_specs SS.empty in
  SS.elements suffixes


let rec from_array ~deprecated_bs_suffix (arr : Ext_json_types.t array) :
    Spec_set.t =
  let specs = ref Spec_set.empty in
  Ext_array.iter arr (fun x ->
      let spec = from_json_single ~deprecated_bs_suffix x in
      if
        Spec_set.exists
          (fun o ->
            spec.in_source == o.in_source && String.equal spec.suffix o.suffix)
          !specs
      then
        Bsb_exception.errorf ~loc:(Ext_json.loc_of x)
          "package-specs: two conflicting module formats with the extension \
           `%s` are both configured to be in-source."
          spec.suffix
      else specs := Spec_set.add spec !specs);
  !specs


(* FIXME: better API without mutating *)
and from_json_single ~deprecated_bs_suffix (x : Ext_json_types.t) : spec =
  match x with
  | Str { str = format; loc } ->
      let format = supported_format format loc in
      {
        format;
        in_source = false;
        suffix = default_suffix ~deprecated_bs_suffix format false;
      }
  | Obj { map; loc } -> (
      match Map_string.find_exn map Bsb_build_schemas._module with
      | Str { str = format } ->
          let format = supported_format format loc in
          let in_source =
            match Map_string.find_opt map Bsb_build_schemas.in_source with
            | Some (True _) -> true
            | Some _ | None -> false
          in
          let suffix =
            match Map_string.find_opt map Bsb_build_schemas.suffix with
            | Some (Str { str = suffix; loc }) -> supported_suffix suffix
            | Some _ ->
                Bsb_exception.errorf ~loc
                  "package-specs: the `suffix` field of the configuration \
                   object must be absent, or a string."
            | None -> default_suffix ~deprecated_bs_suffix format in_source
          in
          { format; in_source; suffix }
      | Arr _ ->
          Bsb_exception.errorf ~loc
            "package-specs: when the configuration is an object, `module` \
             field should be a string, not an array. If you want to pass \
             multiple module specs, try turning package-specs into an array of \
             objects (or strings) instead."
      | _ ->
          Bsb_exception.errorf ~loc
            "package-specs: the `module` field of the configuration object \
             should be a string."
      | exception _ ->
          Bsb_exception.errorf ~loc
            "package-specs: when the configuration is an object, the `module` \
             field is mandatory." )
  | _ ->
      Bsb_exception.errorf ~loc:(Ext_json.loc_of x)
        "package-specs: we expect either a string or an object."


let from_json ?(deprecated_bs_suffix = false) (x : Ext_json_types.t) :
    Spec_set.t =
  if deprecated_bs_suffix then deprecated_bs_suffix_message_warn ();
  match x with
  | Arr { content; _ } -> from_array ~deprecated_bs_suffix content
  | _ -> Spec_set.singleton (from_json_single ~deprecated_bs_suffix x)


let bs_package_output = "-bs-package-output"

(** Assume input is valid

    {[ -bs-package-output commonjs:lib/js/jscomp/test:mjs ]} *)
let package_flag ({ format; in_source; suffix } : spec) dir =
  Ext_string.inter2 bs_package_output
    (Ext_string.concat5 (string_of_format format) Ext_string.single_colon
       (if in_source then dir else prefix_of_format format // dir)
       Ext_string.single_colon suffix)


let flags_of_package_specs (package_specs : t) (dirname : string) : string =
  Spec_set.fold
    (fun format acc -> Ext_string.inter2 acc (package_flag format dirname))
    package_specs Ext_string.empty


let default_package_specs ~deprecated_bs_suffix =
  Spec_set.singleton
    {
      format = NodeJS;
      in_source = false;
      suffix = default_suffix ~deprecated_bs_suffix NodeJS false;
    }


(** [get_list_of_output_js specs true "src/hi/hello"] *)
let get_list_of_output_js (package_specs : Spec_set.t)
    (output_file_sans_extension : string) =
  Spec_set.fold
    (fun spec acc ->
      let basename =
        Ext_namespace.replace_namespace_with_extension
          ~name:output_file_sans_extension ~ext:spec.suffix
      in
      ( Bsb_config.proj_rel
      @@
      if spec.in_source then basename
      else prefix_of_format spec.format // basename )
      :: acc)
    package_specs []


let list_dirs_by (package_specs : Spec_set.t) (f : string -> unit) =
  Spec_set.iter
    (fun (spec : spec) ->
      if not spec.in_source then f (prefix_of_format spec.format))
    package_specs
