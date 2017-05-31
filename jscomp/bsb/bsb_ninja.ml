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


module Rules = Bsb_rule 
let output_build
    ?(order_only_deps=[])
    ?(implicit_deps=[])
    ?(outputs=[])
    ?(implicit_outputs=[])
    ?(inputs=[])
    ?(shadows=[])
    ?restat
    ~output
    ~input
    ~rule
    oc =
  let rule = Bsb_rule.get_name rule  oc in (* Trigger building if not used *)
  output_string oc "build ";
  output_string oc output ;
  outputs |> List.iter (fun s -> output_string oc Ext_string.single_space ; output_string oc s  );
  begin match implicit_outputs with
    | [] -> ()
    | _ ->
      output_string oc " | ";
      implicit_outputs |> List.iter (fun s -> output_string oc Ext_string.single_space ; output_string oc s)
  end;
  output_string oc " : ";
  output_string oc rule;
  output_string oc Ext_string.single_space;
  output_string oc input;
  inputs |> List.iter (fun s ->   output_string oc Ext_string.single_space ; output_string oc s);
  begin match implicit_deps with
    | [] -> ()
    | _ ->
      begin
        output_string oc " | ";
        implicit_deps
        |>
        List.iter (fun s -> output_string oc Ext_string.single_space; output_string oc s )
      end
  end;
  begin match order_only_deps with
    | [] -> ()
    | _ ->
      begin
        output_string oc " || ";
        order_only_deps
        |>
        List.iter (fun s -> output_string oc Ext_string.single_space ; output_string oc s)
      end
  end;
  output_string oc "\n";
  begin match shadows with
    | [] -> ()
    | xs ->
      List.iter (fun (k,v) ->
          output_string oc "  " ;
          output_string oc k ;
          output_string oc " = ";
          match v with
          | `Overwrite s -> output_string oc s ; output_string oc "\n"
          | `Append s ->
            output_string oc "$" ;
            output_string oc k;
            output_string oc Ext_string.single_space;
            output_string oc s ; output_string oc "\n"
        ) xs
  end;
  begin match restat with
    | None -> ()
    | Some () ->
      output_string oc Ext_string.single_space ;
      output_string oc "restat = 1 \n"
  end


let phony ?(order_only_deps=[]) ~inputs ~output oc =
  output_string oc "build ";
  output_string oc output ;
  output_string oc " : ";
  output_string oc "phony";
  output_string oc Ext_string.single_space;
  inputs |> List.iter (fun s ->   output_string oc Ext_string.single_space ; output_string oc s);
  begin match order_only_deps with
    | [] -> ()
    | _ ->
      begin
        output_string oc " || ";
        order_only_deps
        |>
        List.iter (fun s -> output_string oc Ext_string.single_space ; output_string oc s)
      end
  end;
  output_string oc "\n"

let output_kv key value oc  =
  output_string oc key ;
  output_string oc " = ";
  output_string oc value ;
  output_string oc "\n"

let output_kvs kvs oc =
  Array.iter (fun (k,v) -> output_kv k v oc) kvs



let (//) = Ext_filename.combine

type info =
  { all_config_deps : string list  ; (* Figure out [.d] files *)
    (*all_installs :  string list*)
    }

let zero : info =
  { all_config_deps = [] ;
    (*all_installs = []*)
  }

let (++) (us : info) (vs : info) =
  if us == zero then vs else
  if vs == zero then us
  else
    {
      all_config_deps  = us.all_config_deps @ vs.all_config_deps
    ;
      (*all_installs = us.all_installs @ vs.all_installs*)
    }

(** This set is stateful, we should make it functional in the future.
    It only makes sense when building one project combined with [-regen]
*)
(* let files_to_install = String_hash_set.create 96 *)

let install_file (file : string) files_to_install =
  String_hash_set.add  files_to_install (Ext_filename.chop_extension_if_any file )

let handle_file_group oc ~custom_rules 
    ~package_specs ~js_post_build_cmd  
    (files_to_install : String_hash_set.t) acc (group: Bsb_build_ui.file_group) : info =
  let handle_module_info  oc  module_name
      ( module_info : Binary_cache.module_info)
      info  =
    let installable =
      match group.public with
      | Export_all -> true
      | Export_none -> false
      | Export_set set ->  String_set.mem module_name set in
    let emit_build (kind : [`Ml | `Mll | `Re | `Mli | `Rei ])  file_input : info =
      let filename_sans_extension = Filename.chop_extension file_input in
      let input = Bsb_config.proj_rel file_input in
      let output_file_sans_extension = filename_sans_extension in
      let output_ml = output_file_sans_extension ^ Literals.suffix_ml in
      let output_mlast = output_file_sans_extension  ^ Literals.suffix_mlast in
      let output_mlastd = output_file_sans_extension ^ Literals.suffix_mlastd in
      let output_mliast = output_file_sans_extension ^ Literals.suffix_mliast in
      let output_mliastd = output_file_sans_extension ^ Literals.suffix_mliastd in
      let output_cmi = output_file_sans_extension ^ Literals.suffix_cmi in
      let output_cmj =  output_file_sans_extension ^ Literals.suffix_cmj in
      let output_js =
        String_set.fold (fun s acc ->
          Bsb_config.package_output ~format:s (Ext_filename.output_js_basename output_file_sans_extension)
          :: acc
          ) package_specs []
      in
      (* let output_mldeps = output_file_sans_extension ^ Literals.suffix_mldeps in  *)
      (* let output_mlideps = output_file_sans_extension ^ Literals.suffix_mlideps in  *)
      let shadows =
        ( "bs_package_flags",
          `Append
            (String_set.fold (fun s acc ->
                 Ext_string.inter2 acc (Bsb_config.package_flag ~format:s (Filename.dirname output_cmi))

               ) package_specs Ext_string.empty)
        ) ::
        (if group.dir_index = 0 then [] else
           [
             "bs_package_includes", `Append "$bs_package_dev_includes"
             ;
             ("bsc_extra_includes",
              `Overwrite
                ("${" ^ Bsb_build_util.string_of_bsb_dev_include group.dir_index ^ "}")
             )
           ]
        )
      in
      if kind = `Mll then
        output_build oc
          ~output:output_ml
          ~input
          ~rule: Rules.build_ml_from_mll ;
      begin match kind with
        | `Mll
        | `Ml
        | `Re ->
          let input, rule  =
            if kind = `Re then
              input, Rules.build_ast_and_deps_from_reason_impl
            else if kind = `Mll then
              output_ml, Rules.build_ast_and_deps
            else
              input, Rules.build_ast_and_deps
          in
          begin
            output_build oc
              ~output:output_mlast
              (* ~implicit_outputs:[output_mldeps] *)
              ~input
              ~rule;
            output_build
              oc
              ~output:output_mlastd
              ~input:output_mlast
              ~rule:Rules.build_bin_deps
              ?shadows:(if group.dir_index = 0 then None
                else Some [Bsb_build_schemas.bsb_dir_group, `Overwrite (string_of_int group.dir_index)])
            ;
            let rule_name , cm_outputs, deps =
              if module_info.mli = Mli_empty then
                Rules.build_cmj_cmi_js, [output_cmi], []
              else  Rules.build_cmj_js, []  , [output_cmi]

            in
            let shadows =
              match js_post_build_cmd with
              | None -> shadows
              | Some cmd ->
                ("postbuild",
                 `Overwrite ("&& " ^ cmd ^ Ext_string.single_space ^ String.concat Ext_string.single_space output_js)) :: shadows
            in
            output_build oc
              ~output:output_cmj
              ~shadows
              ~outputs:  (output_js @ cm_outputs)
              ~input:output_mlast
              ~implicit_deps:deps
              ~rule:rule_name ;
            if installable then begin install_file file_input files_to_install end;
            {all_config_deps = [output_mlastd]; 
            (*all_installs = [output_cmi];  *)
            }

          end
        | `Mli
        | `Rei ->
          let rule =
            if kind = `Mli then Rules.build_ast_and_deps
            else Rules.build_ast_and_deps_from_reason_intf  in
          output_build oc
            ~output:output_mliast
            (* ~implicit_outputs:[output_mlideps] *)
            ~input
            ~rule;
          output_build oc
            ~output:output_mliastd
            ~input:output_mliast
            ~rule:Rules.build_bin_deps
            ?shadows:(if group.dir_index = 0 then None
                      else Some [Bsb_build_schemas.bsb_dir_group, `Overwrite (string_of_int group.dir_index)])
          ;
          output_build oc
            ~shadows
            ~output:output_cmi
            ~input:output_mliast
            (* ~implicit_deps:[output_mliastd] *)
            ~rule:Rules.build_cmi;
          if installable then begin install_file file_input files_to_install end ;
          {
            all_config_deps = [output_mliastd];
            (*all_installs = [output_cmi] ;*)

          }

      end
    in
    begin match module_info.ml with
      | Ml input -> emit_build `Ml input
      | Re input -> emit_build `Re input
      | Ml_empty -> zero
    end ++
    begin match module_info.mli with
      | Mli mli_file  ->
        emit_build `Mli mli_file
      | Rei rei_file ->
        emit_build `Rei rei_file
      | Mli_empty -> zero
    end ++
    begin match module_info.mll with
      | Some mll_file ->
        begin match module_info.ml with
          | Ml_empty -> emit_build `Mll mll_file
          | Ml input | Re input ->
            failwith ("both "^ mll_file ^ " and " ^ input ^ " are found in source listings" )
        end
      | None -> zero
    end ++ info

  in
  let map_to_source_dir = 
    (fun x -> Bsb_config.proj_rel (group.dir //x )) in
  group.generators
  |> List.iter (fun  ({output; input; command}  : Bsb_build_ui.build_generator)-> 
      begin match String_map.find_opt command custom_rules with 
      | None -> Ext_pervasives.failwithf ~loc:__LOC__ "custom rule %s used but  not defined" command
      | Some rule -> 
        begin match output, input with
        | output::outputs, input::inputs -> 
          output_build oc 
            ~outputs:(List.map map_to_source_dir  outputs)
            ~inputs:(List.map map_to_source_dir inputs) 
            ~output:(map_to_source_dir output)
            ~input:(map_to_source_dir input)
            ~rule
        | [], _ (* either output or input can not be empty *)
        | _, []  -> assert false (* Error should be raised earlier *)
        end
      end
  );  (* we need create a rule for it --
  {[
    rule ocamllex 
  ]}
  *)
  begin 
  String_map.fold (fun  k v  acc ->
      handle_module_info  oc k v acc
    ) group.sources  acc ; 
  end


let handle_file_groups
 oc ~package_specs ~js_post_build_cmd
  ~files_to_install ~custom_rules
  (file_groups  :  Bsb_build_ui.file_group list) st =
  List.fold_left (handle_file_group oc ~package_specs ~custom_rules ~js_post_build_cmd files_to_install ) st  file_groups
