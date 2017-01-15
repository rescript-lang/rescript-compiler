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

let (//) = Ext_filename.combine

let output_ninja
    ~builddir
    ~cwd
    ~js_post_build_cmd
    ~package_specs
    bsc
    bsdep
    package_name
    ocamllex
    bs_external_includes
    (bs_file_groups : Bsb_build_ui.file_group list)
    bsc_flags
    ppx_flags
    bs_dependencies
    refmt

  =
  let ppx_flags = Bsb_build_util.flag_concat "-ppx" ppx_flags in
  let bs_groups  , source_dirs,static_resources  =
    List.fold_left (fun (acc, dirs,acc_resources) ({Bsb_build_ui.sources ; dir; resources }) ->
        String_map.merge (fun modname k1 k2 ->
            match k1 , k2 with
            | None , None ->
              assert false
            | Some a, Some b  ->
              failwith ("conflict files found: " ^ modname)
            | Some v, None  -> Some v
            | None, Some v ->  Some v
          ) acc  sources ,  dir::dirs , (List.map (fun x -> dir // x ) resources) @ acc_resources
      ) (String_map.empty,[],[]) bs_file_groups in
  Binary_cache.write_build_cache (builddir // Binary_cache.bsbuild_cache) 
    (bs_groups : Binary_cache.module_info String_map.t) ;
  let bsc_flags =
    String.concat " " bsc_flags
  in
  let bsc_includes =
    Bsb_build_util.flag_concat "-I" @@ (bs_external_includes @ source_dirs  )
  in
  let oc = open_out_bin (builddir // Literals.build_ninja) in
  begin
    let () =
      output_string oc "ninja_required_version = 1.7.1 \n" ;
      output_string oc "bs_package_flags = ";
      begin match package_name with
        | None -> ()
        | Some x -> 
          output_string oc ("-bs-package-name "  ^ x  )
      end;
      output_string oc "\n"
    in
    let bs_package_includes =
      Bsb_build_util.flag_concat "-bs-package-include" bs_dependencies in

    let () =
      oc
      |>
      Bsb_ninja.output_kvs
        [
          "src_root_dir", cwd (* TODO: need check its integrity*);

          "bsc", bsc ;
          "bsdep", bsdep;
          "ocamllex", ocamllex;
          "bsc_includes", bsc_includes ;
          "bsc_flags", bsc_flags ;
          "ppx_flags", ppx_flags;
          "bs_package_includes", bs_package_includes;
          "refmt", refmt;
          (* "builddir", builddir; we should not have it set, since it's correct here *)

        ]
    in
    let all_deps, all_cmis =
      Bsb_ninja.handle_file_groups oc       
        ~js_post_build_cmd  ~package_specs bs_file_groups ([],[]) in
    let all_deps =
      (* we need copy package.json into [_build] since it does affect build output *)
      (* 
         it is a bad idea to copy package.json which requires to copy js files
      *)
      static_resources
      |> List.fold_left (fun all_deps x ->
          Bsb_ninja.output_build oc
            ~output:x
            ~input:(Bsb_config.proj_rel x)
            ~rule:Bsb_ninja.Rules.copy_resources;
          x:: all_deps
        ) all_deps in
    Bsb_ninja.phony oc ~order_only_deps:all_deps
      ~inputs:[]
      ~output:Literals.build_ninja ;
    close_out oc;
  end
