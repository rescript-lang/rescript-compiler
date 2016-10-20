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


module Schemas = struct 
  let files = "files"
end
type module_info = Binary_cache.module_info 
type 'a file_group = 
  { dir : string ;
    sources : 'a
  } 

let main_ninja = "build.ninja"
let (//) = Filename.concat 
module Default = struct
  let bsc = ref  "bsc.exe"
  let bsbuild = ref "bsbuild.exe"
  let bsdep = ref "bsdep.exe"
  let ocamllex =  ref "ocamllex.opt"
  let bs_external_includes = ref []
  let package_name = ref None
  let bsc_flags = ref []
  let ppx_flags = ref []
  let static_resources = ref []
  let build_output_prefix = ref "_build"
  let bs_file_groups = ref []
end
module Flags = struct 
  let bsc = ("bsc", "bsc.exe")
  let bsbuild = ("bsbuild", "bsbuild.exe")
  let bsdep = ("bsdep", "bsdep.exe")
  let package_name = "package-name"
  let bs_package_name = "-bs-package-name"
  let ocamllex = ("ocamllex", "ocamllex.opt")
  let bs_external_includes = "bs-external-includes"
  let bsc_flags = "bsc-flags"
  let file_groups = "file-groups"
  let ppx_flags = "ppx-flags"
  let build_output_prefix = ("build-output-prefix", Filename.current_dir_name)
  let static_resources = "static-resources"
end


let space = " "



(* TODO check duplication *)
let module_info_of_ml exist ml : module_info =
  match exist with 
  | None -> { ml  = Ml ml ; mli = Mli_empty ; mll = None }
  | Some x -> { x with ml = Ml ml}

let module_info_of_re exist ml : module_info =
  match exist with 
  | None -> { ml  = Re ml ; mli = Mli_empty ; mll = None }
  | Some x -> { x with ml = Re ml} 

let module_info_of_mli exist mli : module_info = 
  match exist with 
  | None -> { mli  = Mli mli ; ml = Ml_empty ; mll = None }
  | Some x -> { x with mli = Mli mli} 

let module_info_of_rei exist mli : module_info = 
  match exist with 
  | None -> { mli  = Rei mli ; ml = Ml_empty ; mll = None }
  | Some x -> { x with mli = Rei mli} 

let module_info_of_mll exist mll : module_info = 
  match exist with 
  | None -> { mll  = Some mll ; ml = Ml_empty ; mli = Mli_empty }
  | Some x -> { x with mll = Some mll} 


let map_update ?dir map  name = 
  let prefix  x = match dir with None -> x | Some v -> v // x in
  let module_name = Ext_filename.module_name_of_file name in 
  let aux v name = 
    if Filename.check_suffix name ".ml" then module_info_of_ml v @@ prefix name  else
    if Filename.check_suffix name ".mll" then module_info_of_mll v @@ prefix  name else 
    if Filename.check_suffix name ".mli" then  module_info_of_mli v @@ prefix name else 
    if Filename.check_suffix name ".re" then  module_info_of_re v @@ prefix name else 
    if Filename.check_suffix name ".rei" then  module_info_of_rei v @@ prefix name else 
      assert false   in 
  match String_map.find module_name map with 
  | exception Not_found 
    -> String_map.add module_name (aux None name) map 
  | v -> 
    String_map.add module_name (aux (Some v) name)  map


let output_ninja 
    bsc
    bsbuild
    bsdep
    package_name
    ocamllex
    build_output_prefix
    bs_external_includes
    static_resources 
    bs_file_groups 
    bsc_flags
    ppx_flags 
  = 
  let ppx_flags =
  String.concat space @@
    Ext_list.flat_map (fun x -> ["-ppx";  x ])  ppx_flags in 
  let bs_files, source_dirs  = List.fold_left (fun (acc,dirs) {sources ; dir } -> 
      String_map.merge (fun modname k1 k2 ->
          match k1 , k2 with
          | None , None 
          | Some _, Some _  -> assert false 
          | Some v, None  -> Some v 
          | None, Some v ->  Some v 
        ) acc  sources , dir::dirs
    ) (String_map.empty,[]) bs_file_groups in
  if not (Sys.file_exists build_output_prefix && Sys.is_directory build_output_prefix) then 
    begin 
      ignore @@ Unix.mkdir build_output_prefix 0o777
    end;
  Binary_cache.write_build_cache (build_output_prefix // Binary_cache.bsbuild_cache) bs_files ;
  let internal_includes =
      source_dirs
      |> Ext_list.flat_map (fun x -> ["-I" ; build_output_prefix // x ]) in 
  let external_includes = 
      Ext_list.flat_map (fun x -> ["-I" ; x]) bs_external_includes in 

  let bsc_parsing_flags =
    String.concat " " bsc_flags 
  in  
  let bsc_computed_flags =
    let init_flags = 
      match package_name with 
      | None -> external_includes @ internal_includes 
      | Some x -> Flags.bs_package_name ::  x :: external_includes @ internal_includes
    in 

    String.concat " " ( bsc_flags @ init_flags)
  in
  let oc = open_out main_ninja in 
  begin 
    let all_deps = ref [] in
    let all_cmis = ref [] in 
    let () = 
      output_string oc "bsc = "; output_string oc bsc ; output_string oc "\n";
      output_string oc "bsc_computed_flags = "; output_string oc bsc_computed_flags ; output_string oc "\n";
      output_string oc "bsc_parsing_flags = "; output_string oc bsc_parsing_flags ; output_string oc "\n";
      output_string oc "bsbuild = "; output_string oc bsbuild ; output_string oc "\n";
      output_string oc "bsdep = "; output_string oc bsdep ; output_string oc "\n";
      output_string oc "ocamllex = "; output_string oc ocamllex ; output_string oc "\n";
      output_string oc "ppx_flags = "; output_string oc ppx_flags ; output_string oc "\n";
      output_string oc "build_output_prefix = "; output_string oc build_output_prefix ; output_string oc "\n";
      output_string oc "builddir = "; output_string oc build_output_prefix ; output_string oc "\n";
      output_string oc {|
# for ast building, we remove most flags with respect to -I 
rule build_ast
  command = ${bsc} ${pp_flags} ${ppx_flags} ${bsc_parsing_flags} -c -o ${out} -bs-syntax-only -bs-binary-ast ${in}
  description = Building ast  ${out}
rule build_ast_from_reason_impl
  command = ${bsc} -pp refmt ${ppx_flags} ${bsc_parsing_flags} -c -o ${out} -bs-syntax-only -bs-binary-ast -impl ${in}     
  description = Building ast from reason re ${out}
rule build_ast_from_reason_intf
  command = ${bsc} -pp refmt ${ppx_flags} ${bsc_parsing_flags} -c -o ${out} -bs-syntax-only -bs-binary-ast -intf ${in}        
  description = Building ast from reason rei  ${out}

rule build_deps
  command = ${bsdep} -bs-oprefix ${build_output_prefix}  -bs-MD ${in}
  description = Building deps ${out}

rule build_ml_from_mll
  command = ${ocamllex} -o ${out} ${in}
  description = Building ml from ml - ${in}
# this rule has multiple output        
rule build_cmj_only
  depfile = ${in}.d
  command = ${bsc} -bs-no-builtin-ppx-ml -bs-no-implicit-include ${bsc_computed_flags} -o ${in} -c -impl ${in}
  description = Building cmj - ${out}

# TODO: Windows path concat 
rule build_cmj_cmi
  depfile = ${in}.d
  command = ${bsc} -bs-assume-no-mli -bs-no-implicit-include -bs-no-builtin-ppx-ml ${bsc_computed_flags} -o ${in} -c -impl ${in}
  description = Building cmj and cmi - ${out}

rule build_cmi
  depfile = ${in}.d
  command = ${bsc} -bs-no-builtin-ppx-mli -bs-no-implicit-include ${bsc_computed_flags} -o ${out} -c -intf ${in}
  description = Building cmi - ${out}
|};
    if static_resources <> []   then 
      output_string oc {|
rule copy_resources
  command = cp ${in}  ${out}
  description = Copying ${in} into ${out}
|}
    in

    bs_files
    |> String_map.iter (fun module_name ({mli; ml; mll } : module_info) -> 
        let spit_out_ml (kind : [`Ml | `Re ])  file filename_sans_extension = 
          if kind = `Ml then 
            output_string oc 
              (Printf.sprintf "build %s.mlast : build_ast %s\n" 
                 (build_output_prefix // filename_sans_extension) file)
          else 
            output_string oc (Printf.sprintf "build %s.mlast : build_ast_from_reason_impl %s\n"
                                (build_output_prefix // filename_sans_extension) file)
          ;
          output_string oc (Printf.sprintf "build %s.mlast.d : build_deps %s.mlast\n" 
                              (build_output_prefix // filename_sans_extension)
                              (* output should always be marked explicitly,
                                 otherwise the build system can not figure out clearly
                                 however, for the command we don't need pass `-o`
                              *)
                              (build_output_prefix // filename_sans_extension));
          all_deps := (build_output_prefix // filename_sans_extension ^ Literals.suffix_mlastd) :: !all_deps;
          let rule_name , output, deps = 
            let cmi_file =
              build_output_prefix // filename_sans_extension ^ Literals.suffix_cmi
            in
            if mli = Mli_empty then "build_cmj_only",  cmi_file  , ""
            else "build_cmj_cmi", "", cmi_file  in  
          output_string oc 
            (Printf.sprintf "build %s.cmj %s : %s %s.mlast |  %s\n"
               (build_output_prefix // filename_sans_extension)
               output
               rule_name 
               (build_output_prefix // filename_sans_extension)
               deps
            ) in 

        begin match ml with 
          | Ml ml_file -> 
            let filename_sans_extension = Filename.chop_extension ml_file in 
            spit_out_ml `Ml ml_file filename_sans_extension
          | Re re_file -> 
            let filename_sans_extension = Filename.chop_extension re_file in 
            spit_out_ml `Re re_file filename_sans_extension

          | Ml_empty -> () end;
        let spit_out_mli (kind : [`Mli | `Rei ])  mli_file filename = 
          if kind = `Mli then 
            output_string oc (Printf.sprintf "build %s.mliast : build_ast %s\n" 
                                (build_output_prefix // filename)
                                ( mli_file))
          else
            output_string oc (Printf.sprintf "build %s.mliast : build_ast_from_reason_intf %s\n" 
                                (build_output_prefix // filename) 
                                ( mli_file));
          output_string oc (Printf.sprintf "build %s.mliast.d : build_deps %s.mliast\n"
                              (build_output_prefix // filename )
                              (build_output_prefix // filename));
          output_string oc (Printf.sprintf "build %s.cmi : build_cmi %s.mliast | %s.mliast.d\n"
                              (build_output_prefix // filename)
                              (build_output_prefix // filename)
                              (build_output_prefix // filename)
                           );
          all_cmis := (build_output_prefix // filename ^  Literals.suffix_cmi) :: !all_cmis ; 
          all_deps :=  build_output_prefix // (filename ^ Literals.suffix_mliastd) :: !all_deps in 

        begin match mli with 
          | Mli mli_file  -> 
            let filename = Filename.chop_extension mli_file in 
            spit_out_mli `Mli mli_file filename
          | Rei rei_file -> 
            let filename = Filename.chop_extension rei_file in 
            spit_out_mli `Rei rei_file filename
          | Mli_empty -> ()
        end;
        begin match mll with 
          | Some mll_file -> 
            (* if ml already exists then no need generate same *)
              let filename = Filename.chop_extension mll_file in
              if ml = Ml_empty then 
                spit_out_ml `Ml (filename ^ Literals.suffix_ml) filename;
              output_string oc (Printf.sprintf "build %s.ml : build_ml_from_mll %s.mll\n"
                                  (build_output_prefix // filename)
                                  filename);
              (* actually we would prefer generators in source ?
                 generator are divided into two categories:
                 1. not system dependent (ocamllex,ocamlyacc)
                 2. system dependent - has to be run on client's machine
              *)
          | None -> ()
        end
      );
    static_resources 
    |> List.iter (fun x -> 
        output_string oc 
          (Printf.sprintf "build %s : copy_resources %s\n"
             (build_output_prefix // x) x 
          );
        all_deps := (build_output_prefix // x) :: !all_deps
      )
    ;
    output_string oc  {|
rule reload
      command = ${bsbuild} -init
|};
    output_string oc (Printf.sprintf "build build.ninja : reload | bsconfig.json\n" );

    output_string oc (Printf.sprintf "build config : phony %s\n" 
                        (String.concat " "   !all_deps)) ;
    output_string oc (Printf.sprintf "build cmis : phony %s\n"
                        (String.concat " " !all_cmis)
                     );
    close_out oc;
  end

let config_file = "bsconfig.json"
let config_file_bak = "bsconfig.json.bak"
let write_ninja_file () = 
  let config_json_chan = open_in_bin config_file in 
  let global_data = Json_lexer.parse_json_from_chan config_json_chan  in
  let update_queue = ref [] in 
  let get_list_string s = 
    Ext_array.to_list_map (fun (x : Json_lexer.t) ->
        match x with 
        | `Str x -> Some x 
        | _ -> None
      ) s   in 
  let () = 
    match global_data with
    | `Obj map -> 
      map 
      |>
      Json_lexer.test "config"   (`Obj  begin fun m ->
          m
          |> Json_lexer.test "bsc"  (`Str (fun s -> Default.bsc := s))
          |> Json_lexer.test "bsbuild"  (`Str (fun s -> Default.bsbuild := s))
          |> Json_lexer.test "bsdep"  (`Str (fun s -> Default.bsdep := s))
          |> Json_lexer.test "package-name" (`Str (fun s -> Default.package_name := Some s))
          |> Json_lexer.test "ocamllex" (`Str (fun s -> Default.ocamllex :=  s))
          |> Json_lexer.test "external-includes" 
            (`Arr (fun s -> Default.bs_external_includes := get_list_string s))

          |> Json_lexer.test "bsc-flags" (`Arr (fun s -> Default.bsc_flags :=  get_list_string s ))

          |> Json_lexer.test "ppx-flags" (`Arr (fun s -> Default.ppx_flags := get_list_string s))
          |> Json_lexer.test "copy-or-symlink" (`Arr (fun s -> 
              Default.static_resources := get_list_string s))
          |> ignore
        end)
      |> 
      ignore
      ;
      map
      |> Json_lexer.test "file-groups" (`Arr (fun file_groups ->
          let  expect_file_group (x : Json_lexer.t String_map.t )  =
            let dir = ref Filename.current_dir_name in 
            let sources = ref String_map.empty in 
            let () = 
              x 
              |> Json_lexer.test "directory" 
                (`Str (fun s -> dir := s))
              |> Json_lexer.test Schemas.files 
                (`Arr_loc (fun s loc_start loc_end ->
                     let dir = !dir in 
                     if Array.length s  = 0 then 
                       begin 
                         let files_array = Sys.readdir dir  in 
                         let files, file_array =
                           Array.fold_left (fun (acc, f) name -> 
                               if Filename.check_suffix name ".ml" ||
                                  Filename.check_suffix name ".mll" ||
                                  Filename.check_suffix name ".mli" ||
                                  Filename.check_suffix name ".re" ||
                                  Filename.check_suffix name ".rei" then 
                                 (map_update ~dir acc name , name :: f)
                               else (acc,f)
                             ) (String_map.empty, []) files_array in 
                         update_queue :=
                           {Ext_file_pp.loc_start ;
                            loc_end; action = (`print (fun oc offset -> 
                               let indent = String.make offset ' ' in 
                               let p_str s = 
                                 output_string oc indent ; 
                                 output_string oc s ;
                                 output_string oc "\n"
                               in
                               match file_array with 
                               | []
                                 -> output_string oc "[ ]\n"
                               | first::rest 
                                 -> 
                                 output_string oc "[ \n";
                                 p_str ("\"" ^ first ^ "\"");
                                 List.iter 
                                   (fun f -> 
                                      p_str (", \"" ^f ^ "\"")
                                   ) rest;
                                 p_str "]" 
                                 (* we need add a new line in the end,
                                    otherwise it will be idented twice
                                 *)
                             ))} :: !update_queue;
                         sources := files
                       end
              
                     else 
                       sources := Array.fold_left (fun acc s ->
                           match s with 
                           | `Str s -> 
                             map_update ~dir acc s
                           | _ -> acc
                         ) String_map.empty s
                      ))
              |> ignore 
            in 
            {dir = !dir; sources = !sources} in 
          Default.bs_file_groups := Ext_array.to_list_map (fun x ->
              match x with 
              | `Obj map -> Some (expect_file_group map)
              | _ -> None
            ) file_groups

        ))
      |> ignore

    | _ -> ()
  in
  begin match List.sort Ext_file_pp.interval_compare  !update_queue with 
  | [] -> ()
  | queue -> 
    let file_size = in_channel_length config_json_chan in
    let oc = open_out_bin config_file_bak in
    let () = 
      Ext_file_pp.process_wholes
        queue file_size config_json_chan oc in 
    close_out oc ;
    close_in config_json_chan ; 
    Unix.unlink config_file; 
    Unix.rename config_file_bak config_file
  end;
  Default.(output_ninja 
             !bsc     
             !bsbuild
             !bsdep
             !package_name
             !ocamllex
             !build_output_prefix
             !bs_external_includes
             !static_resources 
             !bs_file_groups 
             !bsc_flags
             !ppx_flags )

let load_ninja = ref false

let ninja = "ninja" 
let load_ninja ninja_flags = 

  Unix.execvp ninja
    (Array.concat 
       [
         [|ninja ; "-d"; "keepdepfile"|];
         ninja_flags
       ]
    )
let call_ninja flags = 
  Sys.command 
    (String.concat " " (ninja :: "-d" :: "keepdepfile":: flags))

let bsninja_flags =
  [
    "-init", Arg.Unit write_ninja_file,
    " generate build.ninja";
  ]

let usage = {|Usage: bsbuild.exe <options> <files>
Options are:|}

let anonymous arg =
  raise (Arg.Bad ("don't know what to do with " ^ arg))

let () = 
  (* try  *)
    begin match Ext_array.rfind_and_split Sys.argv Ext_string.equal "-" with 
      | `No_split ->       
        begin 
          Arg.parse_argv Sys.argv  bsninja_flags anonymous usage;
          (* load_ninja [| "config" |] *)
          (* Here the failure will cause non-terminating, since 
             [bsbuild.exe -init] fail, it will retry again
          *)
        end
      | `Split (bsninja_argv, ninja_flags) 
        -> 
        Arg.parse_argv bsninja_argv bsninja_flags anonymous usage;
        let exit_flag = call_ninja ["config"]  in
        if exit_flag <> 0 then 
          begin
            exit exit_flag 
          end; 
        load_ninja ninja_flags
    end
  (* with x -> *)
  (*   begin *)
  (*     Location.report_exception Format.err_formatter x ; *)
  (*     exit 2 *)
  (*   end *)





