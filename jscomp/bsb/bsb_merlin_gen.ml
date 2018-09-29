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


let merlin = ".merlin"
let merlin_header = "####{BSB GENERATED: NO EDIT"
let merlin_trailer = "####BSB GENERATED: NO EDIT}"
let merlin_trailer_length = String.length merlin_trailer
let (//) = Ext_path.combine

(** [new_content] should start end finish with newline *)
#if BS_NATIVE then
let revise_merlin nested merlin new_content =
#else
let revise_merlin merlin new_content =
#end
  if Sys.file_exists merlin then
    let merlin_chan = open_in_bin merlin in
    let size = in_channel_length merlin_chan in
    let s = really_input_string merlin_chan size in
    let () =  close_in merlin_chan in

    let header =  Ext_string.find s ~sub:merlin_header  in
    let tail = Ext_string.find s ~sub:merlin_trailer in
    if header < 0  && tail < 0 then (* locked region not added yet *)
      let ochan = open_out_bin merlin in
      output_string ochan s ;
      output_string ochan "\n";
      output_string ochan merlin_header;
#if BS_NATIVE then
      output_string ochan "\n# -backend ";
      output_string ochan nested;
#end
      Buffer.output_buffer ochan new_content;
      output_string ochan merlin_trailer ;
      output_string ochan "\n";
      close_out ochan
    else if header >=0 && tail >= 0  then
      (* there is one, hit it everytime,
         should be fixed point
      *)
      let ochan = open_out_bin merlin in
      output_string ochan (String.sub s 0 header) ;
      output_string ochan merlin_header;
#if BS_NATIVE then
      output_string ochan "\n# -backend ";
      output_string ochan nested;
#end
      Buffer.output_buffer ochan new_content;
      output_string ochan merlin_trailer ;
      output_string ochan (Ext_string.tail_from s (tail +  merlin_trailer_length));
      close_out ochan
    else failwith ("the .merlin is corrupted, locked region by bsb is not consistent ")
  else
    let ochan = open_out_bin merlin in
    output_string ochan merlin_header ;
    Buffer.output_buffer ochan new_content;
    output_string ochan merlin_trailer ;
    output_string ochan "\n";
    close_out ochan

(* ATTENTION: order matters here, need resolve global properties before
   merlin generation
*)
let merlin_flg_ppx = "\nFLG -ppx " 
let merlin_flg_pp = "\nFLG -pp "
let merlin_s = "\nS "
let merlin_b = "\nB "


#if BS_NATIVE then
let merlin_pkg = "\nPKG"
#end
let merlin_flg = "\nFLG "
let bs_flg_prefix = "-bs-"

let output_merlin_namespace buffer ns= 
  match ns with 
  | None -> ()
  | Some x -> 
    Buffer.add_string buffer merlin_b ; 
    Buffer.add_string buffer Bsb_config.lib_bs ; 
    Buffer.add_string buffer merlin_flg ; 
    Buffer.add_string buffer "-open ";
    Buffer.add_string buffer x 

#if BS_NATIVE then
let bsc_flg_to_merlin_ocamlc_flg ~backend bsc_flags  =
#else
let bsc_flg_to_merlin_ocamlc_flg bsc_flags  =
#end
  merlin_flg ^ 
  String.concat Ext_string.single_space 
    (List.filter (fun x -> not (Ext_string.starts_with x bs_flg_prefix )) @@ 
#if BS_NATIVE then
     if backend = Bsb_config_types.Js then Literals.dash_nostdlib::bsc_flags else bsc_flags)
#else
     Literals.dash_nostdlib::bsc_flags) 
#end 

(* No need for [-warn-error] in merlin  *)     
let warning_to_merlin_flg (warning: Bsb_warning.t option) : string=     
  merlin_flg ^ Bsb_warning.get_warning_flag warning


let merlin_file_gen ~cwd
#if BS_NATIVE then
    ~backend
#end
    built_in_ppx
    ({bs_file_groups = res_files ; 
      generate_merlin;
      ppx_flags;
      pp_flags ;
      bs_dependencies;
      bs_dev_dependencies;
      bsc_flags; 
      built_in_dependency;
      external_includes; 
      reason_react_jsx ; 
      namespace;
      package_name;
      warning; 
#if BS_NATIVE then
      ocamlfind_dependencies;
#end
     } : Bsb_config_types.t)
  =
  if generate_merlin then begin     
    let buffer = Buffer.create 1024 in
    output_merlin_namespace buffer namespace; 
    Ext_list.iter ppx_flags (fun x ->
        Buffer.add_string buffer (merlin_flg_ppx ^ x )
      );
    Ext_option.iter pp_flags (fun x -> 
      Buffer.add_string buffer (merlin_flg_pp ^ x)
    );  
    Ext_option.iter reason_react_jsx 
      (fun s -> 
         Buffer.add_string buffer (merlin_flg_ppx ^ s));
    Buffer.add_string buffer (merlin_flg_ppx  ^ built_in_ppx);
    (*
    (match external_includes with 
    | [] -> ()
    | _ -> 

      Buffer.add_string buffer (merlin_flg ^ Bsb_build_util.include_dirs external_includes
      ));
    *)
    Ext_list.iter external_includes (fun path -> 
        Buffer.add_string buffer merlin_s ;
        Buffer.add_string buffer path ;
        Buffer.add_string buffer merlin_b;
        Buffer.add_string buffer path ;
      );      
#if BS_NATIVE then
    let nested = match backend with
      | Bsb_config_types.Js       -> "js"
      | Bsb_config_types.Native   -> "native"
      | Bsb_config_types.Bytecode -> "bytecode" 
    in
#end
    Ext_option.iter built_in_dependency (fun package ->
       let path = package.package_install_path in 
#if BS_NATIVE then
       Buffer.add_string buffer ((merlin_s ^ path) // nested);
       Buffer.add_string buffer ((merlin_b ^ path) // nested)  
#else
       Buffer.add_string buffer (merlin_s ^ path );
       Buffer.add_string buffer (merlin_b ^ path)                      
#end
    );

#if BS_NATIVE then
    let bsc_string_flag = bsc_flg_to_merlin_ocamlc_flg ~backend bsc_flags in 
#else
    let bsc_string_flag = bsc_flg_to_merlin_ocamlc_flg bsc_flags in 
#end
    Buffer.add_string buffer bsc_string_flag ;
    Buffer.add_string buffer (warning_to_merlin_flg  warning); 
    Ext_list.iter bs_dependencies (fun package ->
#if BS_NATIVE then
        let path = package.Bsb_config_types.package_install_path // nested in
#else
        let path = package.Bsb_config_types.package_install_path in
#end
        Buffer.add_string buffer merlin_s ;
        Buffer.add_string buffer path ;
        Buffer.add_string buffer merlin_b;
        Buffer.add_string buffer path ;
      );
    Ext_list.iter bs_dev_dependencies (**TODO: shall we generate .merlin for dev packages ?*)
    (fun package ->    
#if BS_NATIVE then
        let path = package.Bsb_config_types.package_install_path // nested in
#else
        let path = package.Bsb_config_types.package_install_path in
#end
        Buffer.add_string buffer merlin_s ;
        Buffer.add_string buffer path ;
        Buffer.add_string buffer merlin_b;
        Buffer.add_string buffer path ;
      );
    Ext_list.iter res_files (fun (x : Bsb_file_groups.file_group) -> 
        if not (Bsb_file_groups.is_empty x) then 
          begin
            Buffer.add_string buffer merlin_s;
            Buffer.add_string buffer x.dir ;
            Buffer.add_string buffer merlin_b;
#if BS_NATIVE then
            Buffer.add_string buffer (Bsb_config.lib_bs // nested // x.dir) ;
#else
            Buffer.add_string buffer (Bsb_config.lib_bs//x.dir) ;
#end
          end
      ) ;
#if BS_NATIVE then
    if List.length ocamlfind_dependencies > 0 then begin
      Buffer.add_string buffer merlin_pkg;
      ocamlfind_dependencies |> List.iter (fun x ->
        Buffer.add_string buffer " ";
        Buffer.add_string buffer x;
      );
    end;
#end
    Buffer.add_string buffer "\n";
#if BS_NATIVE then
    revise_merlin nested (cwd // merlin) buffer 
#else
    revise_merlin (cwd // merlin) buffer 
#end
  end


