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



type mode = 
  | Native
  | Playground of string list (* 3rd party libraries folders paths *)


let get_files ext dir = 
  Ext_array.filter_map (Sys.readdir dir)
    (fun  x ->
       if Ext_string.ends_with x  ext 
       then Some (Filename.concat dir x) else None )
  |> Array.to_list

(** the cache should be readable and also update *)
let check_digest output_file digest : bool = 
  if Sys.file_exists output_file then 
    match 
      List.filter (fun x -> x <> "")
        (String.split_on_char ' ' 
           ((Ext_io.load_file output_file))) with         
    | _head :: old_digest :: _tail->

      Digest.equal digest old_digest

    | _ -> false
  else false   
let (+>) = Ext_buffer.add_string
let from_cmj ~mode (files : string list) (output_file : string) : unit = 
  let cmp = Ext_filename.module_name in     
  let files = List.sort (fun filea fileb  ->
      Ext_string_array.cmp (cmp filea) (cmp fileb)) files in 
  let buf = Ext_buffer.create 10000 in 

  let abs = 
    Ext_list.map files (fun file -> 
        let module_name = (cmp file) in 
        let content : Js_cmj_format.t = Js_cmj_format.from_file file in 
        let () = match mode with
          | Native ->
            begin match content with 
              | {case = Little; package_spec}
                when package_spec = Js_packages_info.runtime_package_specs
                -> ()
              (*TODO: assert its suffixes*)
              | _ -> 
                Format.fprintf Format.err_formatter
                  "@[%s: @[%a@]@]@." file
                  Js_packages_info.dump_packages_info  content.package_spec;              
                assert false   
            end
          | Playground _ -> ()
        in
        (* prerr_endline (Ext_obj.dump content.package_spec); *)
        let c = 
          Marshal.to_string (content.values, content.pure) []
        in 
        Printf.sprintf {|%S (* %d *)|} module_name           
          (String.length c),
        Printf.sprintf {|(* %s *)%S|} module_name c   
      ) in 
  buf +>
  (Printf.sprintf {|let module_names : string array = Obj.magic (
%s
)
let module_data : string array = Obj.magic (
%s
)
|} (String.concat ",\n" (Ext_list.map abs fst)) (String.concat ",\n" (Ext_list.map abs snd))
  );
  buf +> "\n" ;
  let digest  = Digest.to_hex (Ext_buffer.digest buf) in  
  let same = check_digest output_file digest in     
  if not same then   
    let v = open_out_bin output_file in  
    Ext_pervasives.finally v ~clean:close_out (fun f ->   
        output_string f ("(* " ^ digest ^  " *) \n");
        Ext_buffer.output_buffer f buf     
      ) 




let from_cmi (files : string list) (output_file : string) = 
  let cmp = Ext_filename.module_name in   
  let files = List.sort (fun filea fileb  ->
      Ext_string_array.cmp (cmp filea) (cmp fileb)) files in 

  let buf = Ext_buffer.create 10000 in 
  let abs =  Ext_list.map files (fun file -> 
      let module_name = cmp file in 
      let cmi = Cmi_format.read_cmi file in 
      assert (cmi.cmi_name = module_name);
      let content = 
        Marshal.to_string 
          cmi
          [] in 
      Printf.sprintf {|%S (* %d *)|} module_name (String.length content) ,                              
      Printf.sprintf {|(* %s *) %S|} module_name content) in 

  buf +> 
  (Printf.sprintf {|let module_names : string array = Obj.magic (
%s
)
let module_data : string array = Obj.magic (
%s
)
  |}
     (String.concat ",\n" (Ext_list.map abs fst )) (String.concat ",\n" (Ext_list.map abs snd))
  ) ;
  buf +> "\n";
  let digest = Digest.to_hex (Ext_buffer.digest buf) in             
  let same = check_digest output_file digest in 
  if not same then
    let v = open_out_bin output_file in
    Ext_pervasives.finally v ~clean:close_out (fun f ->         
        output_string f ("(* " ^ digest ^ " *)\n");
        Ext_buffer.output_buffer f buf 
      )

;;

let stdlib = "stdlib-406"
let (//) = Filename.concat 
let (|~) = Ext_string.contain_substring

let cmi_target_file = (Filename.dirname Sys.argv.(0) // ".." // "main" // "builtin_cmi_datasets.ml")
let release_cmi = Array.exists ((=) "-release") Sys.argv
let () = 
  if release_cmi then begin  
    print_endline "collecting cmi from ../lib/ocaml in release mode" ;
    try Sys.remove cmi_target_file with _ -> 
      Format.fprintf Format.err_formatter "failed to remove %s@." cmi_target_file
  end 
let mode = 
  match Sys.argv with
  | [|_; "-playground"; folders |] 
    -> 
    Playground (folders
                |> String.split_on_char ','
                |> List.filter (fun s -> s <> ""))
  | _ -> Native
let () = 
  let third_party_cmj_files = match mode with
    | Native -> []
    | Playground folders -> List.fold_left (fun acc folder -> acc @ get_files Literals.suffix_cmj folder) [] folders
  in
  let cmj_files = 
    ( 
      (Filename.dirname Sys.argv.(0) // ".." // "runtime" // "js.cmj") ::
      get_files Literals.suffix_cmj (Filename.dirname Sys.argv.(0) // ".." // stdlib) @             
      get_files Literals.suffix_cmj (Filename.dirname Sys.argv.(0) // ".." // "others") @
      third_party_cmj_files) in 
  from_cmj ~mode cmj_files
    (Filename.dirname Sys.argv.(0) // ".." // "main" // "builtin_cmj_datasets.ml");
  let third_party_cmi_files = match mode with
    | Native -> []
    | Playground folders -> List.fold_left (fun acc folder -> acc @ get_files Literals.suffix_cmi folder) [] folders
  in
  let cmi_files = 
    if release_cmi then   
      get_files Literals.suffix_cmi (".."//"lib"//"ocaml")  
    else    
      (Filename.dirname Sys.argv.(0) // ".." // "runtime" // "js.cmi") ::       
      (get_files Literals.suffix_cmi (Filename.dirname Sys.argv.(0) // ".." // stdlib) @
       get_files Literals.suffix_cmi (Filename.dirname Sys.argv.(0) // ".." // "others") @
       third_party_cmi_files)
      |> List.filter (fun x -> 
          x|~ "js_OO" ||
          x|~  "camlinternal" ||
          not (x |~ "internal")) 
  in     
  from_cmi 
    cmi_files
    cmi_target_file

