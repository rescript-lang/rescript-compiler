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
    | head :: old_digest :: tail->

      Digest.equal digest old_digest

    | _ -> false
  else false   
let (+>) = Ext_buffer.add_string
let from_cmj (files : string list) (output_file : string) : unit = 
  let cmp = Ext_filename.module_name in     
  let files = List.sort (fun filea fileb  ->
      Ext_string_array.cmp (cmp filea) (cmp fileb)) files in 
  let buf = Ext_buffer.create 10000 in 

  let abs = 
    Ext_list.map files (fun file -> 
        let module_name = (cmp file) in 
        let c = 
          (let content = Ext_io.load_file file in 
           String.sub content Ext_cmj_magic.header_length (String.length content - Ext_cmj_magic.header_length)
          ) in 
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
      assert (cmi.cmi_flags = []);
      assert (cmi.cmi_name = module_name);
      let content = 
        Marshal.to_string 
          cmi
          (* cmi_name, crcs can be saved, but only a tiny bit *)
          (* (cmi.cmi_sign) *)
          (* (Array.of_list cmi.cmi_sign) *)
          (* ({ with 
             (* cmi_crcs = [] *)
             cmi_flags = []
             }) *)
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

let () = 
  let cmj_files = 
    ( 
       "runtime" // "js.cmj" ::
      get_files Literals.suffix_cmj stdlib @             
      get_files Literals.suffix_cmj "others") in 
  from_cmj cmj_files
    (Filename.concat "main" "builtin_cmj_datasets.ml");
  let cmi_files = 
    "runtime" // "js.cmi" ::       
    (get_files Literals.suffix_cmi stdlib @
    get_files Literals.suffix_cmi "others" )
    |> List.filter (fun x -> 
       x|~ "js_internalOO" ||
       x|~  "camlinternal" ||
       not (x |~ "internal"))
  in     
  from_cmi cmi_files
    (Filename.concat "main" "builtin_cmi_datasets.ml")

