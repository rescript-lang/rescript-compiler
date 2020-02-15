(* Copyright (C) Authors of BuckleScript
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
let check () = 
  Ext_array.iter 
    Builtin_cmi_datasets.module_sets_cmi
    (fun (name,l) ->
(*     
       prerr_endline @@ String.escaped @@ External_ffi_types.to_string 
         (Ffi_bs ( [External_arg_spec.empty_kind Nothing; External_arg_spec.empty_kind Nothing],Return_identity,Js_get_index {js_get_index_scopes = []}));
       Ext_obj.pp_any Format.err_formatter  (Marshal.from_string "\132\149\166\190\000\000\000\r\000\000\000\006\000\000\000\017\000\000\000\017\176\160\160A\145@\160\004\003@A\153@" 0 : External_ffi_types.t);  
       Format.pp_print_newline Format.err_formatter ();
       Format.pp_print_flush Format.err_formatter ();
       Ext_obj.pp_any Format.err_formatter  (Marshal.from_string "\132\149\166\190\000\000\000\016\000\000\000\b\000\000\000\022\000\000\000\022\176\160\160A\145@\160\160A\004\003@A\153\144@" 0 : External_ffi_types.t);
       Format.pp_print_newline Format.err_formatter ();
       Format.pp_print_flush Format.err_formatter (); *)
       
       prerr_endline (">checking " ^ name);
    let cmi = Lazy.force l in 
    (match cmi.cmi_crcs with 
     | (unit , Some digest) :: _  ->
       Format.fprintf Format.err_formatter "%s -> %s@." unit (Digest.to_hex digest)
     | _ -> ());
     prerr_endline ("<checking " ^ name);
    );
  Ext_array.iter 
    Builtin_cmj_datasets.module_sets
    (fun (name,l) ->
       prerr_endline (">checking " ^ name);
       let cmj = Lazy.force l in 
       Format.fprintf Format.err_formatter "%b@." cmj.pure;
       prerr_endline ("<checking " ^ name);
    ) 