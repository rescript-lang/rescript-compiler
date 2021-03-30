(* Copyright (C) 2019- Hongbo Zhang, Authors of ReScript
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



exception Pp_error

let cmd_nix_quote cmd sourcefile tmpfile : string = 
  Ext_filename.maybe_quote cmd ^ " --print=binary " ^
  Ext_filename.maybe_quote sourcefile ^
  " > " ^ Ext_filename.maybe_quote tmpfile 

let cmd_windows_quote cmd sourcefile tmpfile : string= 
  "cmd /S/C \"" ^
  cmd_nix_quote cmd sourcefile tmpfile
  ^ "\""


let clean tmpfile =   
  (if not !Clflags.verbose then try Sys.remove tmpfile with _ -> () )

(* Sync up with {!Pparse.preprocess} 
   The generated file should not sit 
   in the same directory as sourctree
*)
let pp (sourcefile : string) =    
  let tmpfile = Filename.temp_file "bspp" "" in
  let pp = (*TODO: check to avoid double quoting *)
    Filename.concat (Filename.dirname Sys.executable_name) "refmt.exe" in 
  let comm = 
    if Sys.win32 then cmd_windows_quote pp sourcefile tmpfile 
    else cmd_nix_quote pp sourcefile tmpfile
  in  
  if Ccomp.command comm <> 0 then begin
    clean tmpfile;
    raise Pp_error
  end;
  tmpfile  

