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



type command = 
  { 
    cmd : string ;
    cwd : string ; 
    args : string array 
  }  


let log cmd = 
    Format.fprintf Format.std_formatter "@{<info>Entering@} %s @." cmd.cwd ;  
    Format.fprintf Format.std_formatter "@{<info>Cmd:@} " ; 
    for i = 0 to Array.length cmd.args - 1 do
      Format.print_string cmd.args.(i);
      Format.print_string Ext_string.single_space
    done;
    Format.print_newline ()
let fail cmd =
  Format.fprintf Format.err_formatter "@{<error>Failure:@} %s \n Location: %s@." cmd.cmd cmd.cwd
let run_command_execv_unix  cmd =
  match Unix.fork () with 
  | 0 -> 
    log cmd;
    Unix.chdir cmd.cwd;
    Unix.execv cmd.cmd cmd.args 
  | pid -> 
    match Unix.waitpid [] pid  with 
    | pid, process_status ->       
      match process_status with 
      | Unix.WEXITED eid ->
        if eid <> 0 then 
          begin 
            fail cmd;
            exit eid    
          end;
      | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> 
        begin 
          Format.fprintf Format.err_formatter "@{<error>Interrupted:@} %s@." cmd.cmd;
          exit 2 
        end        


(** TODO: the args are not quoted, here 
  we are calling a very limited set of `bsb` commands, so that 
  we are safe
*)
let run_command_execv_win (cmd : command) =
  let old_cwd = Unix.getcwd () in 
  log cmd;
  Unix.chdir cmd.cwd;
  let eid =
    Sys.command 
      (String.concat Ext_string.single_space 
                           ( Filename.quote cmd.cmd ::( List.tl  @@ Array.to_list cmd.args))) in 
  if eid <> 0 then 
    begin 
      fail cmd;
      exit eid    
    end
  else  begin 
    print_endline ("* Leaving " ^ cmd.cwd ^ " into " ^ old_cwd );
    Unix.chdir old_cwd
  end


let run_command_execv = 
    if Ext_sys.is_windows_or_cygwin then 
      run_command_execv_win
    else run_command_execv_unix  
(** it assume you have permissions, so always catch it to fail 
    gracefully
*)

let rec remove_dir_recursive dir = 
  if Sys.is_directory dir then 
    begin 
      let files = Sys.readdir dir in 
      for i = 0 to Array.length files - 1 do 
        remove_dir_recursive (Filename.concat dir (Array.unsafe_get files i))
      done ;
      Unix.rmdir dir 
    end
  else Sys.remove dir 

let run_command_capture_stdout cmd =
  let ic, oc = Unix.open_process cmd in
  let buf = Buffer.create 64 in
  (try
     while true do
       Buffer.add_channel buf ic 1
     done
   with End_of_file -> ());
  let _ = Unix.close_process (ic, oc) in
  Buffer.contents buf
