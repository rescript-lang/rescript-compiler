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

(* http://stackoverflow.com/questions/1510922/waiting-for-all-child-processes-before-parent-resumes-execution-unix*)

let rec wait_all_children acc = 
  if acc = 0 then ()
  else 
    match Unix.wait () with 
    | pid, process_status -> 
      wait_all_children (acc - 1)



let run_commands commands = 
  let rec aux acc commands = 
    match commands with
    | [ ] -> 
      begin 
        print_endline "Waiting for all children";
        wait_all_children acc;
        print_endline "All jobs finished"
      end
    | (cmd : command) :: rest -> 
      match Unix.fork () with 
      | 0 -> 
        Unix.chdir cmd.cwd;
        Unix.execvp cmd.cmd cmd.args
      | _pid -> 
        aux (acc + 1 )rest in 
  aux 0 commands    


let run_command_execvp cmd =
  match Unix.fork () with 
  | 0 -> 
    print_endline ( "* Entering " ^ cmd.cwd);
    print_string "* " ; 
    for i = 0 to Array.length cmd.args - 1 do
      print_string cmd.args.(i);
      print_string " "
    done;
    print_newline ();
    Unix.chdir cmd.cwd;
    Unix.execvp cmd.cmd cmd.args 
  | pid -> 
    match Unix.waitpid [] pid  with 
    | pid, process_status ->       
      match process_status with 
      | Unix.WEXITED eid ->
        if eid <> 0 then 
          begin 
            prerr_endline ("* Failure : " ^ cmd.cmd ^ "\n* Location: " ^ cmd.cwd);
            exit eid
          end
      | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> 
        begin 
          prerr_endline (cmd.cmd ^ " interrupted");
          exit 2 
        end

let run_command_execv fail_exit cmd =
  match Unix.fork () with 
  | 0 -> 
    print_endline ( "* Entering " ^ cmd.cwd);
    print_string "* " ; 
    for i = 0 to Array.length cmd.args - 1 do
      print_string cmd.args.(i);
      print_string " "
    done;
    print_newline ();
    Unix.chdir cmd.cwd;
    Unix.execv cmd.cmd cmd.args 
  | pid -> 
    match Unix.waitpid [] pid  with 
    | pid, process_status ->       
      match process_status with 
      | Unix.WEXITED eid ->
        if eid <> 0 then 
          begin 
            prerr_endline ("* Failure : " ^ cmd.cmd ^ "\n* Location: " ^ cmd.cwd);
            if fail_exit then exit eid    
          end;
        
      | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> 
        begin 
          prerr_endline (cmd.cmd ^ " interrupted");
          exit 2 
        end        
(*  
let () = 
  run_commands 
    (Array.init 5 (fun i -> {cmd = "sleep"; args = [|"sleep"; "4" |]; cwd = "."})
     |> Array.to_list)   
*)     