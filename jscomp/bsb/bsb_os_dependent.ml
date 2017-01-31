
(** Some deprecated code due to the fact that it does not work well on Windows 
*)
(* http://stackoverflow.com/questions/1510922/waiting-for-all-child-processes-before-parent-resumes-execution-unix*)
(*
let rec wait_all_children acc = 
  if acc = 0 then ()
  else 
    match Unix.wait () with 
    | pid, process_status -> 
      wait_all_children (acc - 1)
*)

(*
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
*)
(*
let run_command_execvp cmd =
  match Unix.fork () with 
  | 0 -> 
    print_endline ( "* Entering " ^ cmd.cwd);
    print_string "* " ; 
    for i = 0 to Array.length cmd.args - 1 do
      print_string cmd.args.(i);
      print_string Ext_string.single_space
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
*)

(*  
let () = 
  run_commands 
    (Array.init 5 (fun i -> {cmd = "sleep"; args = [|"sleep"; "4" |]; cwd = "."})
     |> Array.to_list)   
*)     

(*
let rec remove_dirs_recursive cwd roots = 
  Array.iter 
    (fun root -> 
       let cur = Filename.concat cwd root in 
       if Sys.is_directory cur then 
         begin       
           remove_dirs_recursive cur (Sys.readdir cur); 
           Unix.rmdir cur ; 
         end
       else 
         Sys.remove cur
    )
    roots        
*)