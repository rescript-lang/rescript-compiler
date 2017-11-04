let (//) = Filename.concat

(** may nonterminate when [cwd] is '.' *)
let rec unsafe_root_dir_aux cwd  = 
  if Sys.file_exists (cwd//Literals.bsconfig_json) then cwd 
  else unsafe_root_dir_aux (Filename.dirname cwd)     

let project_root = unsafe_root_dir_aux (Sys.getcwd ())
let jscomp = project_root // "jscomp"
let bsc_bin = project_root // "lib" 

let bsc_exe = bsc_bin // "bsc.exe"
let runtime_dir = jscomp // "runtime"
let others_dir = jscomp // "others"
let stdlib_dir = jscomp // "stdlib"

let rec safe_dup fd =
  let new_fd = Unix.dup fd in
  if (Obj.magic new_fd : int) >= 3 then
    new_fd (* [dup] can not be 0, 1, 2*)
  else begin
    let res = safe_dup fd in
    Unix.close new_fd;
    res
  end

let safe_close fd =
  try Unix.close fd with Unix.Unix_error(_,_,_) -> ()


type output = {
  stderr : string ; 
  stdout : string ;
  exit_code : int 
}

let perform command args = 
  let new_fd_in, new_fd_out = Unix.pipe () in 
  let err_fd_in, err_fd_out = Unix.pipe () in 
  match Unix.fork () with 
  | 0 -> 
    begin try 
        safe_close new_fd_in;  
        safe_close err_fd_in;
        Unix.dup2 err_fd_out Unix.stderr ; 
        Unix.dup2 new_fd_out Unix.stdout; 
        Unix.execv command args 
      with _ -> 
        exit 127
    end
  | pid ->
    (* when all the descriptors on a pipe's input are closed and the pipe is 
        empty, a call to [read] on its output returns zero: end of file.
       when all the descriptiors on a pipe's output are closed, a call to 
       [write] on its input kills the writing process (EPIPE).
    *)
    safe_close new_fd_out ; 
    safe_close err_fd_out ; 
    let in_chan = Unix.in_channel_of_descr new_fd_in in 
    let err_in_chan = Unix.in_channel_of_descr err_fd_in in 
    let buf = Buffer.create 1024 in 
    let err_buf = Buffer.create 1024 in 
    (try 
       while true do 
         Buffer.add_string buf (input_line in_chan );             
         Buffer.add_char buf '\n'
       done;
     with
       End_of_file -> ()) ; 
    (try 
       while true do 
         Buffer.add_string err_buf (input_line err_in_chan );
         Buffer.add_char err_buf '\n'
       done;
     with
       End_of_file -> ()) ; 
    let exit_code = match snd @@ Unix.waitpid [] pid with 
      | Unix.WEXITED exit_code -> exit_code 
      | Unix.WSIGNALED _signal_number 
      | Unix.WSTOPPED _signal_number  -> 127 in 
    {
      stdout = Buffer.contents buf ; 
      stderr = Buffer.contents err_buf;
      exit_code 
    }


let perform_bsc args = 
  perform bsc_exe 
    (Array.append 
       [|bsc_exe ; 
         "-bs-package-name" ; "bs-platform"; 
         "-bs-no-version-header"; 
         "-bs-cross-module-opt";
         "-w";
         "-40";
         "-I" ;
         runtime_dir ; 
         "-I"; 
         others_dir ; 
         "-I" ; 
         stdlib_dir
       |] args)

let bsc_check_eval str = 
  perform_bsc [|"-bs-eval"; str|]        

  let debug_output o = 
  Printf.printf "\nexit_code:%d\nstdout:%s\nstderr:%s\n"
    o.exit_code o.stdout o.stderr
