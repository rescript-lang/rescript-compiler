type dep_info = {
  dir_or_file : string ;
  stamp : float 
}

type t = 
  { file_stamps : dep_info array ; 
    source_directory :  string
  }


let magic_number = "BS_DEP_INFOS_20161022"


let write (fname : string)  (x : t) = 
  let oc = open_out_bin fname in 
  output_string oc magic_number ;
  output_value oc x ; 
  close_out oc 

let read (fname : string) : t = 
  let ic = open_in_bin fname in  (* Windows binary mode*)
  let buffer = really_input_string ic (String.length magic_number) in
  assert (buffer = magic_number);
  let res : t = input_value ic  in 
  close_in ic ; 
  res



let no_need_regenerate = ""


let rec check_aux xs i finish = 
  if i = finish then no_need_regenerate
  else 
    let k = Array.unsafe_get  xs i  in
    let current_file = k.dir_or_file in
    let stat = Unix.stat  current_file in 
    if stat.st_mtime <= k.stamp then 
      check_aux xs (i + 1 ) finish 
    else current_file

(** check time stamp for all files 
    TODO: those checks system call can be saved later
    Return a reason 
*)
let check ~cwd file =
  try 
    let {file_stamps = xs; source_directory} = read file  in 
    if cwd <> source_directory then 
      source_directory ^ " -> " ^ cwd
    else check_aux xs  0 (Array.length xs)  
  with _ -> file ^ " does not exist"
