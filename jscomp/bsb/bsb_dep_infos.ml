type dep_info = {
  dir_or_file : string ;
  stamp : float 
}

type t = dep_info array 

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

(** check time stamp for all files 
    TODO: those checks system call can be saved later
    Return a reason 
*)
let check file =
  try 
    let xs = read file  in 
    let rec aux i finish = 
      if i = finish then no_need_regenerate
      else 
        let k = Array.unsafe_get  xs i  in
        let current_file = k.dir_or_file in
        let stat = Unix.stat  current_file in 
        if stat.st_mtime <= k.stamp then 
          aux (i + 1 ) finish 
        else current_file
    in aux 0 (Array.length xs)  
  with _ -> file ^ " does not exist"
