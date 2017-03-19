
let (//) = Filename.concat





let  resolve_bs_package  
    ~cwd
    name = 
  let marker = Literals.bsconfig_json in 
  let sub_path = name // marker  in
  let rec aux  cwd  = 
    let abs_marker =  cwd // Literals.node_modules // sub_path in 
    if Sys.file_exists abs_marker then Some (Filename.dirname abs_marker)
    else 
      let cwd' = Filename.dirname cwd in (* TODO: may non-terminating when see symlinks *)
      if String.length cwd' < String.length cwd then  
        aux    cwd' 
      else 
        try 
          let abs_marker = 
            Sys.getenv "npm_config_prefix" 
            // "lib" // Literals.node_modules // sub_path in
          if Sys.file_exists abs_marker
          then Some (Filename.dirname abs_marker)
          else None
            (* Bs_exception.error (Bs_package_not_found name) *)
        with 
          Not_found -> None
          (* Bs_exception.error (Bs_package_not_found name)           *)
  in
  aux cwd 


(** The package does not need to be a bspackage 
  example:
  {[
    resolve_npm_package_file ~cwd "reason/refmt"
  ]}
  It also returns the path name
*)
let resolve_npm_package_file ~cwd sub_path =
  let rec aux  cwd  = 
    let abs_marker =  cwd // Literals.node_modules // sub_path in 
    if Sys.file_exists abs_marker then Some abs_marker
    else 
      let cwd' = Filename.dirname cwd in 
      if String.length cwd' < String.length cwd then  
        aux cwd' 
      else 
        try 
          let abs_marker = 
            Sys.getenv "npm_config_prefix" 
            // "lib" // Literals.node_modules // sub_path in
          if Sys.file_exists abs_marker
          then Some  abs_marker
          else None
            (* Bs_exception.error (Bs_package_not_found name) *)
        with 
          Not_found -> None
          (* Bs_exception.error (Bs_package_not_found name)           *)
  in
  aux cwd 