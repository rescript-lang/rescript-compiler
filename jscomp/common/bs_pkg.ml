
let (//) = Filename.concat





let  resolve_bs_package  
    ~cwd
    name = 
  let marker = Literals.bsconfig_json in 
  let sub_path = name // marker  in
  let rec aux origin cwd name = 
    let abs_marker =  cwd // Literals.node_modules // sub_path in 
    if Sys.file_exists abs_marker then Some (Filename.dirname abs_marker)
    else 
      let cwd' = Filename.dirname cwd in 
      if String.length cwd' < String.length cwd then  
        aux origin   cwd' name
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
  aux cwd cwd name
