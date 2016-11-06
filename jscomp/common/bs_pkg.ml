
let (//) = Filename.concat



let  resolve_bs_package  
    ?(subdir="")
    ~cwd
    name = 
  let sub_path = name // subdir  in
  let rec aux origin cwd name = 
    let destdir =  cwd // Literals.node_modules // sub_path in 
    if Ext_sys.is_directory_no_exn destdir then Some destdir
    else 
      let cwd' = Filename.dirname cwd in 
      if String.length cwd' < String.length cwd then  
        aux origin   cwd' name
      else 
        try 
          let destdir = 
            Sys.getenv "npm_config_prefix" 
            // "lib" // Literals.node_modules // sub_path in
          if Ext_sys.is_directory_no_exn destdir
          then Some destdir
          else None
            (* Bs_exception.error (Bs_package_not_found name) *)
        with 
          Not_found -> None
          (* Bs_exception.error (Bs_package_not_found name)           *)
  in
  aux cwd cwd name
