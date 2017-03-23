
let (//) = Filename.concat




(** It makes sense to have this function raise, when [bsb] could not resolve a package, it used to mean
    a failure 
*)
let  resolve_bs_package  
    ~cwd
    name = 
  let marker = Literals.bsconfig_json in 
  let sub_path = name // marker  in
  let rec aux  cwd  = 
    let abs_marker =  cwd // Literals.node_modules // sub_path in 
    if Sys.file_exists abs_marker then (* Some *) (Filename.dirname abs_marker)
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
          then 
            Filename.dirname abs_marker
          else
            begin 
              Format.fprintf Format.err_formatter 
                "@{<error>Package not found: resolving package %s in %s  @}@." name cwd ;             
              Bsb_exception.error (Package_not_found (name, None))
            end
        with 
          Not_found -> 
          begin 
            Format.fprintf Format.err_formatter 
              "@{<error>Package not found: resolving package %s in %s  @}@." name cwd ;             
            Bsb_exception.error (Package_not_found (name,None))
          end
  in
  aux cwd 


let cache = String_hashtbl.create 0 

(** TODO: collect all warnings and print later *)
let resolve_bs_package ~cwd package = 
  match String_hashtbl.find_opt cache package with 
  | None -> 
    let result = resolve_bs_package ~cwd package in 
    Format.fprintf Format.std_formatter "@{<info>Package@} %s -> %s@." package result ; 
    String_hashtbl.add cache package result ;
    result 
  | Some x 
    -> 
    let result = resolve_bs_package ~cwd package in 
    if result <> x then 
      begin 
        Format.fprintf Format.err_formatter  
          "@{<warning>Duplicated package:@} %s %s (chosen) vs %s in %s @." package x result cwd;
      end;
    x

(** The package does not need to be a bspackage 
  example:
  {[
    resolve_npm_package_file ~cwd "reason/refmt";;
    resolve_npm_package_file ~cwd "reason/refmt/xx/yy"
  ]}
  It also returns the path name
  Note the input [sub_path] is already converted to physical meaning path according to OS
*)
(* let resolve_npm_package_file ~cwd sub_path = *)
(*   let rec aux  cwd  =  *)
(*     let abs_marker =  cwd // Literals.node_modules // sub_path in  *)
(*     if Sys.file_exists abs_marker then Some abs_marker *)
(*     else  *)
(*       let cwd' = Filename.dirname cwd in  *)
(*       if String.length cwd' < String.length cwd then   *)
(*         aux cwd'  *)
(*       else  *)
(*         try  *)
(*           let abs_marker =  *)
(*             Sys.getenv "npm_config_prefix"  *)
(*             // "lib" // Literals.node_modules // sub_path in *)
(*           if Sys.file_exists abs_marker *)
(*           then Some  abs_marker *)
(*           else None *)
(*             (\* Bs_exception.error (Bs_package_not_found name) *\) *)
(*         with  *)
(*           Not_found -> None *)
(*           (\* Bs_exception.error (Bs_package_not_found name)           *\) *)
(*   in *)
(*   aux cwd  *)
