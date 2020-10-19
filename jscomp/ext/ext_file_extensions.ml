type valid_input = 
  | Ml 
  | Mli
  | Re
  | Rei
  | Res
  | Resi
  | Intf_ast
  | Impl_ast
  | Mlmap
  | Cmi
  | Unknown


(** This is per-file based, 
    when [ocamlc] [-c -o another_dir/xx.cmi] 
    it will return (another_dir/xx)
*)    

let classify_input ext = 

  match () with 
  | _ when ext = Literals.suffix_ml ->   
    Ml
  | _ when ext = Literals.suffix_re ->
    Re
  | _ when ext = !Config.interface_suffix ->
    Mli  
  | _ when ext = Literals.suffix_rei ->
    Rei
  | _ when ext =  Literals.suffix_ast ->
    Impl_ast 
  | _ when ext = Literals.suffix_iast ->
    Intf_ast
  | _ when ext =  Literals.suffix_mlmap ->
    Mlmap 
  | _ when ext =  Literals.suffix_cmi ->
    Cmi
  | _ when ext = Literals.suffix_res -> 
    Res
  | _ when ext = Literals.suffix_resi -> 
    Resi    
  | _ -> Unknown  