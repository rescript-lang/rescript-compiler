

let print fmt summary =  
  let rec aux (s : Env.summary) = 
    match s with 
    | Env_empty -> ()
    | Env_value (s, id, des) 
      -> 
      Printtyp.value_description id fmt des ; aux s 
    | Env_type (s, id,des)
      -> aux s
      (* -> Printtyp.type_declaration id fmt des; aux s *)
    | Env_extension (s, id,des)
      -> (* Printtyp.extension_constructor id fmt des ;  *)
      aux s 
    | Env_module(s,id,des) -> aux s
    | Env_modtype(s,id,des) -> aux s
    | Env_class(s,id,des) -> aux s 
    | Env_cltype(s,id,des) -> aux s 
    | Env_open(s,id) -> aux s 
    | Env_functor_arg(s,id) -> aux s in
  aux summary
