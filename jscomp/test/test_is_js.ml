external is_js : unit ->  bool  = "caml_is_js" 

let v = is_js ()
