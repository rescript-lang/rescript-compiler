type t 
external mk_obj_spec : ?display_name:string -> test:int -> config:int  -> hi:string -> 
  unit ->  t =
  "caml_ignore" [@@js.obj]
let v ?display_name () = mk_obj_spec ~test:3 ~config:3 ~hi:"ghos" ?display_name ()
let v2  = mk_obj_spec ~test:3 ~config:3 ~hi:"ghos"  ()
let v3  = mk_obj_spec ~test:3 ~config:3 ~hi:"ghos" ~display_name:"display" ()
