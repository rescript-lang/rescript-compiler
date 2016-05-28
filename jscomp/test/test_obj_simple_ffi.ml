type t 
external mk_obj_spec : ?displayName:string -> test:int -> config:int  -> hi:string -> 
  unit ->  t =
  "caml_ignore" [@@bs.obj]
let v ?displayName () = mk_obj_spec ~test:3 ~config:3 ~hi:"ghos" ?displayName ()
let v2  = mk_obj_spec ~test:3 ~config:3 ~hi:"ghos"  ()
let v3  = mk_obj_spec ~test:3 ~config:3 ~hi:"ghos" ~displayName:"display" ()
