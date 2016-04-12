external log  : 'a -> unit = "caml_ignore"  [@@bs.call "console.log"]
(** we should also allow js function call from an external js module 
    
*)

let v u = 
  log u;
  u
