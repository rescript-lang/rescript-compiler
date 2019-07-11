external log : 'a -> unit = "caml_ignore"
  [@@bs.val "console.log"]
(** we should also allow js function call from an external js module *)

let v u = log u ; u
