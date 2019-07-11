(** [%js{ text : 32 ; label : "hel" }] Attention: also ok for nested case *)

let mk () =
  let module N = struct
    external mk : text:'b -> label:'a -> < text: 'b ; label: 'a > = ""
      [@@bs.obj]
  end in
  N.mk ~text:32 ~label:"hel"
