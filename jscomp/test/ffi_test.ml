

external f : int -> int = "xx" [@@js.call ]


let u = f 3 
let v = Js.nil

let a, b ,c, d = Js.(true_, false_, nil, undef)

module Textarea = struct
  type t
  external create : unit -> t  = "TextArea" [@@ js.new ]
  (* TODO: *)
  (* external set_minHeight : t -> int -> unit = "minHeight" [@@js.set ]     *)
  (* external get_minHeight : t ->  int = "minHeight" [@@js.get] *)

end

(* let v = *)
(*   let u = Textarea.create () in  *)
(*    Textarea.set_minHeight u 3 ; *)
(*    Textarea.get_minHeight u  *)
