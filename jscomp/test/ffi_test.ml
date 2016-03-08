

external f : int -> int = "xx" [@@js.call ]


let u () = f 3 
let v = Js.nil

let a, b ,c, d = Js.(true_, false_, nil, undef)

module Textarea = struct
  type t
  external create : unit -> t  = "TextArea" [@@ js.new ]
  (* TODO: *)
  external set_minHeight : t -> int -> unit = "minHeight" [@@js.set ]
  external get_minHeight : t ->  int = "minHeight" [@@js.get]
  external draw : t -> string  -> unit = "string" [@@js.send ]

end
module Int32Array = struct
  type t 
  external create : int -> t = "Int32Array" [@@js.new]
  external get : t -> int -> int = "" [@@js.get_index]
  external set : t -> int -> int -> unit = "" [@@js.set_index]
end

let v () =
  let u = Textarea.create () in
   Textarea.set_minHeight u 3 ;
   Textarea.get_minHeight u
   (* Textarea.set_minHeight_x *)


let f () = 
  let module Array = Int32Array in 
  let v  = Array.create 32 in 
  begin 
    v.(0) <- 3   ;
    v.(0)
  end
