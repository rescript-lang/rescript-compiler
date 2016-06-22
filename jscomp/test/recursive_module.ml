module  rec Int32 : sig
  type t
  type buffer
  external buffer : t -> buffer = "buffer" [@@js.get]
  external get : t -> int -> int  = "" [@@js.get_index]
  external set : t -> int -> int -> unit = "" [@@js.set_index]
  external create : int array -> t = "Int32Array" [@@js.new]
  external of_buffer : buffer -> t = "Int32Array" [@@js.new]
end = Int32


module  rec Int3 : sig
  val u : int -> int 
end = Int3


