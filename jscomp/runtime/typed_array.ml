
type buffer




module Int32_array = struct
  type t 
  external buffer : t -> buffer = "buffer" [@@js.get]
  external get : t -> int -> int32  = "" [@@js.get_index]
  external set : t -> int -> int32 -> unit = "" [@@js.set_index]
  external create : int32 array -> t = "Int32Array" [@@js.new]
  external of_buffer : buffer -> t = "Int32Array" [@@js.new]
end 

module Float64_array = struct
  type t 
  external buffer : t -> buffer = "buffer" [@@js.get]
  external get : t -> int -> float  = "" [@@js.get_index]
  external set : t -> int -> float -> unit = "" [@@js.set_index]
  external create : float array -> t = "Float64Array" [@@js.new]
  external of_buffer : buffer -> t = "Float64Array" [@@js.new]
end 


(*
 it still return number, [float] in this case
*)
module Float32_array = struct
  type t 
  external buffer : t -> buffer = "buffer" [@@js.get]
  external get : t -> int -> float  = "" [@@js.get_index]
  external set : t -> int -> float -> unit = "" [@@js.set_index]
  external create : float array -> t = "Float32Array" [@@js.new]
  external of_buffer : buffer -> t = "Float32Array" [@@js.new]
end 
