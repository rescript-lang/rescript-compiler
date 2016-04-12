
type buffer




module Int32_array = struct
  type t 
  external buffer : t -> buffer = "buffer" [@@bs.get]
  external get : t -> int -> int32  = "" [@@bs.get_index]
  external set : t -> int -> int32 -> unit = "" [@@bs.set_index]
  external create : int32 array -> t = "Int32Array" [@@bs.new]
  external of_buffer : buffer -> t = "Int32Array" [@@bs.new]
end 

module Float64_array = struct
  type t 
  external buffer : t -> buffer = "buffer" [@@bs.get]
  external get : t -> int -> float  = "" [@@bs.get_index]
  external set : t -> int -> float -> unit = "" [@@bs.set_index]
  external create : float array -> t = "Float64Array" [@@bs.new]
  external of_buffer : buffer -> t = "Float64Array" [@@bs.new]
end 


(*
 it still return number, [float] in this case
*)
module Float32_array = struct
  type t 
  external buffer : t -> buffer = "buffer" [@@bs.get]
  external get : t -> int -> float  = "" [@@bs.get_index]
  external set : t -> int -> float -> unit = "" [@@bs.set_index]
  external create : float array -> t = "Float32Array" [@@bs.new]
  external of_buffer : buffer -> t = "Float32Array" [@@bs.new]
end 
