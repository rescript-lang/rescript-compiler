(**
  Since [others] depend on this file, its public mli files **should not
  export types** introduced here, otherwise it would cause 
  conflicts here.

  If the type exported here is also exported in modules from others,
  you will get a type not equivalent.

  
  Types defined here but should not export:
  - ref (make sure not exported in public others/*.mli)
*)

external (^) : string -> string -> string = "#string_append"
external ( = ) : 'a -> 'a -> bool = "%equal"
external ( <> ) : 'a -> 'a -> bool = "%notequal"
external ( == ) : 'a -> 'a -> bool = "%eq"
external ( != ) : 'a -> 'a -> bool = "%noteq"
external ( < ) : 'a -> 'a -> bool = "%lessthan"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"
external ( <= ) : 'a -> 'a -> bool = "%lessequal"
external ( >= ) : 'a -> 'a -> bool = "%greaterequal"
external ( + ) : int -> int -> int = "%addint"
external ( - ) : int -> int -> int = "%subint"
external ( ~- ) : int -> int = "%negint"
external ( * ) : int -> int -> int = "%mulint"
external ( / ) : int -> int -> int = "%divint"
external ( lsl ) : int -> int -> int = "%lslint"
external ( lor ) : int -> int -> int = "%orint"
external ( land ) : int -> int -> int = "%andint"
external ( mod ) : int -> int -> int = "%modint"

type 'a ref = { mutable contents : 'a }
external ref : 'a -> 'a ref = "%makemutable"
external ( ! ) : 'a ref -> 'a = "%field0"
external ( := ) : 'a ref -> 'a -> unit = "%setfield0"
external incr : int ref -> unit = "%incr"
external decr : int ref -> unit = "%decr"

external ( || ) : bool -> bool -> bool = "%sequor"
external ( && ) : bool -> bool -> bool = "%sequand"
external not : bool -> bool = "%boolnot"

external raise : exn -> 'a = "%raise"
external ignore : 'a -> unit = "%ignore"
external fst : 'a * 'b -> 'a = "%field0"
external snd : 'a * 'b -> 'b = "%field1"
external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"
external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"

external ( ** ) : float -> float -> float =  "pow" [@@bs.val] [@@bs.scope "Math"]
external ( ~-. ) : float -> float = "%negfloat"
external ( +. ) : float -> float -> float = "%addfloat"
external ( -. ) : float -> float -> float = "%subfloat"
external ( *. ) : float -> float -> float = "%mulfloat"
external ( /. ) : float -> float -> float = "%divfloat"

module Obj : sig   
  external magic : 'a -> 'b = "%identity"  
end 



module Pervasives : sig 
  external compare : 'a -> 'a -> int = "%compare"
  external not : bool -> bool = "%boolnot"
  external min : 'a -> 'a -> 'a = "%bs_min"
  external max : 'a -> 'a -> 'a = "%bs_max"
  external ( = ) : 'a -> 'a -> bool = "%equal"
end

module Int32 : sig 
  external to_int : int32 -> int = "%int32_to_int"
  external add : int32 -> int32 -> int32 = "%int32_add"
  external shift_left : int32 -> int -> int32 = "%int32_lsl"
  external shift_right_logical : int32 -> int -> int32 = "%int32_lsr"
  external shift_right : int32 -> int -> int32 = "%int32_asr"
  external logand : int32 -> int32 -> int32 = "%int32_and"
  external logxor : int32 -> int32 -> int32 = "%int32_xor"
  external logor : int32 -> int32 -> int32 = "%int32_or"
  external of_int : int -> int32 = "%int32_of_int"
  external mul : int32 -> int32 -> int32 = "%int32_mul"
end



module Int64 : sig 
  external of_int : int -> int64 = "%int64_of_int"
  external of_nativeint : nativeint -> int64 = "%int64_of_nativeint"
  external add : int64 -> int64 -> int64 = "%int64_add"
  external sub : int64 -> int64 -> int64 = "%int64_sub"
  external mul : int64 -> int64 -> int64 = "%int64_mul"
  external div : int64 -> int64 -> int64 = "%int64_div"
  external logor : int64 -> int64 -> int64 = "%int64_or"
  external neg : int64 -> int64 = "%int64_neg"
  external to_int : int64 -> int = "%int64_to_int"
end 





(* We should give it a name like FloatRT to avoid occasional shadowing *)
module FloatRT : sig 
  external _NaN : float = "NaN" [@@bs.val] 
  external isNaN : float -> bool = "" [@@bs.val]
  external isFinite : float -> bool = "" [@@bs.val]
  external toExponentialWithPrecision : float -> digits:int -> string = "toExponential" [@@bs.send]
  external toFixed : float -> string = "" [@@bs.send]
  external toFixedWithPrecision : float -> digits:int -> string = "toFixed" [@@bs.send]
  external fromString : string -> float = "Number" [@@bs.val]
end 

module UndefinedRT : sig 
  type + 'a t 
  external empty : 'a t = "#undefined" 
  external return : 'a -> 'a t = "%identity"
  external toOption : 'a t -> 'a option = "#undefined_to_opt"
end 