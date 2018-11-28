(**
  Since [others] depend on this file, its public mli files **should not
  export types** introduced here, otherwise it would cause 
  conflicts here.

  If the type exported here is also exported in modules from others,
  you will get a type not equivalent.

  
  Types defined here but should not export:

  - ref (make sure not exported in public others/*.mli)
  - in_channel (runtime only)
  - fpclass (runtime only)
  - Obj.t (runtime only)
  - Lexing.lex_tables, Lexing.lexbuf (runtime only)
  - Parsing.parse_tables, Parsing.parser_env (runtime only)
  - Printexc.raw_backtrace_slot,  Printexc.backtrace_slot (runtiem only)
  - Gc.stat, Gc.control (runtime only) 
  - CamlinternalOO.obj CamlinternalOO.closure (runtime only)
  - CamlinternalMod.shape (runtime only)
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





module Bytes : sig
  external unsafe_get : bytes -> int -> char = "%bytes_unsafe_get"
  external unsafe_set : bytes -> int -> char -> unit = "%bytes_unsafe_set"
  external length : bytes -> int = "%bytes_length"
end

module Array : sig 
  external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
  external unsafe_set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"
  external length : 'a array -> int = "%array_length"
  external make : int -> 'a -> 'a array = "caml_make_vect"
end 

module String : sig 
  external unsafe_get : string -> int -> char = "%string_unsafe_get"
  external length : string -> int = "%string_length"
end 

module Char : sig 
  external code : char -> int = "%identity"
  external unsafe_chr : int -> char = "%identity"
end 

module Obj : sig 
  type t 
  external magic : 'a -> 'b = "%identity"
  external repr : 'a -> t = "%identity"
  external field : t -> int -> t = "%obj_field"
  external set_field : t -> int -> t -> unit = "%obj_set_field"
  external tag : t -> int = "caml_obj_tag"
  external dup : t -> t = "caml_obj_dup"
end 

module Nativeint : sig 
  external add : nativeint -> nativeint -> nativeint = "%nativeint_add"
  external div : nativeint -> nativeint -> nativeint = "%nativeint_div"
  external rem : nativeint -> nativeint -> nativeint = "%nativeint_mod"
  external logor : nativeint -> nativeint -> nativeint = "%nativeint_or"
  external shift_left : nativeint -> int -> nativeint = "%nativeint_lsl"
  external logand : nativeint -> nativeint -> nativeint = "%nativeint_and"
  external shift_right_logical : nativeint -> int -> nativeint = "%nativeint_lsr"
  external shift_right : nativeint -> int -> nativeint = "%nativeint_asr"
  external mul : nativeint -> nativeint -> nativeint = "%nativeint_mul"
  external logxor : nativeint -> nativeint -> nativeint = "%nativeint_xor"
  external to_float : nativeint -> float = "caml_nativeint_to_float"
  external of_float : float -> nativeint = "caml_nativeint_of_float"
  external to_int : nativeint -> int = "%nativeint_to_int"
  external to_int32 : nativeint -> int32 = "%nativeint_to_int32"
  external of_int : int -> nativeint = "%nativeint_of_int"
  external neg : nativeint -> nativeint = "%nativeint_neg"
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