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


