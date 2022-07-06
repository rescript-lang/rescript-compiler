open GenTypeCommon

type t

val bsCurryPath : config:config -> t

val dump : t -> string

val emit : t -> string

val fromModule : dir:string -> importExtension:string -> ModuleName.t -> t

val fromStringUnsafe : string -> t

val toCmt : config:config -> outputFileRelative:string -> t -> string
