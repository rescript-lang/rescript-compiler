open GenTypeCommon

type t

val bsCurryPath : config:Config.t -> t
val chopExtensionSafe : t -> t [@@live]
val dump : t -> string
val emit : t -> string
val fromModule : dir:string -> importExtension:string -> ModuleName.t -> t
val fromStringUnsafe : string -> t
val toCmt : config:Config.t -> outputFileRelative:string -> t -> string
