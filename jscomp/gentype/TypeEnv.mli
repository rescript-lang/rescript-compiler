open GenTypeCommon

type t

val addModuleEquation : dep:dep -> internal:bool -> t -> unit
val addModulePath : typeEnv:t -> string -> ResolvedName.t
val addTypeEquations : typeEquations:(Longident.t * type_) list -> t -> t
val applyTypeEquations : config:Config.t -> path:Path.t -> t -> type_ option
val expandAliasToExternalModule : name:string -> t -> dep option
val getModuleEquations : t -> ResolvedName.eq list
val getModuleAccessPath : name:string -> t -> Runtime.moduleAccessPath
val getModule : name:string -> t -> t option
val lookup : name:string -> t -> t option

val lookupModuleTypeSignature :
  path:Path.t -> t -> (Typedtree.signature * t) option

val newModule : name:string -> t -> t
val newModuleType : name:string -> signature:Typedtree.signature -> t -> t
val newType : name:string -> t -> unit
val root : unit -> t
val toString : t -> string
val updateModuleItem : moduleItem:Runtime.moduleItem -> t -> unit
