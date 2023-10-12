open GenTypeCommon

type recordGen
type recordValue
type moduleItem
type moduleAccessPath = Root of string | Dot of moduleAccessPath * moduleItem

val checkMutableObjectField : previousName:string -> name:string -> bool
val default : string
val emitModuleAccessPath : config:Config.t -> moduleAccessPath -> string

val isMutableObjectField : string -> bool
val newModuleItem : name:string -> moduleItem
val newRecordValue : unboxed:bool -> recordGen -> recordValue
val recordGen : unit -> recordGen
val recordValueToString : recordValue -> string
val jsVariantTag : polymorphic:bool -> tag:string option -> string
val jsVariantPayloadTag : n:int -> string
val jsVariantValue : polymorphic:bool -> string
