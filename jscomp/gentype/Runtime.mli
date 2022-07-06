open GenTypeCommon

type recordGen

type recordValue

type moduleItem

type moduleAccessPath = Root of string | Dot of moduleAccessPath * moduleItem

val accessVariant : index:int -> string -> string

val checkMutableObjectField : previousName:string -> name:string -> bool

val default : string

val emitModuleAccessPath : config:config -> moduleAccessPath -> string

val emitJSVariantGetLabel : polymorphic:bool -> string -> string

val emitJSVariantGetPayload : polymorphic:bool -> string -> string

val emitJSVariantWithPayload :
  label:string -> polymorphic:bool -> string -> string

val emitVariantGetLabel : polymorphic:bool -> string -> string

val emitVariantGetPayload :
  inlineRecord:bool -> numArgs:int -> polymorphic:bool -> string -> string

val emitVariantLabel : polymorphic:bool -> string -> string

val emitVariantWithPayload :
  inlineRecord:bool -> label:string -> polymorphic:bool -> string list -> string

val isMutableObjectField : string -> bool

val mangleObjectField : string -> string

val newModuleItem : name:string -> moduleItem

val newRecordValue : unboxed:bool -> recordGen -> recordValue

val recordGen : unit -> recordGen

val recordValueToString : recordValue -> string

val jsVariantTag : polymorphic:bool -> string

val jsVariantValue : polymorphic:bool -> string
