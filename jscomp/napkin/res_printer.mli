val convertBsExternalAttribute : string -> string
val convertBsExtension : string -> string

val printTypeParams :
  (Parsetree.core_type * Asttypes.variance) list -> Res_comments_table.t -> Res_doc.t

val printLongident : Longident.t -> Res_doc.t

val printTypExpr : Parsetree.core_type -> Res_comments_table.t -> Res_doc.t

val addParens : Res_doc.t -> Res_doc.t

val printExpression : Parsetree.expression -> Res_comments_table.t -> Res_doc.t

val printStructure : Parsetree.structure -> Res_comments_table.t -> Res_doc.t [@@live]

val printImplementation :
  width:int -> Parsetree.structure -> comments:Res_comment.t list -> string
val printInterface :
  width:int -> Parsetree.signature -> comments:Res_comment.t list -> string
