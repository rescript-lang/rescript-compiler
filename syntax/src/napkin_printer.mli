
val printTypeParams :
  (Parsetree.core_type * Asttypes.variance) list -> Napkin_comments_table.t -> Napkin_doc.t

val printLongident : Longident.t -> Napkin_doc.t

val printTypExpr : Parsetree.core_type -> Napkin_comments_table.t -> Napkin_doc.t  

val addParens : Napkin_doc.t -> Napkin_doc.t

val printExpression : Parsetree.expression -> Napkin_comments_table.t -> Napkin_doc.t

val printStructure : Parsetree.structure -> Napkin_comments_table.t -> Napkin_doc.t [@@live]

val printImplementation :
  width:int -> Parsetree.structure -> Napkin_comment.t list -> unit
val printInterface :
  width:int -> Parsetree.signature -> Napkin_comment.t list -> unit
