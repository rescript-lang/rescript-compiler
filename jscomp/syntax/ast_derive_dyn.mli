(* *)
val core_type_of_type_declaration : Parsetree.type_declaration -> Parsetree.core_type
val mk_fun : 
  Parsetree.core_type ->
  string -> Parsetree.expression -> Parsetree.expression
