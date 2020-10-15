type kind = Parenthesized | Braced of Location.t | Nothing

val expr: Parsetree.expression -> kind
val structureExpr: Parsetree.expression -> kind

val unaryExprOperand: Parsetree.expression -> kind

val binaryExprOperand: isLhs:bool -> Parsetree.expression -> kind
val subBinaryExprOperand: string -> string -> bool
val rhsBinaryExprOperand: string -> Parsetree.expression -> bool
val flattenOperandRhs: string -> Parsetree.expression -> bool

val lazyOrAssertExprRhs: Parsetree.expression -> kind

val fieldExpr: Parsetree.expression -> kind

val setFieldExprRhs: Parsetree.expression -> kind

val ternaryOperand: Parsetree.expression -> kind

val jsxPropExpr: Parsetree.expression -> kind
val jsxChildExpr: Parsetree.expression -> kind

val binaryExpr: Parsetree.expression -> kind
val modTypeFunctorReturn: Parsetree.module_type -> bool
val modTypeWithOperand: Parsetree.module_type -> bool
val modExprFunctorConstraint: Parsetree.module_type -> bool

val bracedExpr: Parsetree.expression -> bool
val callExpr: Parsetree.expression -> kind

val includeModExpr : Parsetree.module_expr -> bool

val arrowReturnTypExpr: Parsetree.core_type -> bool
