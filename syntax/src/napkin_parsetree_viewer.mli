(* Restructures a nested tree of arrow types into its args & returnType
   * The parsetree contains: a => b => c => d, for printing purposes
   * we restructure the tree into (a, b, c) and its returnType d *)
   val arrowType: Parsetree.core_type ->
   Parsetree.attributes *
   (Parsetree.attributes * Asttypes.arg_label * Parsetree.core_type) list *
   Parsetree.core_type

val functorType: Parsetree.module_type ->
 (Parsetree.attributes * string Asttypes.loc * Parsetree.module_type option) list *
 Parsetree.module_type

(* filters @bs out of the provided attributes *)
val processUncurriedAttribute: Parsetree.attributes -> bool * Parsetree.attributes

type ifConditionKind =
  | If of Parsetree.expression
  | IfLet of Parsetree.pattern * Parsetree.expression

(* if ... else if ... else ... is represented as nested expressions: if ... else { if ... }
* The purpose of this function is to flatten nested ifs into one sequence.
* Basically compute: ([if, else if, else if, else if], else) *)
val collectIfExpressions:
 Parsetree.expression ->
   (ifConditionKind * Parsetree.expression) list * Parsetree.expression option

val collectListExpressions:
 Parsetree.expression -> (Parsetree.expression list * Parsetree.expression option)

type funParamKind =
 | Parameter of {
     attrs: Parsetree.attributes;
     lbl: Asttypes.arg_label;
     defaultExpr: Parsetree.expression option;
     pat: Parsetree.pattern;
   }
 | NewTypes of {attrs: Parsetree.attributes; locs: string Asttypes.loc list}

val funExpr:
 Parsetree.expression ->
   Parsetree.attributes *
   funParamKind list *
   Parsetree.expression

(* example:
*  `makeCoordinate({
*    x: 1,
*    y: 2,
*  })`
*  Notice howe `({` and `})` "hug" or stick to each other *)
val isHuggableExpression: Parsetree.expression -> bool

val isHuggablePattern: Parsetree.pattern -> bool

val isHuggableRhs: Parsetree.expression -> bool

val operatorPrecedence: string -> int

val isUnaryExpression: Parsetree.expression -> bool
val isBinaryOperator: string -> bool
val isBinaryExpression: Parsetree.expression -> bool

val flattenableOperators: string -> string -> bool

val hasAttributes: Parsetree.attributes -> bool

val isArrayAccess: Parsetree.expression -> bool
val isTernaryExpr: Parsetree.expression -> bool
val isIfLetExpr: Parsetree.expression -> bool

val collectTernaryParts: Parsetree.expression -> ((Parsetree.expression * Parsetree.expression) list * Parsetree.expression)

val parametersShouldHug:
 funParamKind list -> bool

val filterTernaryAttributes: Parsetree.attributes -> Parsetree.attributes

val isJsxExpression: Parsetree.expression -> bool
val hasJsxAttribute: Parsetree.attributes -> bool

val shouldIndentBinaryExpr: Parsetree.expression -> bool
val shouldInlineRhsBinaryExpr: Parsetree.expression -> bool
val filterPrinteableAttributes: Parsetree.attributes -> Parsetree.attributes
val partitionPrinteableAttributes: Parsetree.attributes -> (Parsetree.attributes * Parsetree.attributes)

val requiresSpecialCallbackPrintingLastArg: (Asttypes.arg_label * Parsetree.expression) list -> bool
val requiresSpecialCallbackPrintingFirstArg: (Asttypes.arg_label * Parsetree.expression) list -> bool

val modExprApply : Parsetree.module_expr -> (
 Parsetree.module_expr list * Parsetree.module_expr
)

(* Collection of utilities to view the ast in a more a convenient form,
 * allowing for easier processing.
 * Example: given a ptyp_arrow type, what are its arguments and what is the
 * returnType? *)


val modExprFunctor : Parsetree.module_expr -> (
 (Parsetree.attributes * string Asttypes.loc * Parsetree.module_type option) list *
 Parsetree.module_expr
)

val splitGenTypeAttr : Parsetree.attributes -> (bool * Parsetree.attributes)

val collectPatternsFromListConstruct:
 Parsetree.pattern list -> Parsetree.pattern ->
   (Parsetree.pattern list * Parsetree.pattern)

val isBlockExpr : Parsetree.expression -> bool

val isTemplateLiteral: Parsetree.expression -> bool

val collectOrPatternChain:
 Parsetree.pattern -> Parsetree.pattern list

val processBracesAttr : Parsetree.expression -> (Parsetree.attribute option * Parsetree.expression)

val filterParsingAttrs : Parsetree.attributes -> Parsetree.attributes

val isBracedExpr : Parsetree.expression -> bool

val isPipeExpr : Parsetree.expression -> bool

val extractValueDescriptionFromModExpr: Parsetree.module_expr -> Parsetree.value_description list

type jsImportScope =
 | JsGlobalImport (* nothing *)
 | JsModuleImport of string (* from "path" *)
 | JsScopedImport of string list (* window.location *)

val classifyJsImport: Parsetree.value_description -> jsImportScope

(* (__x) => f(a, __x, c) -----> f(a, _, c)  *)
val rewriteUnderscoreApply: Parsetree.expression -> Parsetree.expression

(* (__x) => f(a, __x, c) -----> f(a, _, c)  *)
val isUnderscoreApplySugar: Parsetree.expression -> bool
