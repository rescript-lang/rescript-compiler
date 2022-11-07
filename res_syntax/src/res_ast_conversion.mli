(* The purpose of this module is to convert a parsetree coming from the reason
 * or ocaml parser, into something consumable by the rescript printer. *)

(* Ocaml/Reason parser interprets string literals: i.e. escape sequences and unicode.
 * For printing purposes you want to preserve the original string.
 * Example: "😎" is interpreted as "\240\159\152\142"
 * The purpose of this routine is to place the original string back in
 * the parsetree for printing purposes. Unicode and escape sequences
 * shouldn't be mangled when *)
val replaceStringLiteralStructure :
  (string * Location.t) list -> Parsetree.structure -> Parsetree.structure
val replaceStringLiteralSignature :
  (string * Location.t) list -> Parsetree.signature -> Parsetree.signature

(* Get rid of the explicit/implicit arity attributes *)
val normalizeReasonArityStructure :
  forPrinter:bool -> Parsetree.structure -> Parsetree.structure
val normalizeReasonAritySignature :
  forPrinter:bool -> Parsetree.signature -> Parsetree.signature

(* transform parts of the parsetree into a suitable parsetree suitable
 * for printing. Example: convert reason ternaries into rescript ternaries *)
val structure : Parsetree.structure -> Parsetree.structure
val signature : Parsetree.signature -> Parsetree.signature
