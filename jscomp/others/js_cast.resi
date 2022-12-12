/***
  Safe cast between OCaml values which share the same
  runtime representation

  Different OCaml types might share the same represention in the
  ReScript runtime; while this is a compiler internal knowledge,
  applications might benefit from having a safe and zero cost
  conversion between those types.

  This modules acts as the **single place** for such conversion.

  If for any reason, the runtime representation changes, those function
  will be modified accordingly.
*/

/** `intOfBool b` returns `1` for when `b` is `true` and `0` when `b` is
    `false` */
external intOfBool: bool => int = "%identity"

/** `floatOfInt i` returns the float value of `i` */
external floatOfInt: int => float = "%identity"
