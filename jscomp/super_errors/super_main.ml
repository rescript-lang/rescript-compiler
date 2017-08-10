(* the entry point. This is used by js_main.ml *)
let setup () =
  Super_misc.setup_reason_syntax_printing ();
  Super_location.setup ();
  Super_typetexp.setup ();
  Super_typecore.setup ();
  Super_typemod.setup ();
