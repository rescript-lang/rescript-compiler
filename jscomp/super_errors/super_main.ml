(* the entry point. This is used by js_main.ml *)
let setup () =
  Super_location.setup ();
  Super_typetexp.setup ();
  Super_typemod.setup ();
  Super_typecore.setup ();
