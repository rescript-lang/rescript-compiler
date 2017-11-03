(* the entry point. This is used by js_main.ml *)
let setup () =
  Super_location.setup ();
  Misc.Color.setup !Clflags.color;
  Super_typetexp.setup ();
  Super_typemod.setup ();
  Super_typecore.setup ();
  Misc.Color.setup !Clflags.color;
  Super_env.setup ();
  Super_pparse.setup ();
