let () =
  (* Make sure it's no longer available on disk *)
  if Sys.file_exists "foo.cmi" then Sys.remove "foo.cmi";
  let old_loader = !Env.Persistent_signature.load in
  Env.Persistent_signature.load := (fun ~unit_name ->
    match unit_name with
    | "Foo" ->
      Some { Env.Persistent_signature.
             filename = Sys.executable_name
           ; cmi      = Marshal.from_string Cached_cmi.foo 0
           }
    | _ -> old_loader unit_name);
  Topmain.main ()
