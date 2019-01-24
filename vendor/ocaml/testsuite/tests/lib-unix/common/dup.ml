let _ =
  let f = Unix.dup ~cloexec:true Unix.stdout in
  let txt = "Some output\n" in
  ignore (Unix.write_substring f txt 0 (String.length txt));
  Unix.close f
