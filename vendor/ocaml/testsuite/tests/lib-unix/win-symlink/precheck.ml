let () =
  exit (if Sys.win32 && Unix.has_symlink () then 0 else 1)
