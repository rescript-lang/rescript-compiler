let console =
  try
    Unix.(openfile "/dev/tty" [O_RDWR] 0)
  with _ ->
    Unix.(openfile "CONIN$" [O_RDWR] 0)
in
Printf.printf "/dev/tty = %b\n" (Unix.isatty console)
