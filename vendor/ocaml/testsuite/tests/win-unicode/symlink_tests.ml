external to_utf16 : string -> string = "caml_to_utf16"
external create_file : string -> string -> unit = "caml_create_file"

let foodir = "UNIQU\xE4\xBD\xA0\xE5\xA5\xBD" (* "UNIQU你好" *)
let foofile = "UNIQU\xE4\xBD\xA0\xE5\xA5\xBD/\xE4\xBD\xA0\xE5\xA5\xBD.txt" (* "UNIQU你好/你好.txt" *)
let foofile2 = "UNIQU\xE4\xBD\xA0\xE5\xA5\xBD\\\xE4\xBD\xA0\xE5\xA5\xBD.txt" (* "UNIQU你好\\你好.txt" *)
let fileln = "\xE4\xBD\xA0\xE5\xA5\xBD-file-ln-s" (* "你好-file-ln-s" *)
let dirln = "\xE4\xBD\xA0\xE5\xA5\xBD-dir-ln-s" (* "你好-dir-ln-s" *)

open Unix

let () =
  mkdir foodir 0o777;
  create_file (to_utf16 foofile) foofile;
  symlink ~to_dir:true foodir dirln;
  symlink ~to_dir:false (if Sys.win32 then foofile2 else foofile) fileln; (* workaround MPR#7564 *)
  assert ((stat fileln).st_kind = S_REG);
  assert ((stat dirln).st_kind = S_DIR);
  assert ((lstat fileln).st_kind = S_LNK);
  assert ((lstat dirln).st_kind = S_LNK);
  Sys.remove foofile;
  Sys.remove fileln;
  rmdir dirln;
  rmdir foodir

let () =
  print_endline "OK."
