let link1 = "link1"
let link2 = "link2"

let link_exists s =
  try (Unix.lstat s).Unix.st_kind = Unix.S_LNK with _ -> false

let main () =
  close_out (open_out "test.txt");
  if link_exists link1 then Sys.remove link1;
  if link_exists link2 then Sys.remove link2;
  Unix.symlink ~to_dir:false ".\\test.txt" link1;
  assert ((Unix.stat link1).Unix.st_kind = Unix.S_REG);
  print_endline "Unix.symlink works with backwards slashes";
  Unix.symlink ~to_dir:false "./test.txt" link2;
  assert ((Unix.stat link2).Unix.st_kind = Unix.S_REG);
  print_endline "Unix.symlink works with forward slashes"

let () =
  Unix.handle_unix_error main ()
