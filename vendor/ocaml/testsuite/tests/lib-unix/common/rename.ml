(* Test the Unix.rename function *)

let writefile filename contents =
  let oc = open_out_bin filename in
  output_string oc contents;
  close_out oc

let readfile filename =
  let ic = open_in_bin filename in
  let sz = in_channel_length ic in
  let contents = really_input_string ic sz in
  close_in ic;
  contents

let safe_remove filename =
  try Sys.remove filename with Sys_error _ -> ()

let testrename f1 f2 contents =
  try
    Unix.rename f1 f2;
    if readfile f2 <> contents then print_string "wrong contents!"
    else if Sys.file_exists f1 then print_string "initial file still exists!"
    else print_string "passed"
  with Unix.Unix_error(err, _, _) ->
    print_string "Unix_error exception: "; print_string (Unix.error_message err)

let testfailure f1 f2 =
  try
    Unix.rename f1 f2; print_string "should fail but doesn't!"
  with Unix.Unix_error _ ->
    print_string "fails as expected"

let _ =
  let f1 = "file1.dat" and f2 = "file2.dat" in
  safe_remove f1; safe_remove f2;
  print_string "Rename to nonexisting file: ";
  writefile f1 "abc";
  testrename f1 f2 "abc";
  print_newline();
  print_string "Rename to existing file: ";
  writefile f1 "def";
  writefile f2 "xyz";
  testrename f1 f2 "def";
  print_newline();
  print_string "Renaming a nonexisting file: ";
  testfailure f1 f2;
  print_newline();
  print_string "Renaming to a nonexisting directory: ";
  writefile f1 "abc";
  testfailure f1 (Filename.concat "nosuchdir" f2);
  print_newline();
  safe_remove f1; safe_remove f2
