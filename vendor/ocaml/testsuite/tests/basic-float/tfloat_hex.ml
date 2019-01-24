let try_float_of_string str =
  try
    print_float (float_of_string str);
    print_newline ()
  with exn ->
    print_endline (Printexc.to_string exn)
;;

let () =
  try_float_of_string "0x1A";
  try_float_of_string "0x1Ap3";
  try_float_of_string "0x";
  try_float_of_string "0x.";
  try_float_of_string "0xp0";
  try_float_of_string "0x.p0";
