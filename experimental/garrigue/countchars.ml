let rec long_lines name n ic =
  let l = input_line ic in
  if String.length l > 80 then Printf.printf "%s: %d\n%!" name n;
  long_lines name (n+1) ic

let process_file name =
  try
    let ic = open_in name in
    try long_lines name 1 ic
    with End_of_file -> close_in ic
  with _ ->()

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    process_file Sys.argv.(i)
  done
