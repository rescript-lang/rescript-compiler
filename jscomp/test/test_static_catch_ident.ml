exception Scan_failure of string

let scanf_bad_input ib = function
  | Scan_failure s | Failure s ->
      for i = 0 to 100 do
        print_endline s ;
        (* necessary*)
        print_endline "don't inlinie"
      done
  | x -> raise x
