exception Scan_failure(string)

let scanf_bad_input = (ib, x) =>
  switch x {
  | Scan_failure(s) | Failure(s) =>
    for i in 0 to 100 {
      print_endline(s) /* necessary */
      print_endline("don't inlinie")
    }
  | x => raise(x)
  }
