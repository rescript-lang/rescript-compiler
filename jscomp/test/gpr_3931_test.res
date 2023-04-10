module rec PA: {
  let print: array<int> => unit
} = {
  /* let () = P.print 3 */
  let print = {
    let iter = Array.iter(P.print)
    a => iter(a)
  }
}
and P: {
  let print: int => unit
} = {
  let print = i => print_endline(string_of_int(i))
}

let () = PA.print([1, 2])
