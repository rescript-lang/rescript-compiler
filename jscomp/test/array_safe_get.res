let x = [1, 2]
let y = try x[3] catch {
| Invalid_argument(msg) =>
  print_endline(msg)
  0
}
