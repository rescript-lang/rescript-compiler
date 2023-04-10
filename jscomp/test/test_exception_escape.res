module N: {
  let f: int
} = {
  exception A(int)
  let f = try raise(A(3)) catch {
  | _ => 3
  }
}
