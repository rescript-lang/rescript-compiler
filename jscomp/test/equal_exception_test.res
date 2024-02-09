let v = "gso"

let is_equal = () => {
  assert (Bytes.get(Bytes.make(3, 'a'), 0) == 'a')
  assert (Bytes.unsafe_get(Bytes.make(3, 'a'), 0) == 'a')
  let u = Bytes.make(3, 'a')
  Bytes.unsafe_set(u, 0, 'b')
  assert (Bytes.unsafe_get(u, 0) == 'b')
  assert (String.get(v, 0) == 'g')
}

let is_exception = () =>
  try raise(Not_found) catch {
  | Not_found => ()
  }

let is_normal_exception = _x => {
  module E = {
    exception A(int)
  }
  let v = E.A(3)
  try raise(v) catch {
  | E.A(3) => ()
  }
}

let is_arbitrary_exception = () => {
  module E = {
    exception A
  }
  try raise(E.A) catch {
  | _ => ()
  }
}

let suites = list{
  ("is_equal", is_equal),
  ("is_exception", is_exception),
  ("is_normal_exception", is_normal_exception),
  ("is_arbitrary_exception", is_arbitrary_exception),
}

let e = Not_found
let eq = x =>
  switch x {
  | Not_found => true
  | _ => false
  }
exception Not_found
assert ((e == Not_found) == false)
assert (eq(Not_found) == false)

Mt.from_suites("exception", suites)
