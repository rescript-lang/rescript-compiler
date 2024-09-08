@get external length: array<'a> => int = "length"

@get_index external get: (array<'a>, int) => 'a = ""

let get = (xs, index) =>
  if index < 0 || index >= length(xs) {
    raise(Invalid_argument("index out of bounds"))
  } else {
    xs->get(index)
  }

@set_index external set: (array<'a>, int, 'a) => unit = ""

let set = (xs, index, newval) =>
  if index < 0 || index >= length(xs) {
    raise(Invalid_argument("index out of bounds"))
  } else {
    xs->set(index, newval)
  }
