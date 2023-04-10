let f0 = x =>
  (
    if x > 3 {
      x => x + 1
    } else {
      raise(Not_found)
    }
  )(3)

let f1 = x => (raise(Not_found): _ => _)(x)

let f3 = x =>
  (
    switch x {
    | 0 => x => x + 1
    | 1 => x => x + 2
    | 2 => x => x + 3
    | 3 => x => x + 4
    | _ => raise(Not_found)
    }
  )(3)
