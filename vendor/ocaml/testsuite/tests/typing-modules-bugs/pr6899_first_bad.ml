let should_reject =
  let table = Hashtbl.create 1 in
  fun x y -> Hashtbl.add table x y
