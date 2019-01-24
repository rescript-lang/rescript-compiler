let () = try raise (Invalid_argument "X") with Invalid_argument s ->
  raise (Invalid_argument (s ^ s))
