let actionKey key a b c d e =
  match key with
  | 'v' | 'c' -> a
  | 't' -> b
  | 'b' -> c
  | 'j' -> d
  | 'k' -> e
  | _ -> fun _ -> ()
