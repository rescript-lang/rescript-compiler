let f x =
  let v = ref x in
  let sum = ref 0 in
  while !v > 0 do
    sum := !sum + !v ;
    decr v
  done ;
  !sum
