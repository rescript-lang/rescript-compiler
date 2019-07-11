let () =
  let r = ref 0 in
  for k = 1 to 10 do
    for i = 1 to 10 do
      let x, y = if i mod 2 = 0 then (1, i * 2) else (2, i * 3) in
      r := (!r * x) + y
    done
  done
