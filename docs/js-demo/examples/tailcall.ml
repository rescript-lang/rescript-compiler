let tail_sum n =
  let rec aux acc i =
    if i <= n then
      aux (acc + i) (i + 1)
    else acc
  in aux 0 0


let () = Js.log (tail_sum 100) 