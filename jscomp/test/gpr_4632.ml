[@@@config { flags = [| "-w"; "a" |] }]

module T0 = struct
  let myList = [ 1; 2 ]

  let (head :: tail) = myList
end

module T1 = struct
  let myList = [ (1, 2, 3) ]

  let (h0 :: h1 :: h2) = myList
end
