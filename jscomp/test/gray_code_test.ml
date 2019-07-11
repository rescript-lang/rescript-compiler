let gray_encode b = b lxor (b lsr 1)

let gray_decode n =
  let rec aux p n = if n = 0 then p else aux (p lxor n) (n lsr 1) in
  aux n (n lsr 1)

let bool_string len n =
  let s = Bytes.make len '0' in
  let rec aux i n =
    if n land 1 = 1 then Bytes.set s i '1' ;
    if i <= 0 then s else aux (pred i) (n lsr 1) in
  aux (pred len) n

let next_power v =
  let v = v - 1 in
  let v = (v lsr 1) lor v in
  let v = (v lsr 2) lor v in
  let v = (v lsr 4) lor v in
  let v = (v lsr 8) lor v in
  let v = (v lsr 16) lor v in
  v + 1
