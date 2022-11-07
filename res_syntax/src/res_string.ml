let hexTable =
  [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; |]
  [@ocamlformat "disable"]

let convertDecimalToHex ~strDecimal =
  try
    let intNum = int_of_string strDecimal in
    let c1 = Array.get hexTable (intNum lsr 4) in
    let c2 = Array.get hexTable (intNum land 15) in
    "x" ^ String.concat "" [String.make 1 c1; String.make 1 c2]
  with Invalid_argument _ | Failure _ -> strDecimal
