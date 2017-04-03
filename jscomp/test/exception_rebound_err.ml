exception A of int 
exception B 
exception C of int * int 

let test_js_error4 () = 
  try ignore @@ Js.Json.parse {| {"x"}|}; 1 with
  | Not_found -> 2
  | Invalid_argument "x" -> 3 
  | A 2 -> 4
  | B -> 5  
  | C (1,2) -> 6
  | e -> 7

let f g = 
  try g () with Not_found -> 1 