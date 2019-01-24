(* From Stephen Dolan *)
type (_,_) eq = Refl : ('a, 'a) eq;;
let cast (type a) (type b) (Refl : (a, b) eq) (x : a) = (x : b);;

let is_int (type a) =
  let rec (p : (int, a) eq) = match p with Refl -> Refl in
  p

let bang = print_string (cast (is_int : (int, string) eq) 42);;
