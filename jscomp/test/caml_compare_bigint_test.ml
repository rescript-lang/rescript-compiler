external big : string -> float = "BigInt" [@@bs.val]

let isLessThan title smaller bigger =
  [
    ("compare: " ^ title, fun _ -> Mt.Eq (true, compare bigger smaller > 0));
    ("compare: " ^ title, fun _ -> Mt.Eq (true, compare smaller bigger < 0));
    ("< operator: " ^ title, fun _ -> Mt.Eq (true, smaller < bigger));
    ("<= operator: " ^ title, fun _ -> Mt.Eq (true, smaller <= bigger));
    ("> operator: " ^ title, fun _ -> Mt.Eq (true, bigger > smaller));
    (">= operator: " ^ title, fun _ -> Mt.Eq (true, bigger >= smaller));
    ("min: " ^ title, fun _ -> Mt.Eq (smaller, min bigger smaller));
    ("min: " ^ title, fun _ -> Mt.Eq (smaller, min smaller bigger));
    ("max: " ^ title, fun _ -> Mt.Eq (bigger, max bigger smaller));
    ("max: " ^ title, fun _ -> Mt.Eq (bigger, max smaller bigger));
    ("!= operator: " ^ title, fun _ -> Mt.Eq (true, bigger != smaller));
    ("!= operator: " ^ title, fun _ -> Mt.Eq (true, smaller != bigger));
    ("== operator: " ^ title, fun _ -> Mt.Eq (false, bigger == smaller));
    ("== operator: " ^ title, fun _ -> Mt.Eq (false, smaller == bigger));
  ]

let isEqual title num1 num2 =
  [
    ("< operator: " ^ title, fun _ -> Mt.Eq (false, num2 < num1));
    ("<= operator: " ^ title, fun _ -> Mt.Eq (true, num2 <= num1));
    ("> operator: " ^ title, fun _ -> Mt.Eq (false, num1 > num2));
    (">= operator: " ^ title, fun _ -> Mt.Eq (true, num1 >= num2));
    ("min: " ^ title, fun _ -> Mt.Eq (num1, min num1 num2));
    ("max: " ^ title, fun _ -> Mt.Eq (num1, max num1 num2));
    ("compare: " ^ title, fun _ -> Mt.Eq (0, compare num1 num2));
    ("compare: " ^ title, fun _ -> Mt.Eq (0, compare num2 num1));
    ("!= operator: " ^ title, fun _ -> Mt.Eq (false, num1 != num2));
    ("!= operator: " ^ title, fun _ -> Mt.Eq (false, num2 != num1));
    ("== operator: " ^ title, fun _ -> Mt.Eq (true, num1 == num2));
    ("== operator: " ^ title, fun _ -> Mt.Eq (true, num2 == num1));
  ]

let fiveBigInt = big "5"

(** Not comparing floats and Bigint; not sure this is correct since works in JavaScript*)
let suites : Mt.pair_suites =
  isLessThan "123 and 555555" (big "123") (big "555555")
  @ isEqual "98765 and 98765" (big "98765") (big "98765")
  @ isEqual "same instance" fiveBigInt fiveBigInt

let () = Mt.from_pair_suites __FILE__ suites
