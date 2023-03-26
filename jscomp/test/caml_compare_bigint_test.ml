external fromString : string -> float = "BigInt" [@@bs.val]

let sml = fromString "123"
let big = fromString "3944949939394"

let sameTypes a b = Js.typeof a == Js.typeof b

let assertLessThan title smaller bigger =
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
  ]
  @ ([
       ("!= operator: " ^ title, fun _ -> Mt.Eq (true, bigger != smaller));
       ("!= operator: " ^ title, fun _ -> Mt.Eq (true, smaller != bigger));
       ("== operator: " ^ title, fun _ -> Mt.Eq (false, bigger == smaller));
       ("== operator: " ^ title, fun _ -> Mt.Eq (false, smaller == bigger));
     ]
    |> List.filter (fun _ -> sameTypes smaller bigger))

let assertEqual title num1 num2 =
  [
    ("compare: " ^ title, fun _ -> Mt.Eq (0, compare num1 num2));
    ("compare: " ^ title, fun _ -> Mt.Eq (0, compare num2 num1));
    ("< operator: " ^ title, fun _ -> Mt.Eq (false, num2 < num1));
    ("<= operator: " ^ title, fun _ -> Mt.Eq (true, num2 <= num1));
    ("> operator: " ^ title, fun _ -> Mt.Eq (false, num1 > num2));
    (">= operator: " ^ title, fun _ -> Mt.Eq (true, num1 >= num2));
    ("min: " ^ title, fun _ -> Mt.Eq (num1, min num1 num2));
    ("max: " ^ title, fun _ -> Mt.Eq (num1, max num1 num2));
  ]
  @ ([
       ("!= operator: " ^ title, fun _ -> Mt.Eq (false, num1 != num2));
       ("!= operator: " ^ title, fun _ -> Mt.Eq (false, num2 != num1));
       ("== operator: " ^ title, fun _ -> Mt.Eq (true, num1 == num2));
       ("== operator: " ^ title, fun _ -> Mt.Eq (true, num2 == num1));
     ]
    |> List.filter (fun _ -> sameTypes num1 num2))

let fiveBigInt = fromString "5"

let suites : Mt.pair_suites =
  assertLessThan "123 bigint and 555555 bigint" (fromString "123")
    (fromString "555555")
  @ assertLessThan "123 bigint and float 9999.99" (fromString "123") 9999.99
  @ assertEqual "98765 bigint and 98765 bigint" (fromString "98765")
      (fromString "98765")
  @ assertEqual "same instance" fiveBigInt fiveBigInt
  @ assertEqual "5 bigint and 5 float" fiveBigInt 5.0

let () = Mt.from_pair_suites __FILE__ suites
