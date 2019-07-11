open Complex

let suites =
  Mt.
    [("basic_add", fun _ -> Eq ({re= 2.; im= 2.}, add (add (add one one) i) i))]

;;
Mt.from_pair_suites __MODULE__ suites
