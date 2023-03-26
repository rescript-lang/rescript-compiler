external fromString : string -> float = "BigInt" [@@bs.val]

let sml = fromString "123"
let big = fromString "3944949939394"

let suites : Mt.pair_suites =
  [
    (* compare bigint and bigint *)
    ("compare bigint and bigint", fun _ -> Eq (true, compare big sml > 0));
    ("compare bigint and bigint", fun _ -> Eq (true, compare sml big < 0));
    ("compare bigint and bigint", fun _ -> Eq (true, compare sml sml == 0));
    (* compare bigint and float *)
    ("compare bigint and float", fun _ -> Eq (true, compare big 1.1 > 0));
    ("compare bigint and float", fun _ -> Eq (true, compare 1.1 big < 0));
    ( "compare bigint and float",
      fun _ -> Eq (true, compare (fromString "3") (fromString "3") == 0) );
    (* < operator *)
    ("< operator", fun _ -> Eq (true, sml < big));
    ("< operator", fun _ -> Eq (false, big < sml));
    ("< operator", fun _ -> Eq (false, big < big));
    (* < operator with float *)
    ("< operator with float", fun _ -> Eq (true, 1.1 < big));
    ("< operator with float", fun _ -> Eq (false, big < 1.1));
    (* <= operator *)
    ("<= operator", fun _ -> Eq (true, sml <= big));
    ("<= operator", fun _ -> Eq (false, big <= sml));
    ("<= operator", fun _ -> Eq (true, big <= big));
    (* > operator *)
    ("> operator", fun _ -> Eq (false, sml > big));
    ("> operator", fun _ -> Eq (true, big > sml));
    ("> operator", fun _ -> Eq (false, big > big));
    (* >= operator *)
    (">= operator", fun _ -> Eq (false, sml >= big));
    (">= operator", fun _ -> Eq (true, big >= sml));
    (">= operator", fun _ -> Eq (true, big >= big));
    (* == operator *)
    ("== operator", fun _ -> Eq (false, sml == big));
    ("== operator", fun _ -> Eq (true, big == big));
    (* == operator with float *)
    (* ("== operator with float", fun _ -> Eq (true, fromString "3" == 3.0));
       ("== operator with float", fun _ -> Eq (true, 3.0 == fromString "3"));
       ("== operator with float", fun _ -> Eq (false, 3.0 == big)); *)
    (* != operator *)
    ("!= operator", fun _ -> Eq (true, sml != big));
    ("!= operator", fun _ -> Eq (true, big == big));
    (* != operator with float *)
    (* ("!= operator with float", fun _ -> Eq (false, fromString "3" != 3.0));
       ("!= operator with float", fun _ -> Eq (false, 3.0 != fromString "3"));
       ("!= operator with float", fun _ -> Eq (true, 3.0 != big)); *)
  ]
let () = Mt.from_pair_suites __FILE__ suites
