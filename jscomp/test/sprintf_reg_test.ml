let suites : Mt.pair_suites ref = ref []
let test_id = ref 0
let eq f (a, b) = Mt_global.collect_eq test_id suites f a b

let () =
  let s = Printf.sprintf "%{%s%}." "32%s" in
  eq __LOC__ (s, "%s.")

let () =
  let s = Printf.sprintf "%i %{%s%}" 1 "spells one %s" in
  eq __LOC__ (s, "1 %s")

let () = Mt.from_pair_suites __MODULE__ !suites
