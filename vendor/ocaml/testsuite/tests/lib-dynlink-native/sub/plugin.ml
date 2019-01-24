let rec fact n = if n = 0 then 1 else n * fact (n - 1)

let facts = [ fact 1; fact 2; fact 3; fact 4; fact 5 ]

let () =
  Api.reg_mod "Plugin'"
