let collect_eq = (test_id, suites, loc, x, y) => {
  incr(test_id)
  suites :=
    list{(loc ++ (" id " ++ string_of_int(test_id.contents)), _ => Mt.Eq(x, y)), ...suites.contents}
}
let collect_neq = (test_id, suites, loc, x, y) => {
  incr(test_id)
  suites :=
    list{
      (loc ++ (" id " ++ string_of_int(test_id.contents)), _ => Mt.Neq(x, y)),
      ...suites.contents,
    }
}

let collect_approx = (test_id, suites, loc, x, y) => {
  incr(test_id)
  suites :=
    list{
      (loc ++ (" id " ++ string_of_int(test_id.contents)), _ => Mt.Approx(x, y)),
      ...suites.contents,
    }
}
