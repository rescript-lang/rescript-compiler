let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, (x, y)) => {
  incr(test_id)
  suites :=
    list{(loc ++ (" id " ++ string_of_int(test_id.contents)), _ => Mt.Eq(x, y)), ...suites.contents}
}

let print_or_error = x =>
  switch x {
  | #Ok(a) => "Ok:" ++ Sexpm.to_string(a)
  | #Error(a) => "Error:" ++ a
  }

let () = {
  let a = Sexpm.parse_string("(x x gh 3 3)")
  eq(__LOC__, (#Ok(#List(list{#Atom("x"), #Atom("x"), #Atom("gh"), #Atom("3"), #Atom("3")})), a))
  eq(__LOC__, (Js.String2.trim(print_or_error(a)), Js.String2.trim("Ok:(x x gh 3 3)\n")))
}

let () = Mt.from_pair_suites(__MODULE__, suites.contents)

/*
[%bs.internal.test ]
*/
