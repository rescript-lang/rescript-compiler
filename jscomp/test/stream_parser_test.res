exception Parse_error(string)

/**

expr 
| expr1  expr_aux 
expr_aux 
| + expr 
expr1
| expr2 expr1_aux 
expr1_aux
| * expr1

expr2
| Int
| ( expr )

*/
let parse = (token: unit => Genlex.token) => {
  let look_ahead = Queue.create()
  let token = () =>
    if Queue.is_empty(look_ahead) {
      try token() catch {
      | _ => Kwd("==")
      }
    } else {
      Queue.pop(look_ahead)
    }
  let push = e => Queue.push(e, look_ahead)
  let rec parse_atom = () =>
    switch token() {
    | Int(n) => n
    | Kwd("(") =>
      let v = parse_expr()
      switch token() {
      | Kwd(")") => v
      | _ => raise(Parse_error("Unbalanced parens"))
      }
    | e =>
      push(e)
      raise(Parse_error("unexpected token"))
    }
  and parse_term = () => parse_term_aux(parse_atom())
  and parse_term_aux = e1 =>
    switch token() {
    | Kwd("*") => e1 * parse_term()
    | Kwd("/") => e1 / parse_term()
    | e =>
      push(e)
      e1
    }
  and parse_expr = () => parse_expr_aux(parse_term())
  and parse_expr_aux = e1 =>
    switch token() {
    | Kwd("+") => e1 + parse_expr()
    | Kwd("-") => e1 - parse_expr()
    | e =>
      /* [%debugger]; */ push(e)
      e1
    }

  let r = parse_expr()
  (r, Queue.fold((acc, x) => list{x, ...acc}, list{}, look_ahead))
}
let lexer = Genlex.make_lexer(list{"(", "*", "/", "+", "-", ")"})
let token = chars => {
  let strm = lexer(chars)
  () => Stream.next(strm)
}

/*

http://www.engr.mun.ca/~theo/Misc/exp_parsing.htm

E 
| T { + T}
T 
| F { * F}
F
| Int 
| (E)

*/

let l_parse = (token: unit => Genlex.token) => {
  let look_ahead = Queue.create()
  let token = () =>
    if Queue.is_empty(look_ahead) {
      try token() catch {
      | _ => Kwd("==")
      }
    } else {
      Queue.pop(look_ahead)
    }
  let push = e => Queue.push(e, look_ahead)
  let rec parse_e = () => parse_t_aux(parse_t())
  and parse_t_aux = a =>
    switch token() {
    | Kwd("+") => parse_t_aux(a + parse_t())
    | Kwd("-") => parse_t_aux(a - parse_t())
    | t =>
      push(t)
      a
    }
  and parse_t = () => parse_f_aux(parse_f())
  and parse_f_aux = a =>
    switch token() {
    | Kwd("*") => parse_f_aux(a * parse_f())
    | Kwd("/") => parse_f_aux(a / parse_f())
    | t =>
      push(t)
      a
    }
  and parse_f = () =>
    switch token() {
    | Int(i) => i
    | Kwd("(") =>
      let v = parse_e()
      switch token() {
      | Kwd(")") => v
      | t => raise(Parse_error("Unbalanced )"))
      }
    | t => raise(Parse_error("Unexpected token"))
    }

  let r = parse_e()
  (r, Queue.fold((acc, x) => list{x, ...acc}, list{}, look_ahead))
}

let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => {
  incr(test_id)
  suites :=
    list{(loc ++ (" id " ++ string_of_int(test_id.contents)), _ => Mt.Eq(x, y)), ...suites.contents}
}

let () = {
  let (a, b) = parse(token(Stream.of_string("1 + 2 + (3  - 2) * 3 * 3  - 2 a")))

  eq(__LOC__, (a, b), (10, list{Ident("a")}))
  eq(__LOC__, (2, list{Genlex.Kwd("==")}), parse(token(Stream.of_string("3 - 2  - 1"))))
  eq(__LOC__, (0, list{Genlex.Kwd("==")}), l_parse(token(Stream.of_string("3 - 2  - 1"))))
}

let () = Mt.from_pair_suites(__MODULE__, suites.contents)
