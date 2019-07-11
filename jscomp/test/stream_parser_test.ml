exception Parse_error of string

(**

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

*)
let parse (token : unit -> Genlex.token) =
  let look_ahead = Queue.create () in
  let token () =
    if Queue.is_empty look_ahead then try token () with _ -> Kwd "=="
    else Queue.pop look_ahead
  in
  let push e = Queue.push e look_ahead in
  let rec parse_atom () =
    match token () with
    | Int n ->
        n
    | Kwd "(" -> (
        let v = parse_expr () in
        match token () with
        | Kwd ")" ->
            v
        | _ ->
            raise (Parse_error "Unbalanced parens") )
    | e ->
        push e ;
        raise (Parse_error "unexpected token")
  and parse_term () = parse_term_aux (parse_atom ())
  and parse_term_aux e1 =
    match token () with
    | Kwd "*" ->
        e1 * parse_term ()
    | Kwd "/" ->
        e1 / parse_term ()
    | e ->
        push e ; e1
  and parse_expr () = parse_expr_aux (parse_term ())
  and parse_expr_aux e1 =
    match token () with
    | Kwd "+" ->
        e1 + parse_expr ()
    | Kwd "-" ->
        e1 - parse_expr ()
    | e ->
        (* [%debugger]; *) push e ; e1
  in
  let r = parse_expr () in
  (r, Queue.fold (fun acc x -> x :: acc) [] look_ahead)

let lexer = Genlex.make_lexer ["("; "*"; "/"; "+"; "-"; ")"]

let token chars =
  let strm = lexer chars in
  fun () -> Stream.next strm

(**

http://www.engr.mun.ca/~theo/Misc/exp_parsing.htm

E 
| T { + T}
T 
| F { * F}
F
| Int 
| (E)

*)

let l_parse (token : unit -> Genlex.token) =
  let look_ahead = Queue.create () in
  let token () =
    if Queue.is_empty look_ahead then try token () with _ -> Kwd "=="
    else Queue.pop look_ahead
  in
  let push e = Queue.push e look_ahead in
  let rec parse_e () = parse_t_aux (parse_t ())
  and parse_t_aux a =
    match token () with
    | Kwd "+" ->
        parse_t_aux (a + parse_t ())
    | Kwd "-" ->
        parse_t_aux (a - parse_t ())
    | t ->
        push t ; a
  and parse_t () = parse_f_aux (parse_f ())
  and parse_f_aux a =
    match token () with
    | Kwd "*" ->
        parse_f_aux (a * parse_f ())
    | Kwd "/" ->
        parse_f_aux (a / parse_f ())
    | t ->
        push t ; a
  and parse_f () =
    match token () with
    | Int i ->
        i
    | Kwd "(" -> (
        let v = parse_e () in
        match token () with
        | Kwd ")" ->
            v
        | t ->
            raise (Parse_error "Unbalanced )") )
    | t ->
        raise (Parse_error "Unexpected token")
  in
  let r = parse_e () in
  (r, Queue.fold (fun acc x -> x :: acc) [] look_ahead)

let suites : Mt.pair_suites ref = ref []

let test_id = ref 0

let eq loc x y =
  incr test_id ;
  suites :=
    (loc ^ " id " ^ string_of_int !test_id, fun _ -> Mt.Eq (x, y)) :: !suites

let () =
  let a, b =
    parse (token (Stream.of_string "1 + 2 + (3  - 2) * 3 * 3  - 2 a"))
  in
  eq __LOC__ (a, b) (10, [Ident "a"]) ;
  eq __LOC__
    (2, [Genlex.Kwd "=="])
    (parse (token (Stream.of_string "3 - 2  - 1"))) ;
  eq __LOC__
    (0, [Genlex.Kwd "=="])
    (l_parse (token (Stream.of_string "3 - 2  - 1")))

let () = Mt.from_pair_suites __MODULE__ !suites
