let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc (x, y) =
  incr test_id ;
  suites :=
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


let print_or_error x  =
  match x with
  | `Ok  a ->  "Ok:" ^ Sexpm.to_string a
  | `Error a -> "Error:" ^ a

let () =
  begin
    let a = Sexpm.parse_string "(x x gh 3 3)" in
    eq __LOC__
      (
        `Ok (`List [`Atom "x"; `Atom "x"; `Atom "gh"; `Atom "3"; `Atom "3"]) ,
         a );
    eq __LOC__
      (Js.String2.trim (print_or_error a) , Js.String2.trim "Ok:(x x gh 3 3)\n")
  end


let ()
  = Mt.from_pair_suites __MODULE__ !suites

(**
[%bs.internal.test ]
*)
