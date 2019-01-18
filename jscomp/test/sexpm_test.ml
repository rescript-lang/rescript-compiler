let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc (x, y) =
  incr test_id ;
  suites :=
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


let print_or_error fmt x  =
  match x with
  | `Ok  a -> Format.fprintf fmt "@[Ok:%a@]@." Sexpm.print a
  | `Error a -> Format.fprintf fmt "@[Error:%s@]@." a

let () =
  begin
    let a = Sexpm.parse_string "(x x gh 3 3)" in
    eq __LOC__
      (
        `Ok (`List [`Atom "x"; `Atom "x"; `Atom "gh"; `Atom "3"; `Atom "3"]) ,
         a );
    eq __LOC__
      (Js.String2.trim (Format.asprintf "%a" print_or_error a) , Js.String2.trim "Ok:(x x gh 3 3)\n")
  end


let ()
  = Mt.from_pair_suites __MODULE__ !suites

(**
[%bs.internal.test ]
*)
