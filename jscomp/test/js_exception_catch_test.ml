let suites :  Mt.pair_suites ref  = ref []

let add_test = 
  let counter = ref 0 in
  fun loc test -> 
    incr counter; 
    let id = (loc ^ " id " ^ (string_of_int !counter)) in 
    suites := (id, test) :: ! suites

let eq loc x y = 
  add_test loc (fun _ -> Mt.Eq (x, y)) 
let false_ loc =
  add_test loc (fun _ -> Mt.Ok false)

let true_ loc =
  add_test loc (fun _ -> Mt.Ok true)

let () =
  match Js.Json.parseExn {| {"x"}|} with 
  | exception Js.Exn.Error x -> 
    true_ __LOC__
  | e -> false_ __LOC__


exception A of int 
exception B 
exception C of int * int 

let test f  = 
  try f (); `No_error with
  | Not_found -> `Not_found
  | Invalid_argument "x" -> `Invalid_argument
  | Invalid_argument _ -> `Invalid_any
  | A 2 -> `A2
  | A _ -> `A_any
  | B -> `B
  | C (1,2) -> `C
  | C _ -> `C_any
  | Js.Exn.Error _ -> `Js_error
  | e -> `Any

let () = 
  eq __LOC__ (test (fun _ -> ())) `No_error;
  eq __LOC__ (test (fun _ -> raise Not_found)) `Not_found;
  eq __LOC__ (test (fun _ -> invalid_arg "x")) `Invalid_argument;
  eq __LOC__ (test (fun _ -> invalid_arg "")) `Invalid_any;
  eq __LOC__ (test (fun _ -> raise (A 2))) `A2;
  eq __LOC__ (test (fun _ -> raise (A 3))) `A_any;
  eq __LOC__ (test (fun _ -> raise B)) `B;
  eq __LOC__ (test (fun _ -> raise (C(1,2)))) `C;
  eq __LOC__ (test (fun _ -> raise (C(0,2)))) `C_any;
  eq __LOC__ (test (fun _ -> Js.Exn.raiseError "x")) `Js_error;
  eq __LOC__ (test (fun _ -> failwith "x")) `Any

;;


let () = Mt.from_pair_suites __MODULE__ !suites
