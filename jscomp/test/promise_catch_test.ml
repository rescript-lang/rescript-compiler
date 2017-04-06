let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites



type x = ..

type x += A of int 

(** rejectXXError for the FFI .. which is similar to [bs.this] *)
let handler  = fun [@bs.exn] e -> 
  match  e with 
  | Js.Exn.Error v -> `Error
  | Not_found -> `Not_found
  | Invalid_argument _ -> `Invalid_argument
  (* | A _ -> `A *)
  | _ -> `Any

type exn += B of int 
(** check exception rebind *)
let () = 
  eq __LOC__ (handler 1 ) `Error;
  eq __LOC__ (handler Not_found) `Not_found;
  eq __LOC__ (handler (Invalid_argument "")) `Invalid_argument;
  eq __LOC__ (handler (Stack_overflow)) `Any;
  eq __LOC__ (handler (A 3)) `Any 

;; Mt.from_pair_suites __FILE__ !suites


 



