external len : 'a array -> int = "%array_length"
external safe_get : 'a array -> int -> 'a = "%array_safe_get"
external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
external safe_set : 'a array -> int -> 'a -> unit = "%array_safe_set"
external unsafe_set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"

(* Specialization in application *)

let int_a = [|1;2;3|];;
let float_a = [|1.;2.;3.|];;
let addr_a = [|"a";"b";"c"|];;

len int_a;;
len float_a;;
len addr_a;;
(fun a -> len a);;

safe_get int_a 0;;
safe_get float_a 0;;
safe_get addr_a 0;;
(fun a -> safe_get a 0);;

unsafe_get int_a 0;;
unsafe_get float_a 0;;
unsafe_get addr_a 0;;
(fun a -> unsafe_get a 0);;

safe_set int_a 0 1;;
safe_set float_a 0 1.;;
safe_set addr_a 0 "a";;
(fun a x -> safe_set a 0 x);;

unsafe_set int_a 0 1;;
unsafe_set float_a 0 1.;;
unsafe_set addr_a 0 "a";;
(fun a x -> unsafe_set a 0 x);;

(* Specialization during eta-expansion *)

let eta_gen_len : 'a array -> _ = len;;
let eta_gen_safe_get : 'a array -> int -> 'a = safe_get;;
let eta_gen_unsafe_get : 'a array -> int -> 'a = unsafe_get;;
let eta_gen_safe_set : 'a array -> int -> 'a -> unit = safe_set;;
let eta_gen_unsafe_set : 'a array -> int -> 'a -> unit = unsafe_set;;

let eta_int_len : int array -> _ = len;;
let eta_int_safe_get : int array -> int -> int = safe_get;;
let eta_int_unsafe_get : int array -> int -> int = unsafe_get;;
let eta_int_safe_set : int array -> int -> int -> unit = safe_set;;
let eta_int_unsafe_set : int array -> int -> int -> unit = unsafe_set;;

let eta_float_len : float array -> _ = len;;
let eta_float_safe_get : float array -> int -> float = safe_get;;
let eta_float_unsafe_get : float array -> int -> float = unsafe_get;;
let eta_float_safe_set : float array -> int -> float -> unit = safe_set;;
let eta_float_unsafe_set : float array -> int -> float -> unit = unsafe_set;;

let eta_addr_len : string array -> _ = len;;
let eta_addr_safe_get : string array -> int -> string = safe_get;;
let eta_addr_unsafe_get : string array -> int -> string = unsafe_get;;
let eta_addr_safe_set : string array -> int -> string -> unit = safe_set;;
let eta_addr_unsafe_set : string array -> int -> string -> unit = unsafe_set;;
