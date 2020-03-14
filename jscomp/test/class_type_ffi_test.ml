
class type ['k,'v] arrayLike = 
  object 
    method case : 'k -> 'v Js.Null.t 
    method caseSet : 'k * 'v -> unit 
    method case__unsafe : 'k -> 'v 
    method length : int 
  end[@bs]

class type floatArray = [int, float] arrayLike
(** here we can see [@bs] is really attached to `object end` instead of `class type` *)
class type intArray = [int, int] arrayLike


let sum_float_array (arr : floatArray Js.t) = 
  let v = ref 0. in
  for i = 0 to arr##length - 1 do 
    v := !v +. arr##case__unsafe i     
  done;
  !v 

let sum_int_array (arr : intArray Js.t) = 
  let v = ref 0 in
  for i = 0 to arr##length - 1 do 
    v := !v + arr## case__unsafe i     
  done;
  !v 

(* TODO: warning about unprocessed attributes *)
let sum_poly zero add (arr : _ arrayLike Js.t) = 
  let v = ref zero in 
  for i = 0 to arr##length - 1 do 
    v := add  !v  (arr##case__unsafe i ) [@bs] 
  done;
  !v 


(* TODO: create a special type 
   ['a Js.prop_set] for better error message
*)
let test_set x = 
  x##length__aux #= 3 

let f (x : < bark : string -> unit [@bs.meth] ; fight : unit -> unit [@bs.meth] > Js.t)  =
  x##bark "he";
  x##fight ()

(* This type is generated on the fly -- in which case
  it can not be nominal
*)
(* let ff 
    (fn :('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9 -> 'a10 -> 'a11 -> 'a12 [@bs])) a0  a1  a2  a3  a4  a5  a6  a7  a8  a9  a10  a11  = 
  fn a0  a1  a2  a3  a4  a5  a6  a7  a8  a9  a10  a11 [@bs] *)


(* let ff2 
    fn a0  a1  a2  a3  a4  a5  a6  a7  a8  a9  a10  a11 a12 = 
  fn a0  a1  a2  a3  a4  a5  a6  a7  a8  a9  a10  a11 a12 [@bs] *)

(* Test [fn_run_method] *)
(* let off2 o  a0  a1  a2  a3  a4  a5  a6  a7  a8  a9  a10  a11 a12 = 
  o##huge_method a0  a1  a2  a3  a4  a5  a6  a7  a8  a9  a10  a11 a12 *)

(* Test [fn_mk] *)
(* let mk_f () = 
  fun [@bs] a0  a1  a2  a3  a4  a5  a6  a7  a8  a9  a10  a11 a12 -> 
  a0  a1  a2  a3  a4  a5  a6  a7  a8  a9  a10  a11 a12 *)

(* Test [fn_method] *)
(* let omk_f ()= 
  object
   method huge_methdo a0  a1  a2  a3  a4  a5  a6  a7  a8  a9  a10  a11 a12 =
  a0  a1  a2  a3  a4  a5  a6  a7  a8  a9  a10  a11 a12
end [@bs]   *)