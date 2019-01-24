(* Data that should be statically allocated by the compiler (flambda only) *)

external is_in_static_data : 'a -> bool = "caml_is_in_static_data"

(* Also after inlining *)
let g x =
  let block = (1,x) in
  assert(is_in_static_data block)

let () = (g [@inlined always]) 2

(* Toplevel immutable blocks should be static *)
let block3 = (Sys.opaque_identity 1, Sys.opaque_identity 2)
let () = assert(is_in_static_data block3)

(* Not being bound shouldn't prevent it *)
let () =
  assert(is_in_static_data (Sys.opaque_identity 1, Sys.opaque_identity 2))

(* Only with rounds >= 2 currently !
(* Also after inlining *)
let h x =
  let block = (Sys.opaque_identity 1,x) in
  assert(is_in_static_data block)

let () = (h [@inlined always]) (Sys.opaque_identity 2)
*)

(* Recursive constant values should be static *)
let rec a = 1 :: b
and b = 2 :: a
let () =
  assert(is_in_static_data a);
  assert(is_in_static_data b)

(* And a mix *)
type e = E : 'a -> e

let rec f1 a = E (g1 a, l1)
and g1 a = E (f1 a, l2)
and l1 = E (f1, l2)
and l2 = E (g1, l1)

let () =
  assert(is_in_static_data f1);
  assert(is_in_static_data g1);
  assert(is_in_static_data l1);
  assert(is_in_static_data l2)

(* Also in functions *)
let i () =
  let rec f1 a = E (g1 a, l1)
  and g1 a = E (f1 a, l2)
  and l1 = E (f1, l2)
  and l2 = E (g1, l1) in

  assert(is_in_static_data f1);
  assert(is_in_static_data g1);
  assert(is_in_static_data l1);
  assert(is_in_static_data l2)

let () = (i [@inlined never]) ()

module type P = module type of Pervasives
(* Top-level modules should be static *)
let () = assert(is_in_static_data (module Pervasives:P))

(* Not constant let rec to test extraction to initialize_symbol *)
let r = ref 0
let rec a = (incr r; !r) :: b
and b = (incr r; !r) :: a

let next =
  let r = ref 0 in
  fun () -> incr r; !r

let () =
  assert(is_in_static_data next)

(* Exceptions without arguments should be static *)
exception No_argument
let () = assert(is_in_static_data No_argument)

(* And also with constant arguments *)
exception Some_argument of string
let () = assert(is_in_static_data (Some_argument "some string"))

(* Even when exposed by inlining *)
let () =
  let exn =
    try (failwith [@inlined always]) "some other string" with exn -> exn
  in
  assert(is_in_static_data exn)

(* Verify that approximation intersection correctly loads exported
   approximations.

   Is_static_flambda_dep.pair is a pair with 1 as first element. The
   intersection of approximations should return a block with
   approximation: [tag 0: [tag 0: Int 1, Unknown], Unknown] *)
let f x =
  let pair =
    if Sys.opaque_identity x then
      (1, 2), 3
    else
      Is_static_flambda_dep.pair, 4
  in
  let n = fst (fst pair) in
  let res = n, n in
  assert(is_in_static_data res)
  [@@inline never]

let () =
  f true;
  f false

(* Verify that physical equality/inequality is correctly propagated *)

(* In these tests, tuple can be statically allocated only if it is a
   known constant since the function is never inlined (hence this
   code is never at toplevel) *)

let () =
  let f () =
    let v = (1, 2) in
    (* eq is supposed to be considered always true since v is a
       constant, hence aliased to a symbol.
       It is not yet optimized away if it is not constant *)
    let eq = v == v in
    let n = if eq then 1 else 2 in
    let tuple = (n,n) in
    assert(is_in_static_data tuple)
  in
  (f [@inlined never]) ()

let () =
  let f () =
    let v = (1, 2) in
    (* same with inequality *)
    let eq = v != v in
    let n = if eq then 1 else 2 in
    let tuple = (n,n) in
    assert(is_in_static_data tuple)
  in
  (f [@inlined never]) ()

let () =
  let f x =
    let v1 = Some x in
    let v2 = None in
    let eq = v1 == v2 in
    (* The values are structurally different, so must be physically
       different *)
    let n = if eq then 1 else 2 in
    let tuple = (n,n) in
    assert(is_in_static_data tuple)
  in
  (f [@inlined never]) ()

let () =
  let f x =
    let v1 = Some x in
    let v2 = None in
    let eq = v1 != v2 in
    (* same with inequality *)
    let n = if eq then 1 else 2 in
    let tuple = (n,n) in
    assert(is_in_static_data tuple)
  in
  (f [@inlined never]) ()

let () =
  let f x =
    let v1 = (1, 2) in
    let v2 = (3, 2) in
    let eq = v1 == v2 in
    (* difference is deeper *)
    let n = if eq then 1 else 2 in
    let tuple = (n,n) in
    assert(is_in_static_data tuple)
  in
  (f [@inlined never]) ()

module Int = struct
  type t = int
  let compare (a:int) b = compare a b
end
module IntMap = (Map.Make [@inlined])(Int)

let () =
  let f () =
    let a = IntMap.empty in
    let b = (IntMap.add [@inlined]) 1 (Some 1) a in
    assert(is_in_static_data b);
    let c = (IntMap.add [@inlined]) 1 (Some 2) b in
    assert(is_in_static_data c);
    let d = (IntMap.add [@inlined]) 1 (Some 2) c in
    assert(is_in_static_data d);
  in
  (f [@inlined never]) ()
