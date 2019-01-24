external cmp : 'a -> 'a -> int = "%compare";;
external eq : 'a -> 'a -> bool = "%equal";;
external ne : 'a -> 'a -> bool = "%notequal";;
external lt : 'a -> 'a -> bool = "%lessthan";;
external gt : 'a -> 'a -> bool = "%greaterthan";;
external le : 'a -> 'a -> bool = "%lessequal";;
external ge : 'a -> 'a -> bool = "%greaterequal";;

type intlike = A | B | C | D

(* Check specialization in explicit application *)

let gen_cmp x y = cmp x y;;
let int_cmp (x : int) y = cmp x y;;
let bool_cmp (x : bool) y = cmp x y;;
let intlike_cmp (x : intlike) y = cmp x y;;
let float_cmp (x : float) y = cmp x y;;
let string_cmp (x : string) y = cmp x y;;
let int32_cmp (x : int32) y = cmp x y;;
let int64_cmp (x : int64) y = cmp x y;;
let nativeint_cmp (x : nativeint) y = cmp x y;;

let gen_eq x y = eq x y;;
let int_eq (x : int) y = eq x y;;
let bool_eq (x : bool) y = eq x y;;
let intlike_eq (x : intlike) y = eq x y;;
let float_eq (x : float) y = eq x y;;
let string_eq (x : string) y = eq x y;;
let int32_eq (x : int32) y = eq x y;;
let int64_eq (x : int64) y = eq x y;;
let nativeint_eq (x : nativeint) y = eq x y;;

let gen_ne x y = ne x y;;
let int_ne (x : int) y = ne x y;;
let bool_ne (x : bool) y = ne x y;;
let intlike_ne (x : intlike) y = ne x y;;
let float_ne (x : float) y = ne x y;;
let string_ne (x : string) y = ne x y;;
let int32_ne (x : int32) y = ne x y;;
let int64_ne (x : int64) y = ne x y;;
let nativeint_ne (x : nativeint) y = ne x y;;

let gen_lt x y = lt x y;;
let int_lt (x : int) y = lt x y;;
let bool_lt (x : bool) y = lt x y;;
let intlike_lt (x : intlike) y = lt x y;;
let float_lt (x : float) y = lt x y;;
let string_lt (x : string) y = lt x y;;
let int32_lt (x : int32) y = lt x y;;
let int64_lt (x : int64) y = lt x y;;
let nativeint_lt (x : nativeint) y = lt x y;;

let gen_gt x y = gt x y;;
let int_gt (x : int) y = gt x y;;
let bool_gt (x : bool) y = gt x y;;
let intlike_gt (x : intlike) y = gt x y;;
let float_gt (x : float) y = gt x y;;
let string_gt (x : string) y = gt x y;;
let int32_gt (x : int32) y = gt x y;;
let int64_gt (x : int64) y = gt x y;;
let nativeint_gt (x : nativeint) y = gt x y;;

let gen_le x y = le x y;;
let int_le (x : int) y = le x y;;
let bool_le (x : bool) y = le x y;;
let intlike_le (x : intlike) y = le x y;;
let float_le (x : float) y = le x y;;
let string_le (x : string) y = le x y;;
let int32_le (x : int32) y = le x y;;
let int64_le (x : int64) y = le x y;;
let nativeint_le (x : nativeint) y = le x y;;

let gen_ge x y = ge x y;;
let int_ge (x : int) y = ge x y;;
let bool_ge (x : bool) y = ge x y;;
let intlike_ge (x : intlike) y = ge x y;;
let float_ge (x : float) y = ge x y;;
let string_ge (x : string) y = ge x y;;
let int32_ge (x : int32) y = ge x y;;
let int64_ge (x : int64) y = ge x y;;
let nativeint_ge (x : nativeint) y = ge x y;;

(* Check specialization in eta-expansion *)

let eta_gen_cmp : 'a -> _ = cmp;;
let eta_int_cmp : int -> _ = cmp;;
let eta_bool_cmp : bool -> _ = cmp;;
let eta_intlike_cmp : intlike -> _ = cmp;;
let eta_float_cmp : float -> _ = cmp;;
let eta_string_cmp : string -> _ = cmp;;
let eta_int32_cmp : int32 -> _ = cmp;;
let eta_int64_cmp : int64 -> _ = cmp;;
let eta_nativeint_cmp : nativeint -> _ = cmp;;

let eta_gen_eq : 'a -> _ = eq;;
let eta_int_eq : int -> _ = eq;;
let eta_bool_eq : bool -> _ = eq;;
let eta_intlike_eq : intlike -> _ = eq;;
let eta_float_eq : float -> _ = eq;;
let eta_string_eq : string -> _ = eq;;
let eta_int32_eq : int32 -> _ = eq;;
let eta_int64_eq : int64 -> _ = eq;;
let eta_nativeint_eq : nativeint -> _ = eq;;

let eta_gen_ne : 'a -> _ = ne;;
let eta_int_ne : int -> _ = ne;;
let eta_bool_ne : bool -> _ = ne;;
let eta_intlike_ne : intlike -> _ = ne;;
let eta_float_ne : float -> _ = ne;;
let eta_string_ne : string -> _ = ne;;
let eta_int32_ne : int32 -> _ = ne;;
let eta_int64_ne : int64 -> _ = ne;;
let eta_nativeint_ne : nativeint -> _ = ne;;

let eta_gen_lt : 'a -> _ = lt;;
let eta_int_lt : int -> _ = lt;;
let eta_bool_lt : bool -> _ = lt;;
let eta_intlike_lt : intlike -> _ = lt;;
let eta_float_lt : float -> _ = lt;;
let eta_string_lt : string -> _ = lt;;
let eta_int32_lt : int32 -> _ = lt;;
let eta_int64_lt : int64 -> _ = lt;;
let eta_nativeint_lt : nativeint -> _ = lt;;

let eta_gen_gt : 'a -> _ = gt;;
let eta_int_gt : int -> _ = gt;;
let eta_bool_gt : bool -> _ = gt;;
let eta_intlike_gt : intlike -> _ = gt;;
let eta_float_gt : float -> _ = gt;;
let eta_string_gt : string -> _ = gt;;
let eta_int32_gt : int32 -> _ = gt;;
let eta_int64_gt : int64 -> _ = gt;;
let eta_nativeint_gt : nativeint -> _ = gt;;

let eta_gen_le : 'a -> _ = le;;
let eta_int_le : int -> _ = le;;
let eta_bool_le : bool -> _ = le;;
let eta_intlike_le : intlike -> _ = le;;
let eta_float_le : float -> _ = le;;
let eta_string_le : string -> _ = le;;
let eta_int32_le : int32 -> _ = le;;
let eta_int64_le : int64 -> _ = le;;
let eta_nativeint_le : nativeint -> _ = le;;

let eta_gen_ge : 'a -> _ = ge;;
let eta_int_ge : int -> _ = ge;;
let eta_bool_ge : bool -> _ = ge;;
let eta_intlike_ge : intlike -> _ = ge;;
let eta_float_ge : float -> _ = ge;;
let eta_string_ge : string -> _ = ge;;
let eta_int32_ge : int32 -> _ = ge;;
let eta_int64_ge : int64 -> _ = ge;;
let eta_nativeint_ge : nativeint -> _ = ge;;

(* Check results of computations *)

let int_vec = [(1,1);(1,2);(2,1)];;
let bool_vec = [(false,false);(false,true);(true,false)];;
let intlike_vec = [(A,A);(A,B);(B,A)];;
let float_vec = [(1.,1.);(1.,2.);(2.,1.)];;
let string_vec = [("1","1");("1","2");("2","1")];;
let int32_vec = [(1l,1l);(1l,2l);(2l,1l)];;
let int64_vec = [(1L,1L);(1L,2L);(2L,1L)];;
let nativeint_vec = [(1n,1n);(1n,2n);(2n,1n)];;

let test_vec cmp eq ne lt gt le ge vec =
  let uncurry f (x,y) = f x y in
  let map f l = List.map (uncurry f) l in
  (map gen_cmp vec, map cmp vec),
  (map (fun gen spec -> map gen vec, map spec vec)
     [gen_eq,eq; gen_ne,ne; gen_lt,lt; gen_gt,gt; gen_le,le; gen_ge,ge])
;;

test_vec
  int_cmp int_eq int_ne int_lt int_gt int_le int_ge
  int_vec;;
test_vec
  bool_cmp bool_eq bool_ne bool_lt bool_gt bool_le bool_ge
  bool_vec;;
test_vec
  intlike_cmp intlike_eq intlike_ne intlike_lt intlike_gt intlike_le intlike_ge
  intlike_vec;;
test_vec
  float_cmp float_eq float_ne float_lt float_gt float_le float_ge
  float_vec;;
test_vec
  string_cmp string_eq string_ne string_lt string_gt string_le string_ge
  string_vec;;
test_vec
  int32_cmp int32_eq int32_ne int32_lt int32_gt int32_le int32_ge
  int32_vec;;
test_vec
  int64_cmp int64_eq int64_ne int64_lt int64_gt int64_le int64_ge
  int64_vec;;
test_vec
  nativeint_cmp nativeint_eq nativeint_ne
  nativeint_lt nativeint_gt nativeint_le nativeint_ge
  nativeint_vec;;

let eta_test_vec cmp eq ne lt gt le ge vec =
  let uncurry f (x,y) = f x y in
  let map f l = List.map (uncurry f) l in
  (map eta_gen_cmp vec, map cmp vec),
  (map (fun gen spec -> map gen vec, map spec vec)
     [eta_gen_eq,eq; eta_gen_ne,ne; eta_gen_lt,lt;
      eta_gen_gt,gt; eta_gen_le,le; eta_gen_ge,ge])
;;

eta_test_vec
  eta_int_cmp eta_int_eq eta_int_ne eta_int_lt eta_int_gt eta_int_le eta_int_ge
  int_vec;;
eta_test_vec
  eta_bool_cmp eta_bool_eq eta_bool_ne eta_bool_lt eta_bool_gt
  eta_bool_le eta_bool_ge
  bool_vec;;
eta_test_vec
  eta_intlike_cmp eta_intlike_eq eta_intlike_ne eta_intlike_lt eta_intlike_gt
  eta_intlike_le eta_intlike_ge
  intlike_vec;;
eta_test_vec
  eta_float_cmp eta_float_eq eta_float_ne eta_float_lt eta_float_gt
  eta_float_le eta_float_ge
  float_vec;;
eta_test_vec
  eta_string_cmp eta_string_eq eta_string_ne eta_string_lt eta_string_gt
  eta_string_le eta_string_ge
  string_vec;;
eta_test_vec
  eta_int32_cmp eta_int32_eq eta_int32_ne eta_int32_lt eta_int32_gt
  eta_int32_le eta_int32_ge
  int32_vec;;
eta_test_vec
  eta_int64_cmp eta_int64_eq eta_int64_ne eta_int64_lt eta_int64_gt
  eta_int64_le eta_int64_ge
  int64_vec;;
eta_test_vec
  eta_nativeint_cmp eta_nativeint_eq eta_nativeint_ne
  eta_nativeint_lt eta_nativeint_gt eta_nativeint_le eta_nativeint_ge
  nativeint_vec;;
