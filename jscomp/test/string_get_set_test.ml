
external caml_string_get_16 : string -> int -> int = "%caml_string_get16"
external caml_string_get_32 : string -> int -> int32 = "%caml_string_get32"
external caml_string_get_64 : string -> int -> int64 = "%caml_string_get64"

(* TODO: endian-aware ? *)
;; Mt.from_pair_suites __MODULE__ Mt.[
    __LOC__ , (fun _ -> Eq (caml_string_get_16 "2\000" 0, 50));
    __LOC__ , (fun _ -> Eq (caml_string_get_16 "20" 0, 12338));
    __LOC__, (fun _ -> Eq(caml_string_get_32 "0123" 0, 858927408l));
    __LOC__, (fun _ -> Eq(caml_string_get_32 "0123" 0, 858927408l));
    __LOC__, (fun _ -> Eq(caml_string_get_32 "3210" 0, 808530483l));
    __LOC__, (fun _ -> Eq(caml_string_get_64 "12345678" 0, 4050765991979987505L));
    __LOC__, (fun _ -> Eq(caml_string_get_64 "87654321" 0, 3544952156018063160L))
]

(* (\** does not make sense under immutable string setting *\) *)
(* external caml_string_set_16 : string -> int -> int -> unit = *)
(*   "%caml_string_set16" *)
(* external caml_string_set_32 : string -> int -> int32 -> unit = *)
(*   "%caml_string_set32" *)
(* external caml_string_set_64 : string -> int -> int64 -> unit = *)
(*   "%caml_string_set64" *)

(* let s = String.make 10 '\x00' *)
(* let empty_s = "" *)

(* let assert_bound_check2 f v1 v2 = *)
(*   try *)
(*     ignore(f v1 v2); *)
(*     assert false *)
(*   with *)
(*      | Invalid_argument("index out of bounds") -> () *)

(* let assert_bound_check3 f v1 v2 v3 = *)
(*   try *)
(*     ignore(f v1 v2 v3); *)
(*     assert false *)
(*   with *)
(*      | Invalid_argument("index out of bounds") -> () *)

(* let f () = *)
(*   assert_bound_check2 caml_string_get_16 s (-1); *)
(*   assert_bound_check2 caml_string_get_16 s 9; *)
(*   assert_bound_check2 caml_string_get_32 s (-1); *)
(*   assert_bound_check2 caml_string_get_32 s 7; *)
(*   assert_bound_check2 caml_string_get_64 s (-1); *)
(*   assert_bound_check2 caml_string_get_64 s 3; *)

(*   assert_bound_check3 caml_string_set_16 s (-1) 0; *)
(*   assert_bound_check3 caml_string_set_16 s 9 0; *)
(*   assert_bound_check3 caml_string_set_32 s (-1) 0l; *)
(*   assert_bound_check3 caml_string_set_32 s 7 0l; *)
(*   assert_bound_check3 caml_string_set_64 s (-1) 0L; *)
(*   assert_bound_check3 caml_string_set_64 s 3 0L; *)

(*   assert_bound_check2 caml_string_get_16 empty_s 0; *)
(*   assert_bound_check2 caml_string_get_32 empty_s 0; *)
(*   assert_bound_check2 caml_string_get_64 empty_s 0; *)

(*   assert_bound_check3 caml_string_set_16 empty_s 0 0; *)
(*   assert_bound_check3 caml_string_set_32 empty_s 0 0l; *)
(*   assert_bound_check3 caml_string_set_64 empty_s 0 0L *)
