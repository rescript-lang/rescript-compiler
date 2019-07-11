module BA1 = Bigarray.Array1

(* let v = BA1.create Int32 C_layout 20;; *)

let sum (v : (int32, 'a, 'b) BA1.t) =
  let result = ref 0l in
  for i = 0 to BA1.dim v - 1 do
    (* caml_ba_dim_1 *)
    result := Int32.add !result v.{i} (* caml_ba_get_1*)
  done

(* let vv : (int32, Bigarray.int32_elt, Bigarray.fortran_layout) BA1.t *)
(* = BA1.create Int32 Fortran_layout 30 ;; *)

let init (v : (Complex.t, Bigarray.int32_elt, Bigarray.c_layout) BA1.t) =
  for i = 0 to BA1.dim v - 1 do
    (* caml_ba_dim_1 *)
    (* let i = Int32.of_int i in *)
    v.{i} <- {re= float @@ (i * i); im= float @@ (i * i * i)}
  done

let init2 (v : (int32, Bigarray.int32_elt, Bigarray.c_layout) BA1.t) =
  for i = 0 to BA1.dim v - 1 do
    (* caml_ba_dim_1 *)
    (* let i = Int32.of_int i in *)
    v.{i} <- Int32.of_int i
  done

let init3 (v : (int32, _, Bigarray.c_layout) BA1.t) =
  for i = 0 to BA1.dim v - 1 do
    (* caml_ba_dim_1 *)
    (* let i = Int32.of_int i in *)
    v.{i} <- Int32.of_int i
  done

(* let a = *)
(* init v ; *)
(* (\* init vv; *\) *)
(* (sum v(\* , sum vv *\)) *)
