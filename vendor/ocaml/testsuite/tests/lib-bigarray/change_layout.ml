(** Test the various change_layout for Genarray and the various Array[n] *)

open Bigarray

let pp_sep ppf () = Format.fprintf ppf ";@ "
 let print_array pp ppf a =
 Format.fprintf ppf "@[<hov>[|%a|]@]"
    Format.(pp_print_list ~pp_sep pp) (Array.to_list a)

let print_index = print_array Format.pp_print_int

let do_test n test =
  let rec aux l n =
    if n = 0 then l
    else
      aux
        begin match test (n-1) with
        | Some error -> error :: l
        | None -> l
        end
        (n-1) in
  aux [] n

let kind = float64

let c = c_layout
let fortran = fortran_layout

let rank = 5
let dims = Array.init rank (fun n -> n+2)
let size = Array.fold_left ( * ) 1 dims

let report s test =
  let errors = do_test size test in
  if errors = [] then
    Format.printf"@[%s: Ok@]@." s
  else
    Format.printf "@[%s:@;Failed at indices @[<hov>%a@]@]@." s
      (Format.pp_print_list ~pp_sep print_index)
      errors

let array =
  let a = Array1.create kind c size in
  for i = 0 to size - 1 do a.{i} <- float i done;
  a

(** Test for generic biggarray *)
let gen = reshape (genarray_of_array1 array) dims

let sizes =
  let a = Array.make rank 1 in
  let _ = Array.fold_left (fun (i,s) x -> a.(i)<- s; (i+1, s * x)) (0,1) dims in
  a

let multi_index n =
  Array.init rank ( fun i ->  (n / sizes.(i)) mod dims.(i)  )

let testG n =
  let pos = multi_index n in
  let initial = Genarray.get gen pos in
  Genarray.set gen pos (-1.);
  let different = Genarray.get gen pos <> initial in
  let gen' = Genarray.change_layout gen fortran in
  Genarray.set gen' ( Array.init rank @@ fun n -> 1 + pos.( rank - 1 - n ) ) initial;
  if not (different && initial = Genarray.get gen pos) then Some pos
  else None

;;
report "Generic rank test" testG
;;

(* Scalar *)
let scalar =
  let a = Array0.create kind c in
  Array0.set a 0.; a
;;
let test  =
  let a' = Array0.change_layout scalar fortran in
  Array0.set a' 1.;
  Array0.get scalar = 1.

;;
Format.printf "Scalar test: %s@." (if test then "Ok" else "Failed")
;;

(* Vector *)
let vec = array1_of_genarray @@ reshape gen [|size|]
let test1 i =
  let initial = vec.{i}  in
  vec.{i} <- -1.;
  let different = vec.{i} <> initial in
  let vec' = Array1.change_layout vec fortran in
  vec'.{ i + 1 } <- initial;
  if different && initial = vec.{i} then None
    else Some [|i|]

;;
report "Rank-1 array test" test1
;;

(* Matrix *)
let mat = array2_of_genarray @@ reshape gen [|dims.(0); size / dims.(0) |]
let bi_index n = n mod dims.(0), n / dims.(0)

let test2 n =
  let i, j = bi_index n in
  let initial = mat.{i,j}  in
  mat.{i,j} <- -1.;
  let different = mat.{i,j} <> initial in
  let mat' = Array2.change_layout mat fortran in
  mat'.{ j + 1, i + 1 } <- initial;
  if different && initial = mat.{i, j} then None
    else Some [|i; j|]


;;
report "Rank-2 array test" test2
;;

(* Rank 3 *)
let t3 = array3_of_genarray @@
  reshape gen [|dims.(0); dims.(1); size / (dims.(0) * dims.(1)) |]

let tri_index n =
  n mod dims.(0),
  (n/ dims.(0)) mod  dims.(1),
  n / (dims.(0) * dims.(1))

let test3 n =
  let i, j, k = tri_index n in
  let initial = t3.{i,j,k}  in
  t3.{i,j,k} <- -1.;
  let different = t3.{i,j,k} <> initial in
  let t3' = Array3.change_layout t3 fortran in
  t3'.{ k + 1, j + 1, i + 1 } <- initial;
  if different && initial = t3.{i, j, k} then None
    else Some [|i;j;k|]


;;
report "Rank-3 array test" test3
;;
