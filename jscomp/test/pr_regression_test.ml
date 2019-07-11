let a =
  let v = ref 3 in
  let action () = incr v in
  let f h = (fun x y -> h x y) (action () ; 3) in
  ignore @@ f ( + ) ;
  ignore @@ f ( + ) ;
  !v

let b =
  let v = ref 3 in
  let action () = incr v in
  let f h = (fun x y -> h x y) (action () ; 3) in
  ignore @@ f ( + ) ;
  ignore @@ f ( + ) ;
  !v

let c =
  let v = ref 3 in
  let action () = incr v in
  let f h = (fun x y -> h x y) 2 (action () ; 3) in
  ignore @@ f ( + ) ;
  ignore @@ f ( + ) ;
  !v

let d =
  let v = ref 3 in
  let action () = incr v in
  let f h g =
    (fun x y -> h x y)
      (let v = 3 in
       action () ; v * v) in
  ignore @@ f ( + ) 3 ;
  ignore @@ f ( + ) 3 ;
  !v

(* ;; Printf.printf "%d%d%d%d\n" a b c d *)

;;
Mt.from_pair_suites __MODULE__
  Mt.[("partial", fun _ -> Eq ((5, 5, 5, 5), (a, b, c, d)))]
