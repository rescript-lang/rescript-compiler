let of_string = 
  [| (0, "0"); (3, "03"); (-3, "-03"); (-63, "-0x3f"); (-31, "-0x1f");
     (47, "0X2f"); (11, "0O13"); (8, "0o10"); (3, "0b11"); (1, "0b01");
     (0, "0b00"); (-3, "-0b11"); (-5, "-0B101"); (332, "0332"); (-32, "-32");
     (-4294967295, "-0xffff_ffff");
     (-1, "0xffff_ffff")
  |]

(* let float_of_string =  *)
(*   [| "nan", nan ; *)
(*      "infinity", infinity; *)
(*      "0.", 0. *)
(*   |] *)

let from_float_of_string xs = 
  xs 
  |> Array.mapi (fun i (a,b) -> 
      string_of_float
    )

let from_of_string xs = 
  of_string 
  |>  Array.mapi (fun i (a,b) -> 
      (Printf.sprintf "of_string %L" i), fun _ -> Mt.Eq(int_of_string b,a ))
  |> Array.to_list 
let u v = Printf.sprintf "%33d" v 
let to_str s = int_of_string s
let v = Printf.sprintf "%3d" 3333

;; Mt.from_pair_suites __FILE__ @@ 
    (from_of_string of_string @
     ["isnan_of_string", (fun _ -> 
          Mt.Eq (true,classify_float( float_of_string "nan") = FP_nan))] @
     (let pairs =
       [| FP_infinite, "infinity";
          FP_infinite, "+infinity"; 
          FP_infinite, "-infinity";
          FP_zero, "0";
          FP_zero, "0."
       |] 
     in 
     pairs 
     |> Array.mapi 
       (fun i (a,b) ->
          (Printf.sprintf "infinity_of_string %d" i ),
          (fun _ -> Mt.Eq(a,
                          classify_float @@ float_of_string b))) 
     |> Array.to_list ) @ 
     [ "throw", (fun _ -> Mt.ThrowAny (fun _ -> ignore @@ float_of_string ""))]
    )
