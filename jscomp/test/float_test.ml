



let epsilon_float =
  Int64.float_of_bits 0x3C_B0_00_00_00_00_00_00L

let results = 
  Array.append 
    [| log10 2., 0.301029995663981198;
       ldexp 1. 6, 64.; (* 1. * 2. ^ 6*)
       ldexp 1. 5, 32.;
       ldexp (1.e-5) 1024, 1.79769313486231605e+303;
       ldexp 1. (-1024), 5.56268464626800346e-309;
       hypot 3. 4. , 5. ; 
       hypot 4. 3. , 5. ; 
       hypot 5. 12., 13.;
       hypot 12. 5., 13.
    |]
    (let a,b = frexp 12.0 in 
     [| a , 0.75 ; 
        float_of_int b, 4.
     |]
    )

let from_pairs ps = 
  ps 
  |> Array.mapi (fun i (a,b) -> Printf.sprintf "pair %d" i , (fun _ -> Mt.Approx(a,b)))
  |> Array.to_list

;; Mt.from_pair_suites __FILE__ @@ Mt.[
"mod_float" , (fun _ ->  
      Approx( (mod_float 3.2 0.5), 0.200000000000000178 ))
] @ 
from_pairs results
