
(* module Mt = Mt_mock *)


let epsilon_float =
  Int64.float_of_bits 0x3C_B0_00_00_00_00_00_00L

;; Mt.from_pair_suites __FILE__ [
"mod_float" , (fun _ ->  
      Approx( (mod_float 3.2 0.5), 0.200000000000000178 ))
]
