open Mt 

;; from_suites "Closure" [
"partial", (fun _ -> 
  assert_equal Test_google_closure.(a,b,c)
    ("3", 101, [|1;2;3|])
           )
]
