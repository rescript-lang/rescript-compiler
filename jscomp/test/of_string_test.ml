

let suites = Mt.[
  "string_of_float_1", (fun  _-> 
    Eq("10.", string_of_float 10.)
                       );
  "string_of_int", (fun _ -> 
    Eq("10", string_of_int 10));
  (* Note that 
     [valid_float_lexem] use Range pattern, so we have to use 
     char as int now to make it passes the test 
   *)
  "valid_float_lexem", (fun _ -> 
                       Eq ("10.", valid_float_lexem "10"))
]

;; Mt.from_pair_suites __MODULE__ suites
