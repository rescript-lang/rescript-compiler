

let ff v = string_of_float v 
let f v = string_of_int v 

;; Mt.from_pair_suites __MODULE__ [
  __LOC__ , (fun _ -> Eq(ff infinity, "inf"));
 __LOC__ , (fun _ -> Eq(ff neg_infinity, "-inf"))
]
