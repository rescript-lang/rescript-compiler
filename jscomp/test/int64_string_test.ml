
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 


let v = Int64.to_string 333L

let f a b =
  eq __LOC__ (Int64.to_string a) b 


;; f (-33L) "-33"  
;; f (33L) "33"
;; f Int64.min_int "-9223372036854775808"
;; f (Int64.(add min_int 100L))
  "-9223372036854775708"

;; for i = 0 to 8 do 
  eq __LOC__ (Int64.(to_string (add min_int (of_int i))))
  ("-922337203685477580" ^ string_of_int (8 - i))
done     
;; for i = 0 to 8 do 
  eq __LOC__ (Int64.(to_string (add min_int (of_int @@ 100 + i))))
  ("-922337203685477570" ^ string_of_int (8 - i))

done    
;; Mt.from_pair_suites __LOC__ !suites