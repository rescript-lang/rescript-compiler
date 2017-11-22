let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let throw loc x  = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id),
     (fun _ -> Mt.ThrowAny(x))) :: !suites



type a = 
  | A0 
  | A1
  | A2
and  b =  
  | B0 
  | B1 [@bs.as 3]
  | B2 
and c = [
  | `c0 
  | `c1 
  | `c2 
  ]   
[@@bs.deriving { jsConverter =  newType  }]


(* ;; aFromJs (Obj.magic 3) *)
let ()  = throw __LOC__ (fun _ -> ignore @@ aFromJs (Obj.magic 3))
let () = throw __LOC__ (fun _ -> ignore @@ bFromJs (Obj.magic 2))
let () = throw __LOC__ (fun _ -> ignore @@ cFromJs (Obj.magic 33))
(* ;; Js.log2    *)

;; Mt.from_pair_suites __FILE__ !suites