
let v = ref 0 

(** print 0,1,2,3,4,5,6,7,8*)
;; while print_endline (string_of_int !v); incr v ; !v < 10 do 
  ignore ();
done
let rec fib = function
  | 0 | 1 -> 1 
  | n -> fib (n - 1) + fib (n - 2)

let x = ref 3 

;;  while let y = ref 3 in
     print_endline (string_of_int !x ) ; incr y;  incr x; fib !x + fib !x < 20 do 
  print_endline (string_of_int 3 )
  done
