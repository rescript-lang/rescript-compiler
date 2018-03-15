type inner =
  | P of int
  | S of int
    
  type outer =
  | A of inner
  | B of inner
  | C of inner
  
  let f = function
  (* These cause unreachable code *)
  | A P a -> a + a
  | A S a -> a - a
  (* These don't, because there's commonality between them *)
  | B P a | B S a | C P a | C S a -> a * a