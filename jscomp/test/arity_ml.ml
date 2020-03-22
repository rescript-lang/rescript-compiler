[@@@warning "A-61-42-40"]
let o = object [@bs]
  method hi x y = x + y  
end
(* Error (warning 61): This primitive declaration uses type Js_OO.Callback.arity3, which is unannotated and
unboxable. The representation of such types may change in future
versions. You should annotate the declaration of Js_OO.Callback.arity3 with [@@boxed]
or [@@unboxed]. *)

(* let h o = 
  (o 1 2 :unit);
  o ~x:"x" 2 ;   *)

(* let u obj =
  (obj##hi 1 2   : unit);
  obj##hi ~x:"x" 2
 *)

(* let u obj = 
  (obj##hi ~x:"x" 2 : unit);
  (obj##hi 1  ~x:2  : unit) *)
(* let h (o : x:int -> int -> unit) = 
  o ~x:1 2    ;  
  o 1 2 ;  *)


(* let h u = 
  let m = u##hi in 
  m 1 2 [@bs]

;; h (object [@bs] method hi x y =x + y end )

Error: This expression has type < hi : (int -> int -> int [@bs.meth]) > Js.t
       but an expression was expected of type
         < hi : (int -> int -> 'a [@bs]); .. > Js.t
       Types for method hi are incompatible *)