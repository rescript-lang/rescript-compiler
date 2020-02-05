

external unsafeInvariantApply : 'a -> 'a = "#full_apply"


let f1 x = unsafeInvariantApply (x ())


let f2 x y = unsafeInvariantApply (x y ())


let f3 fn a b = 
  unsafeInvariantApply (Obj.magic fn a b)
(* This compiles to fn (a,b) is that Obj.magic 
  is an external
*)

let id : (_ -> _ -> _) -> (_ -> _  -> _)= Obj.magic

let f4 fn a b = 
  unsafeInvariantApply 
  (id fn a b)

external fn : int -> int -> int = "f"
[@@bs.val]  

let f5 a b = 
  (id fn a b) 

let f6 a b = 
  unsafeInvariantApply (id fn a b)   

type 'h h = C of 'h [@@unboxed]

type 'a b = { b : 'a } [@@unboxed]

let f7 a b =
  unsafeInvariantApply 
  ((let C f = C fn in  f) a b )


let f8 f a b =
  unsafeInvariantApply 
    ((let C f = f in  f) ~a b )

let f9 f a b =
  unsafeInvariantApply 
    (f.b a b )


let poly_f  = fun[@bs] a b c -> 
  [%debugger]; (* not inline *)
  (a,b,c)    


let rec rec_f  = fun [@bs] a b c -> 
  if a = 0 then  rec_f a b (c+1) [@bs] + 3
  else rec_f (a-1) (b-2) c  [@bs]
(* TODO: *)
(* let label f = 
  f ~a:1 ~b:2 ~c:2 [@bs]   *)