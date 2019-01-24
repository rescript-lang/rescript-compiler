
(* Check that the warning 59 (assignment to immutable value) does not
   trigger on those examples *)
let a = Lazy.force (lazy "a")
let b = Lazy.force (lazy 1)
let c = Lazy.force (lazy 3.14)
let d = Lazy.force (lazy 'a')
let e = Lazy.force (lazy (fun x -> x+1))
let rec f (x:int) : int = g x and g x = f x
let h = Lazy.force (lazy f)
let i = Lazy.force (lazy g)
let j = Lazy.force (lazy 1L)
let k = Lazy.force (lazy (1,2))
let l = Lazy.force (lazy [|3.14|])
let m = Lazy.force (lazy (Sys.opaque_identity 3.14))
let n = Lazy.force (lazy None)

(* Check that obviously wrong code is reported *)
let o = (1,2)
let p = fun x -> x
let q = 3.14
let r = 1

let () =
  Obj.set_field (Obj.repr o) 0 (Obj.repr 3);
  Obj.set_field (Obj.repr p) 0 (Obj.repr 3);
  Obj.set_field (Obj.repr q) 0 (Obj.repr 3);
  Obj.set_field (Obj.repr r) 0 (Obj.repr 3)

let set v =
  Obj.set_field (Obj.repr v) 0 (Obj.repr 3)
  [@@inline]

let () =
  set o

(* Sys.opaque_identity hide all information and shouldn't warn *)

let opaque = Sys.opaque_identity (1,2)
let set_opaque =
  Obj.set_field
    (Obj.repr opaque)
    0
    (Obj.repr 3)
