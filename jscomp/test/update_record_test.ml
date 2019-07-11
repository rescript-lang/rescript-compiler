let suites : Mt.pair_suites ref = ref []
let test_id = ref 0

let eq loc x y =
  Js.log (x, y) ;
  incr test_id ;
  suites :=
    (loc ^ " id " ^ string_of_int !test_id, fun _ -> Mt.Eq (x, y)) :: !suites

type t =
  { a0: int
  ; a1: int
  ; a2: int
  ; a3: int
  ; a4: int
  ; a5: int (* a6 : int ; *)
            (* mutable a7 : int ; *) }

let f (x : t) =
  let y = (Obj.magic (Obj.dup (Obj.repr x)) : t) in
  {y with a0= 1}

let () =
  let v = {a0= 0; a1= 0; a2= 0; a3= 0; a4= 0; a5= 0} in
  eq __LOC__ (v.a0 + 1) (f v).a0

let () = Mt.from_pair_suites __MODULE__ !suites
