let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc (x, y) =
  incr test_id ;
  suites :=
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites

let v  = [| 1 ; 2 ; 3; 3|]


let () =
  eq __LOC__  (4,Array.length v)

let () =
  eq __LOC__ (5,Js.Array2.push v 3 ); (* in Js array length can be changing .. *)
  eq __LOC__ (5, Array.length v );
  eq __LOC__ (5,Js.Array2.length v )


let () =
  eq __LOC__ (3, v.(2));
  v.(2)<-4;
  eq __LOC__ (4,v.(2)) (* should not inline *)

let () =
  while Js.Array2.length v > 0 do
    ignore @@ Js.Array2.pop v
  done;
  eq __LOC__ (0, Js.Array2.length v )


let f v =
  (match Js.Array2.pop v with
  | Some x -> Js.log "hi"
  | None -> Js.log "hi2");
  Js.log (ignore @@ Js.Array2.pop v)


let fff x =
  Array.length x >= 0

let fff2 x =
  if Array.length x >=  10 then Js.log "hi"

let fff3 x =
  if Array.length x >=  0 then 1 else 2

let fff4 x =
  if Array.length x >  0 then 1 else 2

;; eq __LOC__ (fff3 [||], 1 )
;; eq __LOC__ (fff4 [||], 2)
;; eq __LOC__ (fff4 [|1|], 1)
let () = Mt.from_pair_suites __MODULE__ !suites
