


(*
type process 

external on : process -> 
  [
  `beforeExit 
  | `exit 
  ] ->  unit Js.fn -> unit = "on" [@@bs.send]


external p : process = "process" [@@bs.val]


external on_hi : process -> 
  [
  `hello
  | `xx
  ] ->  (unit*unit) Js.fn -> unit = "on" [@@bs.send]

type 'a t 

external (!) : 'a t -> 'a = "identity"

let f x = 
  !x # hey 3  + !x # v

let () = 
  on p `exit (Js.Internal.fn_mk0 (fun _ -> prerr_endline "hello world"));
  on_hi p `xx (Js.Internal.fn_mk1 (fun _ -> prerr_endline "hello world"))

*)

let h0 x = x () [@bs]
(* {[
     function h0 (x){
         return x ()
       }
   ]}
*)
let h00 x =  x () [@bs]

let h1 x = (fun y -> x y [@bs]) (* weird case *)
(*
bucklescript$bsc -bs-syntax-only -dsource -bs-eval 'let h1 x = fun y -> x y'
let h1 x y = x y
*)
let h10 x = x 3 [@bs]

let h30 x = fun [@bs] a ->  x 3 3 a [@bs]
let h33 x =  x 1 2 3 [@bs]
let h34 x = (x 1 2 3 [@bs]) 4


let ocaml_run = fun[@bs] b c -> (fun [@bs] x y z -> x + y + z) 1  b c[@bs]

let a0 =  (fun [@bs]  () -> Js.log "hi")
let a1 () =  (fun[@bs] x -> x )
let a2 =  (fun[@bs] x y -> x + y)
let a3 =  (fun [@bs] x y z -> x + y + z )
(* let a4 = Js.Internal.fn_mk4 (fun x y z -> let u = x * x + y * y + z * z in fun d -> u + d) *)

(* let a44 = Js.Internal.fn_mk4 (fun x y z d -> let u = x * x + y * y + z * z in  u + d) *)

(* let b44 () = Js.Internal.fn_mk4 (fun x y z d -> (x,y,z,d)) *)
(* polymoprhic restriction *)

let test_as : ('a -> 'a)  -> (_ as 'b)  ->  'b = List.map

let xx : unit -> ( _ -> unit [@bs]) = fun () -> fun [@bs] _ -> Js.log 3 


(* let test_hihi = hihi _ [@bs] *)

