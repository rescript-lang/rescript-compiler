


(*
type process 

external on : process -> 
  [
  `beforeExit 
  | `exit 
  ] ->  unit Js_fn.t -> unit = "on" [@@bs.send]


external p : process = "process" [@@bs.val]


external on_hi : process -> 
  [
  `hello
  | `xx
  ] ->  (unit*unit) Js_fn.t -> unit = "on" [@@bs.send]

type 'a t 

external (!) : 'a t -> 'a = "identity"

let f x = 
  !x # hey 3  + !x # v

let () = 
  on p `exit (Js_fn.mk0 (fun _ -> prerr_endline "hello world"));
  on_hi p `xx (Js_fn.mk1 (fun _ -> prerr_endline "hello world"))

*)

let h0 x = Js_fn.run0 x 
(* {[
     function h0 (x){
         return x ()
       }
   ]}
*)
let h00 x = Js_fn.run0 x ()

let h1 x = Js_fn.run1 x 
let h10 x = Js_fn.run1 x 3  

let h30 x = Js_fn.run3 x 3 3   
let h33 x = Js_fn.run3 x 1 2 3
let h34 x = Js_fn.run3 x 1 2 3 4


let ocaml_run = Js_fn.run3 (Js_fn.mk3 (fun x y z -> x + y + z)) 1 

let a0 = Js_fn.mk0 (fun  _ -> Js.log "hi")
let a1 = Js_fn.mk1 (fun x -> x )
let a2 = Js_fn.mk2 (fun x y -> x + y)
let a3 = Js_fn.mk3 (fun x y z -> x + y + z )
let a4 = Js_fn.mk4 (fun x y z -> let u = x * x + y * y + z * z in fun d -> u + d)

let a44 = Js_fn.mk4 (fun x y z d -> let u = x * x + y * y + z * z in  u + d)

let b44 = Js_fn.mk4 (fun x y z d -> (x,y,z,d))
