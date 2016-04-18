


(*
type process 

external on : process -> 
  [
  `beforeExit 
  | `exit 
  ] ->  unit Fn.t -> unit = "on" [@@bs.send]


external p : process = "process" [@@bs.val]


external on_hi : process -> 
  [
  `hello
  | `xx
  ] ->  (unit*unit) Fn.t -> unit = "on" [@@bs.send]

type 'a t 

external (!) : 'a t -> 'a = "identity"

let f x = 
  !x # hey 3  + !x # v

let () = 
  on p `exit (Fn.mk0 (fun _ -> prerr_endline "hello world"));
  on_hi p `xx (Fn.mk1 (fun _ -> prerr_endline "hello world"))

*)

let h0 x = Fn.run0 x 
(* {[
     function h0 (x){
         return x ()
       }
   ]}
*)
let h00 x = Fn.run0 x ()

let h1 x = Fn.run1 x 
let h10 x = Fn.run1 x 3  

let h30 x = Fn.run3 x 3 3   
let h33 x = Fn.run3 x 1 2 3
let h34 x = Fn.run3 x 1 2 3 4


let ocaml_run = Fn.run3 (Fn.mk3 (fun x y z -> x + y + z)) 1 

let a0 = Fn.mk0 (fun  _ -> Js.log "hi")
let a1 = Fn.mk1 (fun x -> x )
let a2 = Fn.mk2 (fun x y -> x + y)
let a3 = Fn.mk3 (fun x y z -> x + y + z )
let a4 = Fn.mk4 (fun x y z -> let u = x * x + y * y + z * z in fun d -> u + d)

let a44 = Fn.mk4 (fun x y z d -> let u = x * x + y * y + z * z in  u + d)

let b44 = Fn.mk4 (fun x y z d -> (x,y,z,d))
