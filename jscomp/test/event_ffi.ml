


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

let h0 x = Js.Internal.fn_run0 x 
(* {[
     function h0 (x){
         return x ()
       }
   ]}
*)
let h00 x = Js.Internal.fn_run0 x ()

let h1 x = Js.Internal.fn_run1 x 
let h10 x = Js.Internal.fn_run1 x 3  

let h30 x = Js.Internal.fn_run3 x 3 3   
let h33 x = Js.Internal.fn_run3 x 1 2 3
let h34 x = Js.Internal.fn_run3 x 1 2 3 4


let ocaml_run = Js.Internal.fn_run3 (Js.Internal.fn_mk3 (fun x y z -> x + y + z)) 1 

let a0 = Js.Internal.fn_mk0 (fun  _ -> Js.log "hi")
let a1 () = Js.Internal.fn_mk1 (fun x -> x )
let a2 = Js.Internal.fn_mk2 (fun x y -> x + y)
let a3 = Js.Internal.fn_mk3 (fun x y z -> x + y + z )
let a4 = Js.Internal.fn_mk4 (fun x y z -> let u = x * x + y * y + z * z in fun d -> u + d)

let a44 = Js.Internal.fn_mk4 (fun x y z d -> let u = x * x + y * y + z * z in  u + d)

let b44 () = Js.Internal.fn_mk4 (fun x y z d -> (x,y,z,d))
(* polymoprhic restriction *)

let test_as : ('a -> 'a)  -> (_ as 'b)  ->  'b = List.map

let xx : unit -> ( _ -> unit [@bs]) = fun () -> fun [@bs] _ -> Js.log 3 


(* let test_hihi = hihi _ [@bs] *)

