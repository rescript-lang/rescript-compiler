

type 'msg applicationCallbacks = 'msg Vdom.applicationCallbacks

type 'msg t =
  | NoCmd
  | Tagger of ('msg applicationCallbacks ref -> unit)
  | Batch of 'msg t list
  | EnqueueCall of ('msg applicationCallbacks ref -> unit)



let none = NoCmd


let batch cmds =
  Batch cmds


let call call =
  EnqueueCall call


let fnMsg fnMsg =
  let open Vdom in
  EnqueueCall (fun callbacks -> !callbacks.enqueue (fnMsg ()))


let msg msg =
  let open Vdom in
  EnqueueCall (fun callbacks -> !callbacks.enqueue msg)


let rec run callbacks =
  function
  | NoCmd -> ()
  | Tagger tagger -> tagger callbacks
  | Batch cmds -> List.fold_left (fun () cmd -> run callbacks cmd) () cmds
  | EnqueueCall cb ->
    (* let () = Js.log ("Cmd.run", "enqueue", cb) in *)
    cb callbacks



(* let wrapCallbacks func callbacks = *)
(*   let open Vdom in *)
(*   ref *)
(*     { enqueue = (fun msg -> !callbacks.enqueue (func msg)) *)
(*     } *)

let map : ('a -> 'b) -> 'a t -> 'b t = fun func cmd ->
  let open Vdom in
  Tagger
    ( fun callbacks ->
        run (wrapCallbacks func callbacks) cmd
    )
