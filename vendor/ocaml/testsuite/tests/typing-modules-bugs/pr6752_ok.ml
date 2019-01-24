(* Adding a type annotation is sufficient to make typing go through *)

module Common0 =
 struct
   type msg = Msg

   let handle_msg = ref (function _ -> failwith "Unable to handle message")
   let extend_handle f =
   let old = !handle_msg in
   handle_msg := f old

   let q : msg Queue.t = Queue.create ()
   let add msg = Queue.add msg q
   let handle_queue_messages () = Queue.iter !handle_msg q
 end

let q' : Common0.msg Queue.t = Common0.q

module Common =
 struct
   type msg = ..

   let handle_msg = ref (function _ -> failwith "Unable to handle message")
   let extend_handle f =
   let old = !handle_msg in
   handle_msg := f old

   let q : msg Queue.t = Queue.create ()
   let add msg = Queue.add msg q
   let handle_queue_messages () = Queue.iter !handle_msg q
 end

module M1 =
 struct
   type Common.msg += Reload of string | Alert of string

   let handle fallback = function
     Reload s -> print_endline ("Reload "^s)
   | Alert s -> print_endline ("Alert "^s)
   | x -> fallback x

   let () = Common.extend_handle handle
   let () = Common.add (Reload "config.file")
   let () = Common.add (Alert "Initialisation done")
 end
