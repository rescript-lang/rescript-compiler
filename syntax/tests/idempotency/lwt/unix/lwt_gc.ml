(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(* [Lwt_sequence] is deprecated – we don't want users outside Lwt using it.
   However, it is still used internally by Lwt. So, briefly disable warning 3
   ("deprecated"), and create a local, non-deprecated alias for
   [Lwt_sequence] that can be referred to by the rest of the code in this
   module without triggering any more warnings. *)
[@@@ocaml.warning "-3"]
module Lwt_sequence = Lwt_sequence
[@@@ocaml.warning "+3"]

let ensure_termination t =
  if Lwt.state t = Lwt.Sleep then begin
    let hook =
      Lwt_sequence.add_l (fun _ -> t) Lwt_main.exit_hooks [@ocaml.warning "-3"]
    in
    (* Remove the hook when t has terminated *)
    ignore (
      Lwt.finalize
        (fun () -> t)
        (fun () -> Lwt_sequence.remove hook; Lwt.return_unit))
  end

let finaliser f =
  (* In order not to create a reference to the value in the
     notification callback, we use an initially unset option cell
     which will be filled when the finaliser is called. *)
  let opt = ref None in
  let id =
    Lwt_unix.make_notification
      ~once:true
      (fun () ->
         match !opt with
         | None ->
           assert false
         | Some x ->
           opt := None;
           ensure_termination (f x))
  in
  (* The real finaliser: fill the cell and send a notification. *)
  (fun x ->
     opt := Some x;
     Lwt_unix.send_notification id)

let finalise f x =
  Gc.finalise (finaliser f) x

(* Exit hook for a finalise_or_exit *)
let foe_exit f called weak () =
  match Weak.get weak 0 with
  | None ->
    (* The value has been garbage collected, normally this point
       is never reached *)
    Lwt.return_unit
  | Some x ->
    (* Just to avoid double finalisation *)
    Weak.set weak 0 None;
    if !called then
      Lwt.return_unit
    else begin
      called := true;
      f x
    end

(* Finaliser for a finalise_or_exit *)
let foe_finaliser f called hook =
  finaliser
    (fun x ->
       (* Remove the exit hook, it is not needed anymore. *)
       Lwt_sequence.remove hook;
       (* Call the real finaliser. *)
       if !called then
         Lwt.return_unit
       else begin
         called := true;
         f x
       end)

let finalise_or_exit f x =
  (* Create a weak pointer, so the exit-hook does not keep a reference
     to [x]. *)
  let weak = Weak.create 1 in
  Weak.set weak 0 (Some x);
  let called = ref false in
  let hook =
    Lwt_sequence.add_l (foe_exit f called weak) Lwt_main.exit_hooks
      [@ocaml.warning "-3"]
  in
  Gc.finalise (foe_finaliser f called hook) x
