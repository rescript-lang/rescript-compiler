(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



exception Off

type on_switch = {
  mutable hooks : (unit -> unit Lwt.t) list;
}

type state =
  | St_on of on_switch
  | St_off

type t = { mutable state : state }

let create () = { state = St_on { hooks = [] } }

let is_on switch =
  match switch.state with
  | St_on _ -> true
  | St_off -> false

let check = function
  | Some{ state = St_off } -> raise Off
  | Some {state = St_on _} | None -> ()

let add_hook switch hook =
  match switch with
  | Some { state = St_on os } ->
    os.hooks <- hook :: os.hooks
  | Some { state = St_off } ->
    raise Off
  | None ->
    ()

let add_hook_or_exec switch hook =
  match switch with
  | Some { state = St_on os } ->
    os.hooks <- hook :: os.hooks;
    Lwt.return_unit
  | Some { state = St_off } ->
    hook ()
  | None ->
    Lwt.return_unit

let turn_off switch =
  match switch.state with
  | St_on { hooks = hooks } ->
    switch.state <- St_off;
    Lwt.join (List.map (fun hook -> Lwt.apply hook ()) hooks)
  | St_off ->
    Lwt.return_unit

let with_switch fn =
  let switch = create () in
  Lwt.finalize
    (fun () -> fn switch)
    (fun () -> turn_off switch)
