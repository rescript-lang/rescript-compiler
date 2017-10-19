

type t = <
  length : int [@bs.get];
  back : unit -> unit [@bs.meth];
  forward : unit -> unit [@bs.meth];
  go : int -> unit [@bs.meth];
  pushState : Js.Json.t -> string -> string -> unit [@bs.meth];
  replaceState : Js.Json.t -> string -> string -> unit [@bs.meth];
  state : Js.Json.t [@bs.get];
> Js.t


let length window = match Js.Undefined.to_opt window##history with
  | None -> -1
  | Some history -> history##length

let back window = match Js.Undefined.to_opt window##history with
  | None -> ()
  | Some history -> history##back

let forward window = match Js.Undefined.to_opt window##history with
  | None -> ()
  | Some history -> history##forward

let go window to' = match Js.Undefined.to_opt window##history with
  | None -> ()
  | Some history -> history##go to'

let pushState window state title url = match Js.Undefined.to_opt window##history with
  | None -> ()
  | Some history -> history##pushState state title url

let replaceState window state title url = match Js.Undefined.to_opt window##history with
  | None -> ()
  | Some history -> history##replaceState state title url

let state window = match Js.Undefined.to_opt window##history with
  | None -> Js.Undefined.empty
  | Some history -> history##state
