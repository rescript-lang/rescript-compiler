

type t = <
  length : int [@bs.get];
  clear : unit -> unit [@bs.meth];
  key : int -> string [@bs.meth];
  getItem : string -> string [@bs.meth];
  removeItem : string -> unit [@bs.meth];
  setItem : string -> string -> unit [@bs.meth];
> Js.t

let length window = match Js.Undefined.to_opt window##localStorage with
  | None -> None
  | Some localStorage -> Some (localStorage##length)


let clear window = match Js.Undefined.to_opt window##localStorage with
  | None -> None
  | Some localStorage -> Some (localStorage##clear ())


let key window idx = match Js.Undefined.to_opt window##localStorage with
  | None -> None
  | Some localStorage -> Some (localStorage##key idx)


let getItem window key = match Js.Undefined.to_opt window##localStorage with
  | None -> None
  | Some localStorage ->
    try Some (localStorage##getItem key)
    with _ -> None


let removeItem window key = match Js.Undefined.to_opt window##localStorage with
  | None -> None
  | Some localStorage -> Some (localStorage##removeItem key)


let setItem window key value = match Js.Undefined.to_opt window##localStorage with
  | None -> None
  | Some localStorage -> Some (localStorage##setItem key value)
