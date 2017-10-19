(* type target = <
  value : string Js.undefined [@bs.get];
> Js.t *)

type 'node t = <
  target : 'node Js.undefined [@bs.get];
  keyCode : int [@bs.get];
  preventDefault : unit -> unit [@bs.meth];
  stopPropagation : unit -> unit [@bs.meth];
> Js.t

type 'node cb = 'node t -> unit [@bs]

type options = bool (* false | true (* TODO:  Define a javascript record as another option *) *)


type popstateEvent = <
> Js.t

type popstateCb = popstateEvent -> unit [@bs]
