type t = < id: int > Js.t

let id = ref 0

let next () =
  id := !id + 1;
  !id
