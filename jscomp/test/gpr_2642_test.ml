type t =
  Pident of string
| Pdot of t * string * int
| Papply of t * t


let rec isfree id = function
Pident id' ->  id = id'
| Pdot(p, s, pos) ->
  isfree id p 
  (* if isfree id p then
    true
  else false *)
| Papply(p1, p2) -> isfree id p1 || isfree id p2
