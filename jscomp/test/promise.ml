type t

external catch : t -> 'a -> 'b = "catch" [@@bs.send]

let f p = catch p 3

class type promise =
  object
    method _then : 'a -> 'b

    method catch : 'a -> 'b
  end[@bs]

external new_promise : unit -> promise Js.t = "Promise"
  [@@bs.new] [@@bs.module "sys-bluebird"]

let () =
  let p = new_promise () in
  (p##_then (fun x -> x + 3))##catch (fun reason -> reason)

let u = [%bs.obj {_then= 3; catch= 32}]
let uu = [%bs.obj {_'x= 3}]
let hh = uu##_'x
