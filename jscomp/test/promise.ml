[@@@bs.config{bs_class_type }]
type t

external catch : t -> 'a -> 'b = "catch" [@@bs.send]

let f p = 
  catch p 3

class type promise =
  object
    method then_ : 'a -> 'b
    method catch : 'a -> 'b
  end [@fn]

external new_promise : unit -> promise Js.t = 
  "Promise" [@@bs.new] [@@bs.module "sys-bluebird"]

let () =
  let p = new_promise() in
  (p##then_(fun x -> x + 3))##catch_(fun reason -> reason)


let u = 
  [%bs.obj{ then_ = 3 ; 
    catch  = 32
  }]


let uu = [%bs.obj{
  _'x_ = 3
}]


let hh = uu##_'x_
