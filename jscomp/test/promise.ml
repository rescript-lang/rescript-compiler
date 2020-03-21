
type t

external catch : t -> 'a -> 'b = "catch" [@@bs.send]

let f p = 
  catch p 3

class type ['b] promise =
  object [@bs]
    method _then : 'a -> 'b promise Js.t
    method catch : 'a -> 'b
  end 

external new_promise : unit -> _ promise Js.t = 
  "Promise" [@@bs.new] [@@bs.module "sys-bluebird"]

let () =
  let p = new_promise() in
  (p##_then(fun x -> x + 3))##catch(fun reason -> reason)


let u = 
  [%bs.obj{ _then = 3 ; 
    catch  = 32
  }]


let uu = [%bs.obj{
  _'x = 3
}]


let hh = uu##_'x
