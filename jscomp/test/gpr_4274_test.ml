module N = struct 
  type t = { x : int}
end

module type X = sig 
  type 'a f = {i : 'a }  
  val forEach : 'a array ->  ('a -> unit) f -> unit [@bs]
end  

(* type annotation here interferes.. *)
let f (module X : X) (xs : N.t array) =
  X.forEach xs ({ X.i = fun x -> Js.log x.x} ) [@bs]


;; Belt.List.forEachU [{N.x=3}] (fun[@bs] x -> Js.log x.x)


module Foo = struct type record = {
                      foo: string;} end
let bar = [|{ Foo.foo = (("bar")[@reason.raw_literal "bar"]) }|]

let _ = Belt.Array.mapU bar ((fun[@bs ] b  -> b.foo))