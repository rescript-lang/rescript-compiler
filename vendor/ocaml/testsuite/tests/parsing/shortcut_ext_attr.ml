
(* Expressions *)
let () =
  let%foo[@foo] x = 3
  and[@foo] y = 4 in
  (let module%foo[@foo] M = M in ()) ;
  (let open%foo[@foo] M in ()) ;
  (fun%foo[@foo] x -> ()) ;
  (function%foo[@foo] x -> ()) ;
  (try%foo[@foo] () with _ -> ()) ;
  (if%foo[@foo] () then () else ()) ;
  while%foo[@foo] () do () done ;
  for%foo[@foo] x = () to () do () done ;
  () ;%foo () ;
  assert%foo[@foo] true ;
  lazy%foo[@foo] x ;
  object%foo[@foo] end ;
  begin%foo[@foo] 3 end ;
  new%foo[@foo] x ;

  match%foo[@foo] () with
  (* Pattern expressions *)
  | lazy%foo[@foo] x -> ()
  | exception%foo[@foo] x -> ()


(* Class expressions *)
class x =
  fun[@foo] x ->
  let[@foo] x = 3 in
  object[@foo]
    inherit[@foo] x
    val[@foo] x = 3
    val[@foo] virtual x : t
    val![@foo] mutable x = 3
    method[@foo] x = 3
    method[@foo] virtual x : t
    method![@foo] private x = 3
    initializer[@foo] x
  end

(* Class type expressions *)
class type t =
  object[@foo]
    inherit[@foo] t
    val[@foo] x : t
    val[@foo] mutable x : t
    method[@foo] x : t
    method[@foo] private x : t
    constraint[@foo] t = t'
  end

(* Type expressions *)
type t =
  (module%foo[@foo] M)

(* Module expressions *)
module M =
  functor[@foo] (M : S) ->
    (val[@foo] x)
    (struct[@foo] end)

(* Module type expression *)
module type S =
  functor[@foo] (M:S) ->
    (module type of[@foo] M) ->
    (sig[@foo] end)

(* Structure items *)
let%foo[@foo] x = 4
and[@foo] y = x

type%foo[@foo] t = int
and[@foo] t = int
type%foo[@foo] t += T

class%foo[@foo] x = x
class type%foo[@foo] x = x
external%foo[@foo] x : _  = ""
exception%foo[@foo] X

module%foo[@foo] M = M
module%foo[@foo] rec M : S = M
and[@foo] M : S = M
module type%foo[@foo] S = S

include%foo[@foo] M
open%foo[@foo] M

(* Signature items *)
module type S = sig
  val%foo[@foo] x : t
  external%foo[@foo] x : t = ""

  type%foo[@foo] t = int
  and[@foo] t' = int
  type%foo[@foo] t += T

  exception%foo[@foo] X

  module%foo[@foo] M : S
  module%foo[@foo] rec M : S
  and[@foo] M : S
  module%foo[@foo] M = M

  module type%foo[@foo] S = S

  include%foo[@foo] M
  open%foo[@foo] M

  class%foo[@foo] x : t
  class type%foo[@foo] x = x

end
