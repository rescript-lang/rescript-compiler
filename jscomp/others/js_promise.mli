type (+'a, +'e) rejectable
type never

type +'a t = ('a, never) rejectable
type +'a promise = 'a t



(* Note: does not allow rejection. *)
(* Note, in Repromise, there is:
     val new_ : unit -> 'a t * ('a -> unit) *)
val make : (resolve:('a -> unit) -> unit) -> 'a t

val resolve : 'a -> 'a t

val then_ : ('a -> 'b t) -> 'a t -> 'b t

val map : ('a -> 'b) -> 'a t -> 'b t

(* Note: in Repromise, these operate on lists instead of arrays. *)
val all : ('a t) array -> ('a array) t
(* Note: all2, ..., all6 can be implemented. *)

val race : ('a t) array -> 'a t

(* Note: there is no reject, catch. *)



module Rejectable :
sig
  type (+'a, +'e) t = ('a, 'e) rejectable

  val relax : 'a promise -> ('a, _) rejectable

  val new_ : unit -> ('a, 'e) rejectable * ('a -> unit) * ('e -> unit)

  val resolve : 'a -> ('a, _) rejectable

  val reject : 'e -> (_, 'e) rejectable

  val then_ :
    ('a -> ('b, 'e) rejectable) -> ('a, 'e) rejectable -> ('b, 'e) rejectable

  val map : ('a -> 'b) -> ('a, 'e) rejectable -> ('b, 'e) rejectable

  val catch :
    ('e -> ('a, 'e2) rejectable) -> ('a, 'e) rejectable -> ('a, 'e2) rejectable

  val all : (('a, 'e) rejectable) array -> ('a array, 'e) rejectable

  val race : (('a, 'e) rejectable) array -> ('a, 'e) rejectable
end



val onUnhandledException : (exn -> never) ref
