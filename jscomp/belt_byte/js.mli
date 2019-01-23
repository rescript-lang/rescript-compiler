type 'a null

type 'a undefined

val toOpt : 'a null -> 'a option

val nullToOption : 'a null -> 'a option

val undefinedToOption : 'a undefined -> 'a option

val fromOpt : 'a option -> 'a undefined

val undefined : 'a undefined

val null : 'a null

val empty : 'a null

val log : 'a -> unit

module Undefined : sig
  type 'a t = 'a undefined
  
  val return : 'a -> 'a t
  
  val empty : 'a t
  
  val fromOpt : 'a option -> 'a t
  
  val toOption : 'a t -> 'a option
end

module Null : sig
  type 'a t = 'a null
  
  val return : 'a -> 'a t
  
  val getUnsafe : 'a t -> 'a
  
  val fromOpt : 'a option -> 'a t
  
  val toOption : 'a t -> 'a option
end

module Exn : sig
  val raiseError : string -> 'a
end
