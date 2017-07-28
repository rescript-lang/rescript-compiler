type 't a = [ `A ]
type 't wrap = 't constraint 't = [> 't wrap a ]
type t = t a wrap

module T = struct
  let foo : 't wrap -> 't wrap -> unit = fun _ _ -> ()
  let bar : ('a a wrap as 'a) = `A
end

module Good : sig
  val bar: t
  val foo: t -> t -> unit
end = T

module Bad : sig
  val foo: t -> t -> unit
  val bar: t
end = T
