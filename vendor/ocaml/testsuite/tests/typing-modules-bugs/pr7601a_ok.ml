module type Param1 = sig
  type 'a r = [< `A of int ] as 'a
  val f : ?a:string -> string -> [ `A of _ ] r
end

module Make1 (M : Param1) = struct
  include M
  let f = f ~a:""
end

module type Param2 = sig
  type t
  type 'a r = [< `A of t ] as 'a
  val f : ?a:string -> string -> [ `A of _ ] r
end

module Make2 (M : Param2) = struct
  include M
  let f = f ~a:""
end

