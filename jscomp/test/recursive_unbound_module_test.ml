


module type S =
sig
  module M : sig val f : unit -> unit end
end

module Make (X : sig end) =
struct
  module M = struct let f () = () end
end

module rec A : sig end =
struct
end
and B : S =
struct
  module C = Make(A)
  include C
end