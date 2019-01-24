type t = T : t;;

module M : sig
  type free = < bar : t -> unit; foo : free -> unit >
end = struct
  class free = object (self : 'self)
    method foo self = ()
    method bar T = self#foo self
  end
end;;
[%%expect{|
type t = T : t
module M : sig type free = < bar : t -> unit; foo : free -> unit > end
|}]
