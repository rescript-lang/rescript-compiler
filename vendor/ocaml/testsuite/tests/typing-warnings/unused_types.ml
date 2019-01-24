module Unused : sig
end = struct
  type unused = int
end
;;

module Unused_nonrec : sig
end = struct
  type nonrec used = int
  type nonrec unused = used
end
;;

module Unused_rec : sig
end = struct
  type unused = A of unused
end
;;

module Unused_exception : sig
end = struct
  exception Nobody_uses_me
end
;;

module Unused_extension_constructor : sig
  type t = ..
end = struct
  type t = ..
  type t += Nobody_uses_me
end
;;

module Unused_exception_outside_patterns : sig
  val falsity : exn -> bool
end = struct
  exception Nobody_constructs_me
  let falsity = function
    | Nobody_constructs_me -> true
    | _ -> false
end
;;

module Unused_extension_outside_patterns : sig
  type t = ..
  val falsity : t -> bool
end = struct
  type t = ..
  type t += Nobody_constructs_me
  let falsity = function
    | Nobody_constructs_me -> true
    | _ -> false
end
;;

module Unused_private_exception : sig
  type exn += private Private_exn
end = struct
  exception Private_exn
end
;;

module Unused_private_extension : sig
  type t = ..
  type t += private Private_ext
end = struct
  type t = ..
  type t += Private_ext
end
;;

module Pr7438 : sig
end = struct
  module type S = sig type t = private [> `Foo] end
  module type X = sig type t = private [> `Foo | `Bar] include S with type t := t end
end;;
