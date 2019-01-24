module A = struct
 module type A_S = sig
 end

 type t = (module A_S)
end

module type S = sig type t end

let f (type a) (module X : S with type t = a) = ()

let _ = f (module A) (* ok *)

module A_annotated_alias : S with type t = (module A.A_S) = A

let _ = f (module A_annotated_alias) (* ok *)
let _ = f (module A_annotated_alias : S with type t = (module A.A_S)) (* ok *)

module A_alias = A
module A_alias_expanded = struct include A_alias end

let _ = f (module A_alias_expanded : S with type t = (module A.A_S)) (* ok *)
let _ = f (module A_alias_expanded) (* ok *)

let _ = f (module A_alias : S with type t = (module A.A_S)) (* doesn't type *)
let _ = f (module A_alias) (* doesn't type either *)
