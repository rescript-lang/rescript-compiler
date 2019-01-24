(* GPR#816 *)
(* This PR means that Foo(Bar).t is known to be equal to Foo(Baz).t
   when Bar is an alias for Baz, even when the definition for Foo is unknown.
   This can happen when .cmi files depend on other .cmi files not in the path
   -- a situation that is partially supported. *)

module A = M

type t1 = M.Foo(M).t
type t2 = A.Foo(A).t
