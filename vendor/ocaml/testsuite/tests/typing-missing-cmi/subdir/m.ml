type a = int
type b = a

module Foo(X : sig end) = struct type t = T end
