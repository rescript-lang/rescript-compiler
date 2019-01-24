module type S = sig

   type o1 = < bar : int; foo : int >
   type o2 = private < foo : int; .. >

   type v1 = T of o1
   type v2 = T of o2

 end

 module M = struct

   type o1 = < bar : int; foo : int >
   type o2 = o1

   type v1 = T of o1
   type v2 = v1 = T of o2

 end

 module F(X : S) = struct

   type 'a wit =
   | V1 : string -> X.v1 wit
   | V2 : int -> X.v2 wit

   let f : X.v1 wit -> unit = function V1 s -> print_endline s

 end [@@warning "+8"] [@@warnerror "+8"]

 module N = F(M)

 let () = N.f (N.V2 0)
