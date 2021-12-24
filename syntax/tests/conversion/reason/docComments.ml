(** The first special comment of the file is the comment associated
     to the whole module. *)

 (** The comment for function f *)
 let f x y = x + y

 (** This comment is not attached to any element since there is another
     special comment just before the next element. *)

 (** Comment for exception My_exception, even with a simple comment
     between the special comment and the exception.*)
 (* A simple comment. *)
 exception My_exception of (int -> int) * int

 (** Comment for type weather  *)
 type weather =
 | Rain of int (** The comment for constructor Rain *)
 | Sun (** The comment for constructor Sun *)

 (** The comment for type my_record *)
 type my_record = {
     foo : int ;    (** Comment for field foo *)
     bar : string ; (** Comment for field bar *)
   }

 (** The comment for module Foo *)
 module Foo =
   struct
     (** The comment for x *)
     let x = 0
     (** A special comment in the class, but not associated to any element. *)
   end

 (** The comment for module type my_module_type. *)
 module type my_module_type =
   sig
     (* Comment for value x. *)
     val x : int
     (* ... *)
   end
