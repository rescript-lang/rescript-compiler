(** The first special comment of the file is the comment associated
     with the whole module.*)


 (** Special comments can be placed between elements and are kept
     by the OCamldoc tool, but are not associated to any element.
     @-tags in these comments are ignored.*)

 (*******************************************************************)
 (** Comments like the one above, with more than two asterisks,
     are ignored. *)

 (** The comment for function f. *)
 val f : int -> int -> int
 (** The continuation of the comment for function f. *)

 (** Comment for exception My_exception, even with a simple comment
     between the special comment and the exception.*)
 (* Hello, I'm a simple comment :-) *)
 exception My_exception of (int -> int) * int

 (** Comment for type weather  *)
 type weather =
 | Rain of int (** The comment for constructor Rain *)
 | Sun (** The comment for constructor Sun *)

 (** Comment for type weather2  *)
 type weather2 =
 | Rain of int (** The comment for constructor Rain *)
 | Sun (** The comment for constructor Sun *)
 (** I can continue the comment for type weather2 here
   because there is already a comment associated to the last constructor.*)

 (** The comment for type my_record *)
 type my_record = {
     foo : int ;    (** Comment for field foo *)
     bar : string ; (** Comment for field bar *)
   }
   (** Continuation of comment for type my_record *)

 (** Comment for foo *)
 val foo : string
 (** This comment is associated to foo and not to bar. *)
 val bar : string
 (** This comment is associated to bar. *)

 (** The comment for module Foo *)
 module Foo :
   sig
     (** The comment for x *)
     val x : int

     (** A special comment that is kept but not associated to any element *)
   end

 (** The comment for module type my_module_type. *)
 module type my_module_type =
   sig
     (** The comment for value x. *)
     val x : int

     (** The comment for module M. *)
     module M :
       sig
         (** The comment for value y. *)
         val y : int

         (* ... *)
       end
   end
