(** Test the html rendering of ocamldoc documentation tags *)

val heterological: unit
(** 
 @author yes
 @param no No description
 @param neither see no description  
 @deprecated since the start of time
 @return ()
 @see "Documentation_tags.mli" Self reference
 @since Now
 @before Time not implemented
*)

val noop: unit
(**
 @raise Not_found Never
 @raise Invalid_argument Never
*)
