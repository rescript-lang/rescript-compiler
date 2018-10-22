

type a = ..
type b = ..

type a += A 
(* A is shadowed so it will available any more,
   it seems record disamibigution does not 
   work with extensible type
*)
#if OCAML_VERSION =~ "<4.03.0" then
type b += A 
#else
type b += AA (*name must be unique since 4.06 *)
#end
