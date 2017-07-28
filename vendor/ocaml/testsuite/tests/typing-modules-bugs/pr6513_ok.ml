module type PR6513 = sig
module type S = sig type u end

module type T = sig
  type 'a wrap
  type uri
end

module Make: functor (Html5 : T with type 'a wrap = 'a) ->
  S with type u = < foo : Html5.uri >
end

(* Requires -package tyxml
module type PR6513_orig = sig
module type S =
sig
        type t
        type u
end

module Make: functor (Html5: Html5_sigs.T with type 'a Xml.wrap = 'a and type 'a wrap = 'a and type 'a list_wrap = 'a list) -> S with
        type t = Html5_types.div Html5.elt and
        type u = < foo: Html5.uri >
end
*)
