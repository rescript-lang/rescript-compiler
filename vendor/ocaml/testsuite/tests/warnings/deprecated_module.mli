[@@@ocaml.deprecated {|
  As you could guess, Deprecated_module is deprecated.
  Please use something else!
|} ]

module M: sig
  val x: int
    [@@ocaml.deprecated]

  type t
    [@@ocaml.deprecated]
end
[@@ocaml.deprecated]
