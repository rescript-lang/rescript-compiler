(lang dune 3.3)

(using menhir 2.1)

(executables_implicit_empty_intf true)

(formatting
 (enabled_for ocaml))

(generate_opam_files true)

(name rescript)

(package
 (name rescript)
 (depends
  (ocaml
   (and
    (>= "4.14.0")
    (< "4.15.0")))
  ounit2
  reason
  cmdliner
  base64
  luv))
