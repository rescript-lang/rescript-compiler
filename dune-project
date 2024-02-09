(lang dune 2.3)

(name rescript)

(generate_opam_files true)

(license LGPL-3.0-or-later)

(authors "Hongbo Zhang <bobzhang1988@gmail.com>")

(maintainers "Hongbo Zhang <bobzhang1988@gmail.com>" "Cristiano Calcagno")

(homepage "https://github.com/rescript-lang/rescript-compiler")

(bug_reports "https://github.com/rescript-lang/rescript-compiler/issues")

(package
 (name rescript)
 (synopsis "ReScript compiler")
 (depends
  (ocaml
   (>= 4.10))
  (ocamlformat
   (= 0.26.1))
  (cppo
   (= 1.6.9))
  (js_of_ocaml-compiler
   (= 5.6.0))
  (ounit2
   (= 2.2.7))
  (reanalyze
   (= 2.24.0))
  dune))
