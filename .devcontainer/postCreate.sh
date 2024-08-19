#!/bin/sh

# Opam stuff post install
opam init -y --bare --disable-sandboxing git+https://github.com/rescript-lang/opam-repository
opam switch create 5.2.0 --packages ocaml-option-static
opam install -y dune cppo=1.6.9 js_of_ocaml-compiler=5.8.1 ocamlformat=0.26.2 ounit2=2.2.7 reanalyze=2.25.1

# Install dev dependencies from OPAM
opam install . --deps-only -y

# For IDE support, install the OCaml language server
opam install ocaml-lsp-server -y

# Add OPAM environment setup to shell startup script
echo 'eval $(opam env)' >> ~/.zshrc

npm install --ignore-scripts
