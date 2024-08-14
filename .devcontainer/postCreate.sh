#!/bin/bash

# Install dev dependencies from OPAM
opam install . --deps-only -y

# For IDE support, install the OCaml language server
opam install ocaml-lsp-server -y

# Add OPAM environment setup to shell startup script
echo 'eval $(opam env)' >> ~/.bashrc

npm install --ignore-scripts
