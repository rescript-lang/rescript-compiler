FROM mcr.microsoft.com/devcontainers/rust:1-1-bookworm
LABEL org.opencontainers.image.authors="Christoph Knittel <ck@cca.io>"
LABEL org.opencontainers.image.description="Docker image for ReScript development."

RUN apt update && apt install -y --no-install-recommends opam musl-tools python-is-python3

# OCaml
# RUN opam init -y --bare --disable-sandboxing git+https://github.com/rescript-lang/opam-repository
# RUN opam switch create 5.2.0 --packages ocaml-option-static
# RUN opam install -y dune cppo=1.6.9 js_of_ocaml-compiler=5.8.1 ocamlformat=0.26.2 ounit2=2.2.7 reanalyze=2.25.1