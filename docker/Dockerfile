FROM rust:1.80.1-bullseye
LABEL org.opencontainers.image.authors="Christoph Knittel <ck@cca.io>"
LABEL org.opencontainers.image.description="Docker image for ReScript development."

RUN apt update && apt install -y --no-install-recommends sudo ca-certificates curl git rsync opam musl-tools python3 python-is-python3

# Node.js
RUN curl -sL https://deb.nodesource.com/setup_20.x | bash -
RUN apt install -y nodejs

# Create a non-root user and switch to it
ARG USERNAME=devuser
ARG USER_UID=1000
ARG USER_GID=$USER_UID

RUN groupadd --gid $USER_GID $USERNAME \
    && useradd --uid $USER_UID --gid $USER_GID -m $USERNAME \
    && usermod -aG sudo $USERNAME \
    && mkdir /workspaces \
    && chown -R $USERNAME:$USERNAME /workspaces

# Allow the devuser to use sudo without a password
RUN echo "$USERNAME ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

USER $USERNAME

# OCaml
RUN opam init -y --bare --disable-sandboxing git+https://github.com/rescript-lang/opam-repository
RUN opam switch create 5.2.0 --packages ocaml-option-static
RUN opam install -y dune cppo=1.6.9 js_of_ocaml-compiler=5.8.1 ocamlformat=0.26.2 ounit2=2.2.7 reanalyze=2.25.1