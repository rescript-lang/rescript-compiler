FROM alpine:3.20
LABEL org.opencontainers.image.authors="Christoph Knittel <ck@cca.io>"
LABEL org.opencontainers.image.description="Alpine-based Docker image for building statically linked ReScript binaries."

# - gcompat needed for ARM64, see https://github.com/actions/runner/issues/801#issuecomment-1374967227
# - python3 needed for ninja build
RUN apk add --no-cache bash gcc g++ git make opam python3 rsync gcompat

# We need to specify the OPAM dir explicitly as the GitHub Actions runner
# will set a different home directory when running in a container.
ENV OPAMROOT=/root/.opam

RUN opam init -y --compiler=5.2.0 --disable-sandboxing

RUN opam install -y dune cppo=1.6.9 ounit2=2.2.7
