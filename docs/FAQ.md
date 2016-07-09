
* The compiler does not build

  In production mode, the compiler is a single file in `jscomp/bin/compiler.ml`. If it is not compiling, make sure you have the right OCaml compiler version. Currently the OCaml compiler is a submodule of bucklescript. Make sure the exact commit hash matches (we only update the compiler occasionally).
