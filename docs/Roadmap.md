Alpha release

- [x] Support format module and all currying
- [x] Int32
- [x] Int64 support
- [x] Documentation about the Run-time encoding
- [x] Stabilize non-object part of FFI
- [x] Document non-object part of FFI
- [x] All files in run-time are ml files
- [x] Recursive module
- [x] Lazy values
- [x] Create a package for opam for the patched compiler, so user can use bucklescript with other ocaml tool chains like 
merlin
- [x] Document difference from ocaml native backend
- [x] Document difference from Js_of_ocaml
- [x] Support goog.module

Beta release:

- [ ] Object part of FFI (we have a working solution; the name mangling is not ideal)
- [ ] Compile local module as dictionary
- [ ] Custom formatter in debug mode
- [ ] Improving Runtime library (no unsafe code)
- [ ] Support es6 module (rollup format) (we current support commonjs and amdjs)

Release:
- [ ] Bindings to Dom and React
- [ ] Bigarray/Bignum support
- [ ] Native JS regex suport
- [ ] Improve curry performance
- [ ] Google closure compiler advanced mode
- [ ] Source map support
- [ ] Catch up with the new release of ocaml compiler
