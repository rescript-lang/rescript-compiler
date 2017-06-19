[BuckleScript](http://bucklescript.github.io/bucklescript/): A JavaScript backend for [OCaml](https://ocaml.org/) focused on smooth integration and clean generated code.

#Fork
This fork is an exploration of creating a build system to build to native/bytecode. The build system that is part of the bucklescript toolchain called [bsb](http://bucklescript.github.io/bucklescript/Manual.html#_bucklescript_build_system_code_bsb_code) only supports JS but is very fast. 

[This fork/branch](https://github.com/bsansouci/bucklescript/tree/simple-native-compilation) is capable of building to native with a simple `bsconfig.json`. Add `"bs-platform": "bsansouci/bucklescript#simple-native-compilation"` as a dependency in your package.json and add a `bsconfig.json` like you would for [bsb](http://bucklescript.github.io/bucklescript/Manual.html#_bucklescript_build_system_code_bsb_code) to work.

For [example](https://github.com/bsansouci/BetterErrors/tree/bsb-support):
```json
{
  "name" : "NameOfLibrary", // name of the product/library
  "sources" : "src",        // same as normal bsb
  "entries": [{
    "kind": "bytecode",     // supports "js", "bytecode" and "native"
    "main": "Index"         // main module
  }]
}
```
