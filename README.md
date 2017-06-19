[BuckleScript](http://bucklescript.github.io/bucklescript/): A JavaScript backend for [OCaml](https://ocaml.org/) focused on smooth integration and clean generated code.

# bsb-native

[This fork/branch](https://github.com/bsansouci/bucklescript/tree/simple-native-compilation) is an exploration of making [bsb](http://bucklescript.github.io/bucklescript/Manual.html#_bucklescript_build_system_code_bsb_code) build to native/bytecode. 

[It](https://github.com/bsansouci/bucklescript/tree/simple-native-compilation) is capable of building to native with a simple `bsconfig.json`. 

## How to

1) Add `"bs-platform": "bsansouci/bucklescript#simple-native-compilation"` as a dependency in your package.json
2) Add a `bsconfig.json` like you would for [bsb](http://bucklescript.github.io/bucklescript/Manual.html#_bucklescript_build_system_code_bsb_code)

For [example](https://github.com/bsansouci/BetterErrors/tree/bsb-support):
```json
{
  "name" : "NameOfLibrary",
  "sources" : "src",
  "entries": [{
    "kind": "bytecode",
    "main": "Index"
  }]
}
```

You can see the full schema merged at [head](http://bucklescript.github.io/bucklescript/docson/#build-schema.json).

## Already using BSB
If you're already using `bsb` for your purposes all you need to do is update the dependency in your `package.json`, run `npm i` again and add the `entries` field to your `bsconfig.json`.
