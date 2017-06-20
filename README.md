# bsb-native

Bsb-native is like [bsb](http://bucklescript.github.io/bucklescript/Manual.html#_bucklescript_build_system_code_bsb_code), but compiles to native OCaml instead.

## Install

1) Add `"bsb-native": "bsansouci/bsb-native"` as a devDependency to your `package.json`
2) Add a `bsconfig.json` like you would for bsb. Bsb-native uses the same schema, located [here](http://bucklescript.github.io/bucklescript/docson/#build-schema.json)

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

## Use

`./node_modules/.bin/bsb -make-world -w` (or add an [npm script](https://docs.npmjs.com/misc/scripts))

The `-make-world` flag builds all of the dependencies.

The `-w` enabled the watch mode which will rebuild on any source file change.
