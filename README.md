# bsb-native

_This is an experimental project. It has no intention of replacing bsb but merely augmenting it._

A fork of [bsb](http://bucklescript.github.io/bucklescript/Manual.html#_bucklescript_build_system_code_bsb_code) to allow it to build applications to js (bsc), native (ocamlopt) or bytecode (ocamlc). 

## How to install

1) Add `"bs-platform": "bsansouci/bucklescript"` as a dependency in your `package.json`
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

You can see the full bsconfig schema at [head](http://bucklescript.github.io/bucklescript/docson/#build-schema.json).

## How to run
All binaries installed through npm are installed in the same folder `node_modules/.bin` so to run from the command-line you can run `./node_modules/.bin/bsb -make-world -w` (or add an [npm script](https://docs.npmjs.com/misc/scripts)).

The `-make-world` flag builds all of the dependencies.

The `-w` enabled the watch mode which will rebuild on any source file change.

## Already using JS bsb
If you're already using `bsb` in your project all you need to do is update the dependency in your `package.json` to `"bs-platform": "bsansouci/bucklescript"` , run `npm i` again and add the `entries` field to your `bsconfig.json`.
