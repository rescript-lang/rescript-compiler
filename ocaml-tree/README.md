
# License

The grammar.js file belongs to tree-sitter/tree-sitter-ocaml, it fails to compile with the
latest Node, so it's vendored here, its license goes to [./LICENSE](LICENSE)

Those C libraries are not needed since we made a snasphot of wasm.

# build wasm
npx tree-sitter build-wasm .
# build native
node-gyp config
node-gyp build