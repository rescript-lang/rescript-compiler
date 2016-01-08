# Get Started

## Hello world

Currently, `ocamlscript` shares the same command line options as the `ocamlc`
bytecode compiler.

Create a file called `hello.ml` as below

```sh
echo 'print_endline "hello world"' > hello.ml
```

```sh
OCAML_RAW_JS=1 ocamlscript -c hello.ml
```

If everything goes well, you should have

```
console.log('hello world')
```

## Compilation model

However, the compilation/link model is like the OCaml native compiler
instead of bytecode compiler, suppose you have modules `A`, `B`, `C`,
module `C` depends on module `B`.

Then you need to compile `B` first before compiling `C`

```sh
OCAML_RAW_JS=1 ocamlscript -c a.ml b.ml
OCAML_RAW_JS=1 ocamlscript -c c.ml
```


