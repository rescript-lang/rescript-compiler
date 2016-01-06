# Get Started

## Hello world

Currently, `ocamlscript` shares the same command line options as `ocamlc`
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

## Compilaton model

However, the compilation/link model is like ocaml native compiler
instead of bytecode compiler, suppose you
have module `A`, `B`, `C`, module `C` depends on module `B`,

Then you need compile `B` first before compiling `C`

```sh
OCAML_RAW_JS=1 ocamlscript -c a.ml b.ml
OCAML_RAW_JS=1 ocamlscript -c c.ml
```


