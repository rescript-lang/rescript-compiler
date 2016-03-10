# Get Started

## Hello world

Currently, `BuckleScript` shares the same command line options as `ocamlc`
bytecode compiler.

Create a file called `hello.ml` as below

```sh
echo 'print_endline "hello world"' > hello.ml
```

```sh
bsc -c hello.ml
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
bsc -c a.ml b.ml
bsc -c c.ml
```


