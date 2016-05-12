
BuckleScript extends OCaml compiler options with several flags to
provide better experience for NPM users.

## To deploy your OCaml library as a npm package

In general, you are expected to deploy two kinds of artifacts, the
generated JS files and meta data which your OCaml dependents relies
on.

Since CommonJS has no namespaces, to allow JS files live in different
directories, we have a flag

```sh
bsc -js-npm-output-path $npm_package_name:path/to/your/js/dir -c a.ml
```

By passing this flag, `bsc` will store your `package_name` and
relative path to `package.json` in `.cmj` files, it will also generate
JS files in the directory you specified, you can and are encouraged
to store js files in a hierarchical directory.

For the binary artifacts, (Note that this is not necessary if you only
want your libraries to be consumed by JS developers, and it has
benefit since end users don't need these binary data any more), the
convention
is to store all `*.cm` data in a *single* directory
`package.json/lib/ocaml`
and js files in a *hierachical* directory
`package.json/lib/js`



## To use OCaml library as a npm package

If you follow the layout convention above, use ocaml package is pretty
straightforward

```
bsc -js-npm-package-include ocaml-library -c a.ml
```



## Conclusion

Your command line would be like this

```
bsc -js-npm-package-include ocaml-library1 -js-npm-package-include
ocaml-library2 -js-npm-output-path $npm_package_name:lib/js/ -c a.ml
```
