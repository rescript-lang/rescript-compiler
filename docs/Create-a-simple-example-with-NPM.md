
Assume you have the required OCaml compiler installed, see [Installation](./Installation).

Create a temporary directory called `npm_test`

```sh
cd npm_test
echo "{}" > package.json
npm install --save bs-platform
echo 'let _ = Js.log "hello bucklescript!"' > hello.ml
./node_modules/.bin/bsc -I ./node_modules/bs-platform/ -c hello.ml
node hello.js
```
