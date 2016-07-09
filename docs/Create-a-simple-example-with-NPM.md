

Create a temporary directory called `npm_test`

```sh
mkdir npm_test
cd npm_test
echo "{}" > package.json
```

First, install BuckleScript.
(For more advanced settings, please read
[Installation](./Installation.md).)



```sh
npm install bs-platform --save
```

Second, create a file called `hello.ml`,

```js
echo 'let _ = Js.log "hello bucklescript!"' > hello.ml
```

Third, build and run

```
`npm bin`/bsc -c hello.ml
node hello.js
```
