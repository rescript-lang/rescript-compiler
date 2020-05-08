## Playground

This folder purpose is to enable the creation of JavaScript bundles of BuckleScript compiler that include cmis and cmjs
from existing third party libraries (like `reason-react` or `bs-webapi`).

### Building the first time

```sh
# from project root folder
git submodule update --init && node scripts/buildocaml.js
./scripts/ninja.js config && ./scripts/ninja.js build
node scripts/install.js

cd playground

yarn # installs all bucklescript packages that we'll get cmij files for
yarn build # get some fresh cmi and cmijs

# cmij.exe will include the libraries cmi and cmjs files to builtin_cmi_datasets and builtin_cmj_datasets
# pass any folders we want to include, separated by commas
../jscomp/bin/cmij.exe -playground node_modules/reason-react/lib/ocaml,node_modules/bs-webapi/lib/ocaml

../scripts/ninja.js build -playground # generates js_refmt_compiler.ml with everything in it

esy build-playground # calls ocamlc.opt to convert js_refmt_compiler.ml to bytecode, then js_of_ocaml to convert bytecode to exports.js
```

### Creating a bundle with some custom node module

```sh
cd playground
yarn add some-package
yarn build
../jscomp/bin/cmij.exe node_modules/my-project/lib/ocaml
../scripts/ninja.js build -playground
esy build-playground
```
