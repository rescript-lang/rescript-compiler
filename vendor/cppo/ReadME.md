Note cppo is only used as a dev dependency, 
it is not needed for release build.

See LICENSE for more details

After running `dune build`
```
bspack -I _build/default/src -bs-main Cppo_main -o cppo_bin.ml
```