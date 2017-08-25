Hello! This is the subdirectory for the new, newcomer-friendly OCaml/Reason warning & error report system. Most of the logic are lifted from the compiler (https://github.com/BuckleScript/ocaml/tree/master). The convention here is to have a `super_foo` for each corresponding compiler's file `foo`. So, for example, `warnings.ml` becomes `super_warnings.ml`. The exception is `super_main`, the entry point, and `super_reason_react`, our special handling of [ReasonReact](https://reasonml.github.io/reason-react/) errors.

Feel free to submit new ones or tweak existing messages in these files! They also have more precise comments in them that tells you how they work.

### Develop

Here's the fastest way to iterate & test these error messages. The setup is currently a bit contrived; if something's not working, please file an issue or ping us in [Discord](discord.gg/reasonml)!

Assuming you're at the root of this repo:

```
# Use the correct opam switch for working on BuckleScript
opam update
opam switch 4.02.3+buckle-master
opam switch reinstall 4.02.3+buckle-master # do this if you get errors related to `ast_utf8_string.ml`
opam install camlp4
eval `opam config env`

# build BuckleScript's forked OCaml
cd vendor/ocaml
./configure -prefix `pwd`
make world.opt
make install

# Build BuckleScript itself
cd ../../jscomp
make world

# install this local bs globally
npm -g install .
```

Now, for testing super_errors on a dummy project. Go somewhere else and do this:

```
bsb -init foo -theme basic-reason
cd foo
npm run build
```

And whenever you modify a file in super_errors, run this inside `jscomp/`:

```
make bin/bsc.exe && ./install-bsc.sh
```

This will substitute the global `bsc.exe` you just installed with the newly built one. Then run `npm build again` in the dummy project and see the changes! The iteration cycle for testing these should be around 2 seconds =).
