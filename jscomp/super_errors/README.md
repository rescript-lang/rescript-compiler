Hello! This is the subdirectory for the new, newcomer-friendly OCaml/Reason warning & error report system. Most of the logic are lifted from the compiler (https://github.com/ocaml/ocaml/tree/4.02). The convention here is to have a `super_foo` for each corresponding compiler's file `foo`. So, for example, `warnings.ml` becomes `super_warnings.ml`. The exception is `super_main`, the entry point.

Feel free to submit new ones or tweak existing messages in these files! They also have more precise comments in them that tells you how they work.

### Develop

Here's the fastest way to iterate & test these error messages. The setup is currently a bit contrived; if something's not working, please file an issue or ping us in [Discord](discord.gg/reasonml)!

Assuming you're at the root of this repo:

```
# Use the correct opam switch for working on BuckleScript
opam update
opam switch 4.02.3+buckle-master
eval `opam config env`
# build BuckleScript's forked OCaml
cd vendor/ocaml
./configure -prefix `pwd`
make world.opt
make install
# Build BuckleScript itself
cd ../../jscomp
make world
# copy over the standard library artifacts. BuckleScript precompiles these and when using bsb, it'll go straight fetch the artifacts without recompiling the stdlib per project
cd ..
make lib/ocaml
```

(This is basically the instructions [here](https://bucklescript.github.io/bucklescript/Manual.html#_minimal_dependencies)

And then set up a dummy project somewhere else, like this:

```
bsb -init foo -theme basic-reason
cd foo
npm run build
```

Then hack into lib/bs/build.ninja (the underlying ninja build file bsb generates and uses) and change the 3 following lines:

```
bsc = ...
bsdep = ...
bsc_flags = ...
```

to:

```
bsc = /path/to/your/cloned/bucklescript/jscomp/bin/bsc.exe
bsdep = /path/to/your/cloned/bucklescript/jscomp/bin/bsb_helper.exe
bsc_flags = -nostdlib -I '/path/to/your/cloned/bucklescript/lib/ocaml' -bs-super-errors -no-alias-deps -color always -w -40+6+7+27+32..39+44+45
```

Now, whenever you make a modification to super_errors, do the following in BuckleScript's `jscomp/` folder:

```
make bin/whole_compiler.ml && make -C bin bsc.exe
```

Then switch to the root of the dummy project and do:

```
./node_modules/bs-platform/bin/ninja.exe -C lib/bs
```

And you should see your changes!
