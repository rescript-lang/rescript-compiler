Hello! This is the subdirectory for the new, newcomer-friendly OCaml/Reason warning & error report system. Most of the logic are lifted from the compiler (https://github.com/BuckleScript/ocaml/tree/master). The convention here is to have a `super_foo` for each corresponding compiler's file `foo`. So, for example, `warnings.ml` becomes `super_warnings.ml`. The exception is `super_main`, the entry point, and `super_reason_react`, our special handling of [ReasonReact](https://reasonml.github.io/reason-react/) errors.

Feel free to submit new ones or tweak existing messages in these files! They also have more precise comments in them that tells you how they work.

### Develop

Here's the fastest way to iterate & test these error messages. The setup is currently a bit contrived; if something's not working, please file an issue or ping us in [Discord](discord.gg/reasonml)!

#### Build

Assuming you're at the root of this repo:

```
# Use the correct opam switch for working on BuckleScript
opam update
opam switch 4.02.3+buckle-master
opam switch reinstall 4.02.3+buckle-master # do this if you get errors even from a clean compilation
opam install camlp4
eval `opam config env`

# build BuckleScript's forked OCaml
cd vendor/ocaml
./configure -prefix `pwd`
make world.opt
make install

# Build BuckleScript itself
cd ../../
make
make install

# install this local bs globally
npm -g install .
```

#### Testing on a dummy project

Now, for testing super_errors on a dummy project. Go somewhere else and do this:

```
bsb -init foo -theme basic-reason
cd foo
npm run build
```

And whenever you modify a file in super_errors, run this inside `jscomp/`:

```
make ../lib/bsc.exe && ./install-bsc.sh
```

This will substitute the global `bsc.exe` you just installed with the newly built one. Then run `npm build again` in the dummy project and see the changes! The iteration cycle for testing these should be around 2 seconds =).

##### Troubleshooting

Did any of the above step not work?

- Make sure you did "eval `opam config env`" In your CLI/bashrc/zshrc

- **If the vendored ocaml changed between when you last iterated on the repo and now**, you probably skipped the `opam switch reinstall 4.02.3+buckle-master` part. You'll have to do `git clean -xdf` and then restart with the build instructions. Careful, as `git clean` removes your uncommitted changes.

- **If these fail too**, make sure you do have the correct `ocamlopt` in your environment: `which ocamlcopt` should show an `opam` path, not `reason-cli` path. If you see the latter, this means it overrode the global `ocamlopt` BuckleScript needed. In this case, either temporarily uninstall reason-cli or make sure your opam PATH overrides the reason-cli PATH (and not the other way around) in your bashrc/zshrc.

#### General Tests

Note: currently you can't test things with external libraries (e.g. ReasonReact).

The fixture tests are located in `jscomp/build_tests/super_errors/` and look like:
```
{some code}
/*
{the normal ocaml error output}

=====

{the supererrors output}
*/

{some more code}
/*
etc
*/
```

Files in `formattingTests` get printed with `-colors always` so we can test formatting. The other ones are printed with `-colors never` so that it's readable.

To add a new test case, add your code to the end of a file, and run `jscomp/build_tests/super_errors/rebuild.sh`. The output will be appended.

To recompile `bsc`, you can, from the `jscomp` directory, run `make bin/bsc.exe`. If this doesn't work, you likely have a problem with the ocaml installation -- go back to `Build` and make sure you followed everything to the letter.

##### Special Iteration Workflow

This section is reserved for when you're making a change to the vendored ocaml compiler itself, in vendor/ocaml, and then testing on super-errors changes at the same time. If you're doing this for whatever reason, then the previous quick iteration workflow wouldn't work. Here's what you have to do after each change:

```
# at project root
cd jscomp
make force-snapshotml # make sure your changes are reflected in jscomp/bin/whole_compiler.ml
make -C bin bsc.exe && ./install-bsc.sh
```
